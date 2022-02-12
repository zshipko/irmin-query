open Lwt.Syntax

module User = struct
  type t = { name : string; age : int } [@@deriving irmin]

  let merge = Irmin.Merge.(option (idempotent t))
  let chars = "abcdefghijklmopqrstuvwxyz"

  let random () =
    let len = 5 + Random.int 5 in
    let age = 5 + Random.int 60 in
    let name =
      String.init len (fun _ -> chars.[Random.int (String.length chars)])
    in
    { name; age }
end

module Store = Irmin_mem.KV.Make (User)
module Query = Irmin_query.Make (Store)
module Info = Irmin_unix.Info (Store.Info)

let prefix ?limit x = { Query.Settings.default with prefix = Some x; limit }
let count t = Lwt_seq.fold_left_s (fun acc _ -> Lwt.return (acc + 1)) 0 t

let rec add_random_users store prefix n =
  let user = User.random () in
  let info = Info.v "Add user: %s" user.name in
  let* () = Store.set_exn store ~info [ prefix; user.name ] user in
  if n - 1 > 0 then add_random_users store prefix (n - 1) else Lwt.return_unit

let init () =
  let config = Irmin_mem.config () in
  let* repo = Store.Repo.v config in
  let+ store = Store.main repo in
  store

let get_users store prefix =
  let+ items = Store.list store [ prefix ] in
  List.filter_map
    (fun (step, x) ->
      match Store.Tree.inspect x with
      | `Contents -> Some [ prefix; step ]
      | _ -> None)
    items

let test_key_count store _ () =
  let* keys = Query.paths store in
  let+ count = count keys in
  Alcotest.(check int "Key count" 10 count)

let test_prefix store _ () =
  let filter = Query.Filter.v (fun _k _v -> Lwt.return true) in
  let iter = Query.Iter.v (fun _k v -> Lwt.return v) in
  let* results =
    Query.filter ~settings:(prefix [ "user" ]) ~filter iter store
  in
  let+ count = count results in
  Alcotest.(check int "Prefix count" 10 count)

let test_query store _ () =
  let filter = Query.Filter.v (fun _k v -> Lwt.return (v.User.age > 100)) in
  let iter = Query.Iter.v (fun k _v -> Lwt.return k) in
  let* results =
    Query.filter ~settings:(prefix [ "user" ]) ~filter iter store
  in
  let+ count = count results in
  Alcotest.(check int "Query count" 0 count)

let test_limit store _ () =
  let iter = Query.Iter.v (fun k _v -> Lwt.return k) in
  let* results = Query.iter ~settings:(prefix ~limit:2 [ "user" ]) iter store in
  let+ count = count results in
  Alcotest.(check int "Limit count" 2 count)

let main =
  let* store = init () in
  let* () = add_random_users store "user" 10 in
  Alcotest_lwt.run "query"
    [
      ( "user",
        [
          Alcotest_lwt.test_case "key count" `Quick (test_key_count store);
          Alcotest_lwt.test_case "prefix" `Quick (test_prefix store);
          Alcotest_lwt.test_case "query" `Quick (test_query store);
          Alcotest_lwt.test_case "limit" `Quick (test_limit store);
        ] );
    ]

let () = Lwt_main.run main
