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

let prefix ?limit x = { Query.Options.default with prefix = Some x; limit }
let count t = Query.fold (fun _ acc -> Lwt.return (acc + 1)) t 0

let rec add_random_users store prefix n =
  let user = User.random () in
  let info = Store.Info.none in
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
  Alcotest.(check int "Key count" 40 count)

let test_prefix store _ () =
  let q = Query.v (fun _k v -> Lwt.return_some v) in
  let* results = Query.exec ~options:(prefix [ "user" ]) q store in
  let+ count = count results in
  Alcotest.(check int "Prefix count" 20 count)

let test_query store _ () =
  let q =
    Query.v (fun _k v ->
        if v.User.age > 100 then Lwt.return_some v else Lwt.return_none)
  in
  let* results = Query.exec ~options:(prefix [ "user" ]) q store in
  let+ count = count results in
  Alcotest.(check int "Query count" 0 count)

let test_limit store _ () =
  let q = Query.v (fun k _v -> Lwt.return_some k) in
  let* results = Query.exec ~options:(prefix ~limit:2 [ "user" ]) q store in
  let+ count = count results in
  Alcotest.(check int "Limit count" 2 count)

let main =
  let* store = init () in
  let* () = add_random_users store "user" 20 in
  let* () = add_random_users store "test" 20 in
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
