open Lwt.Syntax
module Store = Irmin_mem.KV (Irmin.Contents.String)

module Value = struct
  type t = Store.Contents.t
end

module Query = Irmin_query.Make (Store) (Value)

let main =
  let config = Irmin_mem.config () in
  let* repo = Store.Repo.v config in
  let* store = Store.master repo in
  let info s = Irmin_unix.info "%s" s in
  let* () =
    Store.set_exn store ~info:(info "set a/b/c") [ "a"; "b"; "c" ] "123"
  in
  let filter =
    Query.Filter.v @@ fun _k v ->
    let+ v = v () in
    String.length v >= 3
  in
  let iter =
    Query.Iter.v ~pure:false @@ fun k v ->
    Lwt.return @@ Printf.sprintf "%s => %s" (Repr.to_string Store.Key.t k) v
  in
  let settings = { Query.Settings.default with prefix = Some [ "a" ] } in
  let* s = Query.filter ~filter iter ~settings store in
  Query.Results.iter Lwt_io.printl s

let () = Lwt_main.run main
