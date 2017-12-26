open DB
open Model

module Server = Cohttp_lwt_unix.Server

type rep = (Cohttp.Response.t * Cohttp_lwt_body.t)             
type t = (rep Lwt.t * rep Lwt.u) list
type watches = {tbl: (string, t) Hashtbl.t; mu: Lwt_mutex.t}

val register_watch: DataStore.t -> string -> watches -> Service_registry.DB.DataStore.watch Lwt.t
val add_watcher: watches -> string -> rep Lwt.t
