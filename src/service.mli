open Model
module DataStore = DB.DataStore
                     

val lookup: DataStore.t -> string -> string -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

val register: DataStore.t -> string -> service -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
                                                 
val leave: DataStore.t -> string -> string -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
                                                
val create_server_set: DataStore.t -> string -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

val remove_server_set: DataStore.t -> string -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
                                                 
val list_services: DataStore.t -> string -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
