open Model
module DataStore = DB.DataStore
                     
(* 
  api spec 
  lookup -> GET /catalog/:ssid/:id
  register -> POST /catalog/:ssid 
  leave -> DELETE /catalog/:ssid/:sid
  create_server_set -> PUT /catalog/:ssid 
  delete_server_set -> DELETE /catalog/:ssid 
  list_services -> GET /catalog/:ssid 

 *)
                     
val lookup: DataStore.t -> string -> string -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

val register: DataStore.t -> string -> service -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
                                                 
val leave: DataStore.t -> string -> string -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
                                                
val create_server_set: DataStore.t -> string -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

val remove_server_set: DataStore.t -> string -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
                                                 
val list_services: DataStore.t -> string -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
