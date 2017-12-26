open Model
       
type watch_session = {mutable ss: server_set; mu: Lwt_mutex.t}                  
type host_port = {host: string; port: int option}

val make_session: watch_session
val lookup: string -> ?port:int -> string -> string -> (Service.t option) Lwt.t
val list_services: string -> ?port: int -> string -> (ServerSet.t option) Lwt.t
val leave: string -> ?port:int -> string -> string -> unit Lwt.t
val register: string -> ?port:int -> string -> ?interval:float -> Service.t  -> unit Lwt.t
                                                                                     
val create_server_set: string -> ?port:int -> string -> unit Lwt.t
val remove_server_set: string -> ?port:int -> string -> unit Lwt.t
val watch: string -> ?port:int -> string -> watch_session -> unit Lwt.t 
