open Model

type host_port = {host: string; port: int option}
                         
val lookup: string -> ?port:int -> string -> string -> (Service.t option) Lwt.t
val list_services: string -> ?port: int -> string -> (ServerSet.t option) Lwt.t
val leave: string -> ?port:int -> string -> string -> unit Lwt.t
val register: string -> ?port:int -> string -> ?interval:float -> Service.t  -> unit Lwt.t
val create_server_set: string -> ?port:int -> string -> unit Lwt.t
val remove_server_set: string -> ?port:int -> string -> unit Lwt.t

