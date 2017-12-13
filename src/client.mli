open Model
       
val lookup: string -> string -> string -> (Service.t option) Lwt.t
val list_services: string -> string -> ServerSet.t Lwt.t
val leave: string -> string -> string -> unit Lwt.t
val register: string -> string -> Service.t -> unit Lwt.t
val create_server_set: string -> string -> unit Lwt.t
val remove_server_set: string -> string -> unit Lwt.t

