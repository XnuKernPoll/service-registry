open Model

type host_port = {host: string; port: int option}
                         
val lookup: host_port -> string -> string -> (Service.t option) Lwt.t
val list_services: host_port -> string -> (ServerSet.t option) Lwt.t
val leave: host_port -> string -> string -> unit Lwt.t
val register: host_port -> string -> Service.t -> unit Lwt.t
val create_server_set: host_port -> string -> unit Lwt.t
val remove_server_set: host_port -> string -> unit Lwt.t

