open Model
       
module DataStore: Irmin.S with type contents = ServerSet.t
(*store ops*)       
val list_members: DataStore.t -> string -> ServerSet.t Lwt.t
val rm_server_set: DataStore.t -> string -> string Lwt.t
val mk_server_set: DataStore.t -> string -> string Lwt.t
                                                  
val lookup: DataStore.t -> string -> string -> (service option) Lwt.t
val add_service: DataStore.t -> string -> service -> string Lwt.t 
val update_service: DataStore.t -> string -> service -> string Lwt.t
                                                               
val rm_service: DataStore.t -> string -> string -> string Lwt.t
val rm_stale: DataStore.t -> string -> float -> ServerSet.t Lwt.t
val refresh: DataStore.t -> string -> string -> string Lwt.t 
