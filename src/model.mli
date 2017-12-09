type service = {address: string; port: int32; ts: float; id: string}
type server_set = service list

val service_t: service Irmin.Type.t
val server_set_t: service list Irmin.Type.t

module Service : sig
  type t = service
  val t: service Irmin.Type.t
  val compare_ts: service -> service -> int
  val compare_id: service -> service -> int                              
end 

(*val cat_path: string -> string list *)

(*server_set DataType Ops*)       
module ServerSet: sig
  include Irmin.Contents.S
  val lookup: server_set -> string -> service option 
  val rm_service: server_set -> string -> server_set
  val add_service: server_set-> service -> server_set
  val rm_stale: server_set -> float -> server_set
  val update_service: server_set -> service -> server_set
end 

module DataStore: Irmin.S with type contents = ServerSet.t 



                         
                                                                              
(*store ops*)       
val lookup: DataStore.t -> string -> string -> (service option) Lwt.t 
val update_service: DataStore.t -> string -> service -> string Lwt.t
val rm_service: DataStore.t -> string -> string -> string Lwt.t
val rm_stale: DataStore.t -> string -> float -> server_set Lwt.t
