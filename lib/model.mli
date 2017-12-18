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
  val lookup: t -> string -> service option 
  val rm_service: t -> string -> t
  val add_service: t-> service -> t
  val rm_stale: t -> float -> t
  val update_service: t -> service -> t
  val refresh: t -> string -> service option                             
  val zero: t              
end 





                         
                                                      
