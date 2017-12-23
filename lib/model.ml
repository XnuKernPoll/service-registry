open Lwt.Infix
open Batteries
       

 
type service = {address: string; port: int32; ts: float; id: string}
type server_set = service list

let service_t =
  let open Irmin.Type in
  record "service" (fun address port ts id -> {address; port; ts; id;})
  |+ field "address" string (fun t -> t.address)
  |+ field "port" int32 (fun t -> t.port)
  |+ field "ts" float (fun t -> t.ts)
  |+ field "id" string (fun t -> t.id)
  |> sealr
                                   
  
let server_set_t =
  let open Irmin.Type in
  list service_t

module Service = struct
  type t = service
             
  let t = service_t

  let make host port =
    let uuid = Uuidm.to_string (Uuidm.v `V4) in 
    {address = host; port = Int32.of_int port; ts = Unix.time (); id = uuid}

  let make host port uuid =
    {address = host; port = Int32.of_int port; ts = Unix.time (); id = uuid}
            
  let compare_ts l r =
    compare l.ts r.ts

  let compare_id l r =
    compare l.id r.id
  
end 

                   
module ServerSet = struct
  type t = server_set
  let t = server_set_t
            
  let pp = Irmin.Type.pp_json server_set_t
  let of_string s = Irmin.Type.decode_json server_set_t (Jsonm.decoder (`String s))

  let merge = Irmin.Merge.default (server_set_t) |> Irmin.Merge.option
                                            
  let filter_id ss id =
    List.filter (fun x -> x.id == id) ss
                
  let lookup ss id =
    let results = filter_id ss id in
    if (List.length results) >= 1 then
      let sorted = List.sort (Service.compare_ts) results |> List.rev in
      Some (List.hd sorted)
    else
      None

  let rm_service ss id =
    List.filter (fun x -> x.id != id) ss

  let add_service ss svc =
    ss @ [svc]

  let is_fresh svc ct max_span =
    let diff = ct -. svc.ts in
    diff <= max_span 

  let rm_stale ss max_span =
    let ct = Unix.time () in
    List.filter (fun x -> is_fresh x ct max_span) ss

  let update_service ss svc =
    let rmss = rm_service ss svc.id in
    rmss @ [svc]

  let zero = []

  let refresh t ssid =
    let svco = lookup t ssid in
    BatOption.map (fun svc -> {svc with ts = Unix.time () }) svco 
              
               
    
end

