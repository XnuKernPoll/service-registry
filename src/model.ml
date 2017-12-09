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
    
end

module DataStore = Irmin_unix.Git.FS.KV(ServerSet)

(*entertain the idea of having (node, ip, port) as author*)
let info fmt =
  Irmin_unix.info fmt
 
let write_event ssid =
  info "a write event occurred on server_set %s" ssid

let rm_event ssid =
  info "a delete event occurred on %s" ssid

let non_existant ssid =
  Fmt.strf "server set %s doesn't exist" ssid 

let cat_path id =
  ["catalog"; id]
  
let lookup t ssid id =
  DataStore.find t (cat_path ssid) >>= fun res ->
  let svc = BatOption.bind res (fun ss -> ServerSet.lookup ss id) in
  Lwt.return svc


let update_service t ssid svc =
  let path = cat_path ssid in
  DataStore.find t path >>= fun ssopt ->
  match ssopt with
  | Some ss ->
     DataStore.set t ~info:(write_event ssid) path (ServerSet.update_service ss svc)
     >|= fun () ->
     Fmt.strf "%s was updated in serverset %s" svc.id ssid 

  | None -> Lwt.return ( non_existant ssid )
     
let rm_service t ssid id =
  let path = cat_path ssid in
  DataStore.find t path >>= fun ssopt ->
  match ssopt with
  | Some ss ->
     DataStore.set t ~info:(rm_event ssid) path (ServerSet.rm_service ss id)
     >|= fun () -> Fmt.strf "%s was removed from serverset %s" id ssid

  | None ->
     Lwt.return (non_existant ssid)
     
let rm_stale t ssid max =
  let path = cat_path ssid in 
  DataStore.find t path >>= fun ssopt ->
  match ssopt with
  | Some ss ->
     let fresh = ServerSet.rm_stale ss max in 
     DataStore.set t ~info:(rm_event ssid) path fresh >|= fun () ->
     fresh

  | None -> Lwt.return []
              
       
       
  
