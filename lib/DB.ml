open Batteries
open Lwt.Infix
open Model
       
       
module DataStore = Irmin_unix.Git.FS.KV(ServerSet)

(*entertain the idea of having (node, ip, port) as author*)
let info fmt =
  Irmin_unix.info fmt
                  
let ss_removal_event ssid = Fmt.strf "server set %s removed" ssid

let ss_creation_event ssid = Fmt.strf "server set %s created" ssid
 
let write_event ssid =
  info "a write event occurred on server_set %s" ssid

let rm_event ssid =
  info "a delete event occurred on %s" ssid

let non_existant ssid =
  Fmt.strf "server set %s doesn't exist" ssid 

let cat_path id =
  ["catalog"; id]

let mk_server_set t ssid =
  let path = cat_path ssid in
  let ev = ss_creation_event ssid in
  DataStore.set t ~info:( info "%s" ev ) path (ServerSet.zero) >|= fun () ->
  ev

let rm_server_set t ssid =
  let path = cat_path ssid in
  let ev = ss_removal_event ssid in
  DataStore.remove t ~info:(info "%s" ev) path >|= fun () ->
  ev

let list_members t ssid =
  let path = cat_path ssid in
  DataStore.find t path >|= fun res ->
  match res with
  | Some x -> x
  | None -> ServerSet.zero
  
  
    
    
let lookup t ssid id =
  DataStore.find t (cat_path ssid) >>= fun res ->
  let svc = BatOption.bind res (fun ss -> ServerSet.lookup ss id) in
  Lwt.return svc

let add_service t ssid svc =
  let path = cat_path ssid in
  DataStore.find t path >>= fun ssopt ->
  match ssopt with
  | Some ss ->
     DataStore.set t ~info:(write_event ssid) path (ServerSet.add_service ss svc)
     >|= fun () -> Fmt.strf "%s was added to server set %s" svc.id ssid

  | None ->
     Lwt.return (non_existant ssid)
    


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

  | None -> Lwt.return (ServerSet.zero)
              
       

let refresh t ssid id =
  let path = cat_path ssid in
  DataStore.find t path >>= fun ssopt ->
  let refresh = BatOption.bind ssopt (fun ss -> ServerSet.refresh ss id) in
  match (ssopt, refresh) with
  | (None, _) -> Lwt.fail_with ( Fmt.strf "no such server set as %s" ssid)
  | (Some x, None) -> Lwt.fail_with (Fmt.strf "no such sid as %s" id)
  | (Some ss , Some svc) ->
     let nss = ServerSet.update_service ss svc in
     DataStore.set t ~info:(write_event ssid) path nss >>= fun () ->
     let rep = Fmt.strf "service %s was refreshed" id in
     Lwt.return rep 
     
