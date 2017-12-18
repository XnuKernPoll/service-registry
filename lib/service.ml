open Cohttp_lwt
open Cohttp_lwt_unix
open Lwt.Infix
open Model

module DataStore = DB.DataStore
                     
let res_opt r =
  match r with
    | Ok x -> Some x
    | Error x -> None
                     
let path_split =
  Str.split (Str.regexp "/")
                     
let ok_rep body =
  Server.respond_string ~status:`OK ~body:body () 
       
let lookup t ssid id =
  DB.lookup t ssid id >>= fun svc_opt ->
  match svc_opt with
  | Some svc ->
     let body = Fmt.strf "%a\n" (Irmin.Type.pp_json service_t) svc in 
     ok_rep body

  | None ->
     Server.respond_string ~status:`Not_found ~body:"No such service exists" ()
                        
let register t ssid svc =
  DB.add_service t ssid svc >>= fun body -> ok_rep body 

let leave t ssid id =
  DB.rm_service t ssid id >>= fun rep -> ok_rep rep 
                        

let create_server_set t ssid =
  DB.mk_server_set t ssid >>= fun rep -> ok_rep rep
                                                
let remove_server_set t ssid =
  DB.rm_server_set t ssid >>= fun rep -> ok_rep rep
                                                
let list_services t ssid =
  DB.list_members t ssid >>= fun members ->
  let body = Fmt.strf "%a\n" (ServerSet.pp) members in
  ok_rep body 

let continue_if_deserialized o f =
  match o with
  | Some x -> f x 
  | None ->
     Server.respond_string ~status:`Bad_request ~body:"unable to unmarshal data" ()
  

let basic_handler t conn req body =
  let path = Request.uri (req)  |> Uri.path |> path_split in
  let meth = Request.meth(req) in 
  match (meth, path) with
  | (`GET, ["catalog"; ssid]) ->  list_services t ssid
  | (`GET, ["catalog"; ssid; id]) -> lookup t ssid id
  | (`DELETE, ["catalog"; ssid]) -> remove_server_set t ssid
  | (`PUT, ["catalog"; ssid]) -> create_server_set t ssid
  | (`DELETE, ["catalog"; ssid; id]) -> leave t ssid id
  | (`POST, ["catalog"; ssid]) ->
     Cohttp_lwt_body.to_string body >>= fun s ->
     let o = Irmin.Type.decode_json service_t (Jsonm.decoder (`String s) ) |> res_opt  in
     continue_if_deserialized o (register t ssid) 
     
  
