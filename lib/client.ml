open Cohttp_lwt
open Lwt.Infix
open Model

module Client = Cohttp_lwt_unix.Client        
type host_port = {host: string; port: int option}

let dp = 6423
           
let get_port hp =
  match hp.port with
  | Some x -> x
  | None -> dp

let res_opt r =
  match r with
    | Ok x -> Some x
    | Error x -> None
                   

let ss_path ssid =
  Fmt.strf "catalog/%s" ssid 

let svc_path ssid id =
  Fmt.strf "catalog/%s/%s" ssid id


let send_beat addr ssid id =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(svc_path ssid id) () in
  Client.post url >>= fun (rep, body) ->
  match Response.status rep with
  | `OK -> Lwt.return_unit
  | `Not_found ->
     Cohttp_lwt_body.to_string body >>= fun msg ->
     Lwt.fail_with msg
  | _ -> Lwt.fail_with "unknown failure"


let rec beat_proc addr ssid id i =
  Lwt_unix.sleep i >>= fun () -> send_beat addr ssid id
  >>= fun () -> beat_proc addr ssid id i                                                                                                   
             
let lookup addr ssid id =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(svc_path ssid id) () in
  Client.get url >>= fun (rep, body) ->
  body |> Cohttp_lwt_body.to_string >|= fun s ->
  let r = Irmin.Type.decode_json service_t (Jsonm.decoder (`String s)) in
  res_opt r

let list_services addr ssid =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(ss_path ssid) () in
  Client.get url >>= fun (rep, body) ->
  body |> Cohttp_lwt_body.to_string >>= fun s ->
  let ss = ServerSet.of_string s |> res_opt in
  Lwt.return ss
             
let leave addr ssid id =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(svc_path ssid id) () in
  Client.delete url >>= fun (rep, body) -> Lwt.return () 

                
let register addr ssid ?interval:(i=25.0) svc =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(ss_path ssid) () in
  let payload = Fmt.strf "%a\n" (Irmin.Type.pp_json service_t) svc in
  let body = Cohttp_lwt_body.of_string payload in
  Client.post ~body:body url >>= fun (rep, body) -> beat_proc addr ssid svc.id i
                                       
  
let create_server_set addr ssid =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(ss_path ssid) () in
  Client.put url >|= fun (rep, body) -> () 
  

let remove_server_set addr ssid =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(ss_path ssid) () in
  Client.delete url >|= fun (rep, body) -> ()    
   

                                          
