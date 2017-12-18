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


             
let lookup addr ssid id =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(svc_path ssid id) () in
  Client.get url >>= fun (req, body) ->
  body |> Cohttp_lwt_body.to_string >|= fun s ->
  let r = Irmin.Type.decode_json service_t (Jsonm.decoder (`String s)) in
  res_opt r

let list_services addr ssid =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(ss_path ssid) () in
  Client.get url >>= fun (req, body) ->
  body |> Cohttp_lwt_body.to_string >>= fun s ->
  let ss = ServerSet.of_string s |> res_opt in
  Lwt.return ss
             
let leave addr ssid id =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(svc_path ssid id) () in
  Client.delete url >>= fun (req, body) -> Lwt.return () 

                
let register addr ssid svc =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(ss_path ssid) () in
  let payload = Fmt.strf "%a\n" (Irmin.Type.pp_json service_t) svc in
  let body = Cohttp_lwt_body.of_string payload in
  Client.post ~body:body url >|= fun (rep, body) -> ()
                                       
  
let create_server_set addr ssid =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(ss_path ssid) () in
  Client.put url >|= fun (req, body) -> () 
  

let remove_server_set addr ssid =
  let url = Uri.make ~scheme:"http" ~host:addr.host ~port:(get_port addr) ~path:(ss_path ssid) () in
  Client.delete url >|= fun (req, body) -> ()    

      
      
