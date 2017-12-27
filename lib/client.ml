open Cohttp_lwt
open Lwt.Infix
open Model

module Client = Cohttp_lwt_unix.Client        
type host_port = {host: string; port: int option}

type watch_session = {mutable ss: server_set ; mu: Lwt_mutex.t}
                  
let dp = 6423


let print_ss ss =
  let fstr = Fmt.strf "%a\n" (ServerSet.pp) ss in
  print_endline fstr
           
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

let add_service host ?port:(port=dp) ssid svc =
  let url = Uri.make ~scheme:"http" ~host ~port ~path:(ss_path ssid) () in
  let payload = Fmt.strf "%a\n" (Irmin.Type.pp_json service_t) svc in
  let body = Cohttp_lwt_body.of_string payload in
  Client.post ~body:body url >>= fun _ ->
  Lwt.return_unit
  
  
  
  

let send_beat host ?port:(port=6423) ssid id =
  let url = Uri.make ~scheme:"http" ~host:host ~port:port ~path:(svc_path ssid id) () in
  Client.post url >>= fun (rep, body) ->
  match Response.status rep with
  | `OK -> Lwt.return_unit
  | `Not_found ->
     Cohttp_lwt_body.to_string body >>= fun msg ->
     Lwt.fail_with msg
  | _ -> Lwt.fail_with "unknown failure"


let rec beat_proc host ?port:(port=dp) ssid svc i =
  Lwt_unix.sleep i >>= fun () ->
  
  Lwt.catch
    (fun () -> send_beat host ~port ssid svc.id)
    (fun e ->
      let nsvc = {svc with ts = Unix.time ()} in 
      add_service host ~port ssid nsvc
    )
  >>= fun () -> beat_proc host ~port ssid svc i
  
             
let lookup host ?port:(port=dp) ssid id =
  let url = Uri.make ~scheme:"http" ~host ~port ~path:(svc_path ssid id) () in
  Client.get url >>= fun (rep, body) ->
  body |> Cohttp_lwt_body.to_string >|= fun s ->
  let r = Irmin.Type.decode_json service_t (Jsonm.decoder (`String s)) in
  res_opt r

let list_services host ?port:(port=dp) ssid =
  let url = Uri.make ~scheme:"http" ~host ~port ~path:(ss_path ssid) () in
  Client.get url >>= fun (rep, body) ->
  body |> Cohttp_lwt_body.to_string >>= fun s ->
  let ss = ServerSet.of_string s |> res_opt in
  Lwt.return ss
             
let leave host ?port:(port=dp) ssid id =
  let url = Uri.make ~scheme:"http" ~host ~port ~path:(svc_path ssid id) () in
  Client.delete url >>= fun (rep, body) -> Lwt.return () 

                
let register host ?port:(port=dp) ssid ?interval:(i=25.0) svc =
  let url = Uri.make ~scheme:"http" ~host ~port ~path:(ss_path ssid) () in
  let payload = Fmt.strf "%a\n" (Irmin.Type.pp_json service_t) svc in
  let body = Cohttp_lwt_body.of_string payload in
  Client.post ~body:body url >|= fun (rep, body) -> Lwt.async ( fun () -> beat_proc host ~port ssid svc i)
                                       
  
let create_server_set host ?port:(port=dp) ssid =
  let url = Uri.make ~scheme:"http" ~host ~port ~path:(ss_path ssid) () in
  Client.put url >|= fun (rep, body) -> () 
  

let remove_server_set host ?port:(port=dp) ssid =
  let url = Uri.make ~scheme:"http" ~host ~port ~path:(ss_path ssid) () in
  Client.delete url >|= fun (rep, body) -> ()    
   
(*remember to wrap in async*)
let rec watch host ?port:(port=dp) ssid sess = 
  let path = Fmt.strf "watch/%s" ssid in 
  let url = Uri.make ~scheme:"http" ~host ~port ~path () in
  Client.get url >>= fun (rep, body) ->
  body |> Cohttp_lwt_body.to_string >>= fun s ->
  let ss = ServerSet.of_string s |> res_opt in
  match ss with
  | Some svc_set ->
     Lwt_mutex.lock sess.mu >>= fun () ->
     sess.ss <- svc_set;
     Lwt_mutex.unlock sess.mu;
     watch host ~port ssid sess
  | _ ->
     watch host ~port ssid sess
                                                
  
let make_session =
  {ss = []; mu = (Lwt_mutex.create () );}
