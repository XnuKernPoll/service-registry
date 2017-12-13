open Cohttp_lwt
open Lwt.Infix
       
module Client = Cohttp_lwt_unix.Client 

let ss_path uri ssid =
  Fmt.strf ("catalog/%s" ssid) 

let svc_path ssid id =
  Fmt.strf "catalog/%s/%s" ssid id

let dp = 6423
             
let lookup addr ssid id =
  let url = Uri.make ~host:addr ~port:dp ~path:(svc_path ssid id) in
  Client.get url >|= fun (req, body) ->
  let s = body |> Cohttp_lwt_body.to_string in 
  Irmin.Type.decode_json service_t (Jsonm.decoder (`String s))

let list_services addr ssid =
  let url = Uri.make ~host:addr ~port:dp ~path:(ss_path ssid) in
  Client.get url >>= fun (req, body) ->
  let ss = body |> Cohttp_lwt_body.to_string |> ServerSet.of_string in
  Lwt.return ss
             
let leave addr ssid id =
  let url = Uri.make ~host:addr ~port:dp ~path:(svc_path ssid id) in
  Client.delete url >>= fun (req, body) -> Lwt.return () 

                
let register addr ssid svc =
  let url = Uri.make ~host:addr ~port:dp ~path:(ss_path ssid) in
  let payload = Fmt.strf "%a\n" (Irmin.Type.pp_json service_t) svc in
  let body = Cohttp_lwt_body.of_string payload in
  Client.post ~body:body url 
                                       
  
let create_server_set addr ssid =
  let url = Uri.make ~host:addr ~port:dp ~path:(ss_path ssid) in
  Client.put url >|= fun (req, body) -> () 
  

let remove_server_set addr ssid =
  let url = Uri.make ~host:addr ~port:dp ~path:(ss_path ssid) in
  Client.delete url >|= fun (req, body) -> ()    

      
      
