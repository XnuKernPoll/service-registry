open DB
open Model
open Lwt.Infix
             
module Server = Cohttp_lwt_unix.Server
                  
type rep = (Cohttp.Response.t * Cohttp_lwt_body.t)             
type t = (rep Lwt.t * rep Lwt.u) list
type watches = {tbl: (string, t) Hashtbl.t; mu: Lwt_mutex.t}

let print_ss ss =
  let fstr = Fmt.strf "%a\n" (ServerSet.pp) ss in
  print_endline fstr
                 
let reply_with_ss ss =
  let pl = Fmt.strf "%a\n" (ServerSet.pp) ss in
  Server.respond_string ~status:`OK ~body:pl ()


let send_to_watchers watches ssid rep  =
  Lwt_list.iter_p (
      fun (_, w) -> 
      Lwt.return (Lwt.wakeup w rep)
    ) (Hashtbl.find watches.tbl ssid) >>= fun () ->
  Lwt_mutex.lock watches.mu >>= fun () -> 
  Hashtbl.replace watches.tbl ssid [];
  Lwt_mutex.unlock watches.mu;
  Lwt.return_unit 
  
(*will remove the printing after testing*)                   
let callback watches ssid diff =
  match diff with
  | `Added v ->
     print_endline "Added";
     print_ss v;
     reply_with_ss v >>= fun rep -> 
     send_to_watchers watches ssid rep
                      
  | `Updated (_, v) ->
     print_endline "updated";
     print_ss v;
     reply_with_ss v >>= fun rep -> 
     send_to_watchers watches ssid rep

  | `Removed _ ->
     let msg = Fmt.strf "server set %s was deleted" ssid in
     Server.respond_string ~status:`OK ~body:msg () >>= fun rep ->
     send_to_watchers watches ssid rep
                 

