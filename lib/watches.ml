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
  | `Added (_, `Contents (v,_ ) ) ->
     reply_with_ss v >>= fun rep -> 
     send_to_watchers watches ssid rep
                      
  | `Updated (_, (_, `Contents (v, _) ) ) ->
     reply_with_ss v >>= fun rep -> 
     send_to_watchers watches ssid rep

  | `Removed _ ->
     let msg = Fmt.strf "server set %s was deleted" ssid in
     Server.respond_string ~status:`OK ~body:msg () >>= fun rep ->
     send_to_watchers watches ssid rep

  | _ -> Lwt.return_unit
                 

let register_watch t ssid watches =
  Lwt_mutex.lock watches.mu  >>= fun () -> 
  Hashtbl.replace watches.tbl ssid [];
  Lwt.return ( Lwt_mutex.unlock watches.mu ) >>= fun () ->
  DataStore.watch_key t (DB.cat_path ssid) (
      fun diff -> callback watches ssid diff
    )

let add_watcher watches ssid =
  let s = Lwt.wait () in
  let w = Hashtbl.find watches.tbl ssid in
  let nw = w @ [s] in
  Lwt_mutex.lock watches.mu >>= fun () ->
  Hashtbl.replace watches.tbl ssid nw;
  Lwt_mutex.unlock watches.mu;
  let p, _ = s in
  p
  
    
