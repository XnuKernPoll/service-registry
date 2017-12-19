open Service_registry
open DB       
open Lwt.Infix
open Cohttp_lwt_unix
open Cmdliner
       

let config r = Irmin_git.config ~bare:true r
(* Server.create ~mode:(`TCP (`Port 6423)) *)
                              
let last l =
  let i = List.length l - 1 in
  List.nth l i
           
let list_files p =
  let l = Array.to_list (Sys.readdir p) in
  Lwt.return l 
             
                
let rec removal_proc root iv time_out t =
  Lwt_unix.sleep iv >>= fun () -> 
  list_files root >>= fun ssids ->
  Lwt_list.iter_p (
      fun x ->
      rm_stale t x time_out >>= fun ss ->
      Lwt.return_unit
    ) ssids >>= fun () -> removal_proc root iv time_out t 

               
let port =
  let doc = "specifies the port to listen on defaults to 6423" in
  Arg.(value & opt int 6423 & info ["p"; "port"] ~docv:"PORT" ~doc)

     
let root =
  let doc = "specifies the root directory for the irmin repo defaults to /tmp/service_registry " in
  Arg.(value & opt string "/tmp/service_registry" & info ["r"; "root"] ~docv:"ROOT" ~doc) 
   
                 
let server t =
  Server.make ~callback:(fun conn req body -> Service.basic_handler t conn req body) () 
                              
let start root port =
  let conf = config root in
  DataStore.Repo.v conf >>= DataStore.master >>= fun t ->
  Server.create ~mode:(`TCP (`Port 6423)) (server t)
    
    
let _ = print_endline "hello"

open Cohttp_lwt_unix      

let config = Irmin_git.config ~bare:true "/tmp/service_registry"
                              
let start =
  DataStore.Repo.v config >>= DataStore.master >>= fun t -> 
  Server.create ~mode:(`TCP (`Port 6423)) (Server.make ~callback:(fun conn req body -> Service.basic_handler t conn req body) () ) 
    
let _ = Lwt_main.run start 

