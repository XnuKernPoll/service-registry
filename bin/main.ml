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
                           
let rec removal_proc root iv time_out t =
  Lwt_unix.sleep iv >>= fun () -> 
  list_files root  >>= fun ssids ->
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
  let open Model in 
  let tbl = Hashtbl.create 120 in
  let w = {tbl = tbl; mu = ( Lwt_mutex.create () );} in
  Server.make ~callback:(fun conn req body -> Service.basic_handler t w conn req body) () 
                              
let start root port = 
  let conf = config root in
  DataStore.Repo.v conf >>= DataStore.master >>= fun t ->
  Server.create ~mode:(`TCP (`Port port)) (server t)

let start_s root port =
  Lwt_main.run (start root port)
                        
let start_t =
  Term.(const start_s $ root $ port )

let info =
  let doc = "A simple service discovery service" in
  let man = [`S Manpage.s_bugs; `P "To report bugs contact me at flatmapds@gmail.com"] in
  Term.info "service-registry" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man
    
let _ = Term.exit @@ Term.eval (start_t, info)
