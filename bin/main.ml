open Service_registry
open DB       
open Lwt.Infix
open Cohttp_lwt_unix      

let config = Irmin_git.config ~bare:true "/tmp/service_registry"
                              
let start t =
  DataStore.Repo.v config >>= DataStore.master >>= fun t -> 
  Server.create ~mode:(`TCP (`Port 6423)) (Server.make ~callback:(fun conn req body -> Service.basic_handler t conn req body) () ) 
    
