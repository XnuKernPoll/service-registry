open OUnit2
open Service_registry.Model
open Lwt.Infix
open Batteries
open Service_registry.DB       
       
let fs_root = "/tmp/service-registry"
let config = Irmin_git.config ~bare:true fs_root
let repo = DataStore.Repo.v config >>= DataStore.master

let sample_service =
  Service.make "192.168.1.10" 8080 ()

let list_in_repo root =
  let cmd = ("irmin", [|"irmin"; "ls"; "catalog"; (Fmt.strf "--root=%s" root) |]) in
  Lwt_process.pread cmd 
               
let ssid = "mysql"
             
let create_server_set ctx = 
  repo >>= fun t ->
  mk_server_set t ssid >>= fun s ->
  list_in_repo fs_root >>= fun keys ->
  let b = String.exists keys ssid in
  assert_bool "server set creation test failed" b;
  Lwt.return_unit
  
                                         
let add_and_lookup ctx  = 
  repo >>= fun t ->
  add_service t ssid sample_service >>= fun x ->
  lookup t ssid sample_service.id >>= fun svco ->
  let b =  Option.is_some svco in
  assert_bool "either addition or the lookup function is faulty" b;
  Lwt.return_unit
  
                           
                                       
let member_listing ctx = 
  repo >>= fun t ->
  list_members t ssid >>= fun ss ->
  let b = List.length ss > 0 in
  Lwt.return ( assert_bool "membership listing test failed" b )

                          

                                      

let update_test ctx = 
  repo >>= fun t ->
  let nh = "192.168.1.11" in
  let ns = {sample_service with address = nh;} in
  
  update_service t ssid ns >>= fun _ ->
  lookup t ssid ns.id >>= fun svco ->

  match svco with
  | Some svc ->
     let b = svc.address = nh in
     Lwt.return ( assert_bool "update test failed" b )

  | None -> Lwt.return ( assert_bool "update_test failed" false )
                        
  
    
let run f ctxt = Lwt_main.run (f ctxt)
                             
let suite =
  "DB Suite" >:::
    [
      "serverset creation" >:: run create_server_set;
      "add and lookup service" >:: run add_and_lookup;
      "server set membership listing" >:: run member_listing;
    ]
                                         
                  
                                       
                                       
                                       
  
  
                                         
