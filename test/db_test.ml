open OUnit2
open Service_registry.Model
open Lwt.Infix
open Batteries
open Service_registry.DB       
       
let fs_root = "/tmp/service-registry"
let config = Irmin_git.config ~bare:true fs_root
let repo = Lwt_main.run ( DataStore.Repo.v config >>= DataStore.master )

let sample_service =
  Service.make "192.168.1.10" 8080 ()

let list_in_repo root =
  let cmd = ("irmin", [|"irmin"; "ls"; "catalog"; (Fmt.strf "--root=%s" root) |]) in
  Lwt_process.pread cmd 
               
let ssid = "mysql"
             
let create_server_set ctx = 
  mk_server_set repo ssid >>= fun s ->
  list_in_repo fs_root >>= fun keys ->
  let b = String.exists keys ssid in
  assert_bool "server set creation test failed" b;
  Lwt.return_unit
  
                                         
let add_and_lookup ctx  = 
  add_service repo ssid sample_service >>= fun x ->
  lookup repo ssid sample_service.id >>= fun svco ->
  let b =  Option.is_some svco in
  assert_bool "either addition or the lookup function is faulty" b;
  Lwt.return_unit
  
                           
                                       
let member_listing ctx = 
  list_members repo ssid >>= fun ss ->
  let b = List.length ss > 0 in
  Lwt.return ( assert_bool "membership listing test failed" b )

                                                                

let update_test ctx = 
  let nh = "192.168.1.11" in
  let ns = {sample_service with address = nh;} in
  
  update_service repo ssid ns >>= fun _ ->
  lookup repo ssid ns.id >>= fun svco ->
  match svco with
  | Some svc ->
     let b = svc.address = nh in
     Lwt.return ( assert_bool "update test failed" b )

  | None -> Lwt.return ( assert_bool "update_test failed" false )
                        
  
let add_and_remove ctx =
  let ssvc = Service.make "192.168.1.12" 3663 () in
  add_service repo ssid ssvc >>= fun _ ->
  rm_service repo ssid ssvc.id >>= fun _ ->
  lookup repo ssid ssvc.id >>= fun o ->

  match o with
  | Some x ->
     Lwt.return ( assert_failure "the service wasn't removed" )                
  | None ->
     Lwt.return_unit

let refresh_and_lookup ctx =
  Lwt_unix.sleep 2.0 >>= fun () -> 
  refresh repo ssid sample_service.id >>= fun _ ->
  lookup repo ssid sample_service.id >>= fun o ->

  match o with
  | Some svc ->
     let b = sample_service.ts <> svc.ts in
     assert_bool "time stamp was not updated" b;
     Lwt.return_unit
       
  | None ->
     assert_failure "no such svc";
     Lwt.return_unit

let ss_deletion ctx =
  rm_server_set repo ssid >>= fun _ ->
  list_in_repo fs_root >>= fun s ->
  let b = String.exists s ssid != true in
  assert_bool "deletion of server set failed" b;
  Lwt.return_unit
  
       
let run f ctxt = Lwt_main.run (f ctxt)
                             
let suite =
  "DB Suite" >:::
    [
      "serverset creation" >:: run create_server_set;
      "add and lookup service" >:: run add_and_lookup;
      "server set membership listing" >:: run member_listing;
      "update test" >:: run update_test;
      "add and remove test" >:: run add_and_remove;
      "refresh and lookup" >:: run refresh_and_lookup;
      "Server Set deletion" >:: run ss_deletion;
    ]
                                         
                  
                                       
                                       
                                       
  
  
                                         
