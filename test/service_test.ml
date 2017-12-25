open Service_registry.Service
open Service_registry.Model
open OUnit2
open Lwt.Infix

module String = Batteries.String
                  
       
       

module DataStore = Service_registry.DB.DataStore
                     
let fs_root = "/tmp/service-registry"
let config = Irmin_git.config ~bare:true fs_root
let repo = Lwt_main.run ( DataStore.Repo.v config >>= DataStore.master )

let sample_service =
  Service.make "192.168.1.10" 8080 ()                        

let ssid = "mysql"
             
let res_to_opt r =
  match r with
  | Ok x -> Some x
  | Error x -> None
    
let decode_svc s =
  let res = Irmin.Type.decode_json service_t (Jsonm.decoder (`String s) ) in
  res_to_opt res 
  
                                         
let creation ctx =
  create_server_set repo ssid >>= fun _ ->
  Db_test.list_in_repo fs_root >>= fun s ->
  let b = String.exists s ssid in
  assert_bool "serverset creation failed" b;
  Lwt.return_unit 


let register_and_lookup ctx =
  register repo ssid sample_service >>= fun _ ->
  lookup repo ssid sample_service.id >>= fun (req, body) ->
  Cohttp_lwt_body.to_string body >>= fun cstring ->
  let svco = decode_svc cstring in
  (match svco with
   | Some svc -> assert_equal svc sample_service
   | None -> assert_failure "either registration or lookup ops failed"
  );
  Lwt.return_unit
              
let listing ctx =
  list_services repo ssid >>= fun (rep, body) ->
  Cohttp_lwt_body.to_string body >>= fun cstring ->
  let sso = ServerSet.of_string cstring |> res_to_opt in
  (match sso with
   | Some ss ->
      let b = List.length ss > 0 in
      assert_bool "Service Listing Test Failed due to empty list" b
                  
   | None ->
      assert_failure "Service Listing Test Failed nothing was unmarshalled"
  );  
  Lwt.return_unit
        
let run f ctxt = Lwt_main.run (f ctxt)
    
let suite =
  "Service Endpoint Suite" >:::
    [
      "creation endpoint" >:: run creation;
      "register and lookup endpoints" >:: run register_and_lookup;
      "service listing endpoint" >:: run listing ; 
    ]
  
