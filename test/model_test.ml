open OUnit2
open Service_registry.Model
open Batteries
       
let port = 8050
             
let sample_ss =
  let prefix = "192.168.1." in
  let range = List.of_enum (10--19) in

  let f = (fun x ->
      let host =  prefix ^ (Int.to_string x) in
      let id = Int.to_string (x - 9) in
      (host, id)
    ) in
  
  let hosts = List.map (fun x -> f x) range in
  List.map ( fun (host, uuid) -> Service.make host port ~uid:uuid () ) hosts 
  
let existance_cond ss id =
  List.exists (fun x -> x.id = id) ss 

let lookup ctx =
  let svc = ServerSet.lookup sample_ss (Int.to_string 1) in
  let b = Option.is_some svc in
  assert_bool "lookup test failed" b


let removal ctx =
  let new_ss = ServerSet.rm_service sample_ss "2" in
  let b = (existance_cond new_ss "2" != true) in
  assert_bool "service removal test failed" b

let addition ctx =
  let svc = Service.make "192.168.1.20" port ~uid:"11" () in
  let new_ss = ServerSet.add_service sample_ss svc in
  let b = existance_cond new_ss "11" in
  assert_bool "addition test failed" b


let update ctx =
  let svc = List.nth sample_ss 2 in
  let nsvc = {svc with port = (Int32.of_int 8090);} in
  let nss = ServerSet.update_service sample_ss nsvc in
  let b =
    match ( ServerSet.lookup nss svc.id ) with
    | Some x -> x.port != svc.port
    | None -> false
  in
  assert_bool "update test failed" b

let refresh_test ctx =
  Unix.sleep 2; 
  let svc = List.nth sample_ss 2 in
  let o = ServerSet.refresh sample_ss svc.id in
  match o with
  | Some x ->
     let b = x.ts <> svc.ts in
     assert_bool "model refresh failed" b
  | None -> assert_failure "couldn't locate service with id"
  
  
let suite =
  "ServerSet suite" >:::
    ["lookup test" >:: lookup;
     "removal test" >:: removal;
     "addition test" >:: addition;
     "refresh service test" >:: refresh_test;
     "update test" >:: update;      
    ]
  
