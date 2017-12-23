open OUnit2
open Model
open Batteries
       

let sample_ss =
  let prefix = "192.168.1." in
  let port = 8050 in 
  let range = List.of_enum (10--19) in

  let f = (fun x ->
      let host =  prefix ^ (Int.to_string x) in
      let id = Int.to_string (x - 9) in
      (host, id)
    ) in
  
  let hosts = List.map (fun x -> f x) range in
  List.map ( fun (host, uuid) -> Service.make host port uuid ) hosts 
  
let existance_cond ss id =
  List.exists (fun x -> x.id = id) ss 

let lookup ctx =
  let svc = ServerSet.lookup sample_ss "1" in
  let b = Option.is_some svc in
  assert_bool "lookup test failed" b


let removal ctx =
  let new_ss = ServerSet.rm_service sample_ss "2" in
  let b = List.exists (fun x -> x.id = "2") new_ss != true in
  assert_bool "service removal test failed" b

let addition ctx =
  let svc = Service.make "192.168.1.20" port "11" in
  let new_ss = ServerSet.add_service sample_ss svc in
  let b = existance_cond new_ss id in
  assert_bool "service addition test failed" b 
            
  
  
  
