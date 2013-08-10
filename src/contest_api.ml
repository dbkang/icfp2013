(* I've only been able to run this in an interactive shell so 
   far.  To load this in the interactive shell, run these
   commands:

       #use "topfind";;
       #require "netclient";;
       #use "src/contest_api.ml";;
*)

open Http_client;;
open String;;

(* Contest API constants*)
let contest_domain = "http://icfpc2013.cloudapp.net";;
let auth_key = "?auth=0191yxaUHzX7C1if61Js0utpeBAUYTCAAlmgdvbAvpsH1H";;

let eval_path  = "/eval";;
let guess_path = "/guess";;
let train_path = "/train";;

let eval_post_url  = contest_domain ^ eval_path  ^ auth_key;;
let guess_post_url = contest_domain ^ guess_path ^ auth_key;;
let train_post_url = contest_domain ^ train_path ^ auth_key;;

let problem_size n = "{\"size\": " ^ (string_of_int n) ^ "}";;

let send_post post_url post_body =
  let pipeline = new pipeline in
  let post_op = new Http_client.post_raw post_url post_body in
    pipeline#add(post_op);
    pipeline#run();
    post_op#get_resp_body()
;;

let get_training_problem size =
  send_post train_post_url (problem_size size)
;;

let guess problem_id program =
   send_post guess_post_url ("{\"id\": \"" ^ problem_id ^ "\", \"program\": \"" ^ (program_to_string program) ^ "\"}")
;;