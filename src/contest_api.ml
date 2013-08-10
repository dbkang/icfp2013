(* I've only been able to run this in an interactive shell so 
   far.  To load this in the interactive shell, run these
   commands:

       #use "topfind";;
       #require "netclient";;
       #use "src/contest_api.ml";;
*)

open Http_client;;
open String;;

let train_post_url = "http://icfpc2013.cloudapp.net/train?auth=0191yxaUHzX7C1if61Js0utpeBAUYTCAAlmgdvbAvpsH1H";;

let problem_size n = "{\"size\": " ^ (string_of_int n) ^ "}";;

let send_post post_url post_body =
  let pipeline = new pipeline in
  let post_op = new Http_client.post_raw post_url post_body in
    pipeline#add(post_op);
    pipeline#run();
    post_op#get_resp_body();;

let get_training_problem size =
  send_post train_post_url (problem_size size)
