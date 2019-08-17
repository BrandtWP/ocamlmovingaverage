#load "str.cma"

let rec import file =
  let reg_separator = Str.regexp "," in
  (* Skip the first line, columns headers *)
  try
    (* Create a list of values from a line *)
    let line_list = Str.split reg_separator (input_line file) in
    float_of_string(List.nth line_list 4) :: import file

  with 
    | End_of_file -> close_in file; [];;
  
let ic = open_in "aapl.us.txt"
let _ = input_line ic
let data = import ic


let sma l =
  match l with
  | [] -> []
  | hd :: tl -> let n = ref 1. in
    let rec newTerm l prev = 
      match l with
      | [] -> []
      | hd :: tl -> n := !n +. 1.; (prev *. !n +. hd) /. (!n +. 1.) :: newTerm tl hd;
    in
    newTerm tl hd;
;;

let rec firstk k list = match list with
| [] -> failwith "firstk"
| hd::tl -> if k=1 then [hd] else hd::firstk (k-1) tl;;

let rec sum list = match list with
| [] -> 0.
| hd :: tl -> hd +. sum tl
;;

let ma list n = 
  let average l = sum (firstk n l) /. float_of_int(n) in
  let rec getTerm l =  
    match l with
    | [] -> []
    | hd :: tl -> if List.length(tl) = (n-1) then [] else average (hd :: tl) :: (getTerm tl) in
  getTerm list
;;

