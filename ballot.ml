let get_ballot_lines bfile = 
  let ic = open_in bfile in
  let rec get_bl acc = 
    match (try Some (input_line ic) with End_of_file -> None) with
    None -> List.rev acc
    | Some l -> get_bl (l::acc) in
  let ballot_lines = get_bl [] in
  let () = close_in ic in ballot_lines

(* Given a list of comma-delimited "ballots" return a list of lists where each
   element is the list of comma-delimited options.  
 *)
let rec ballots_from_lines (blines : string list) : string list list = match blines with 
| [] -> [] 
| [""] -> []
| h::t -> (String.split_on_char(',') h)::ballots_from_lines(t)

(* compare helper for first_error *)
let rec first_error_compare p ls = match ls with 
| [] -> false 
| h::t -> if List.mem h p then first_error_compare p t else true

(* helper function for first_error *)
let rec first_error_help p ls opt = match ls with 
| [] -> None
| h::t -> if first_error_compare p h then Some opt else first_error_help p t (opt + 1)

(* Identify the index of the first ballot that has a candidate not in the candidate list *)
(* Return None if all ballots are legal. *)
let first_error (cs : 'a list) (ballots : 'a list list) : int option = first_error_help cs ballots 1


(* Helper function for extracting string from tuple *)
let get_string a = match a with 
| (str, num) -> str

(* Given a list of (candidate,score) pairs, compute the String length of the longest candidate *)
let rec max_len (sl : (string * float) list) = match sl with 
| [] -> 0
| h::t -> if String.length(get_string h) > max_len(t) then String.length(get_string h) else max_len(t)

(* helper function for adding extra spaces *)
let rec extra_spaces s spaces = match spaces with 
| 0 -> s
| _ -> extra_spaces s (spaces - 1) ^ " "

(* Pad the string s (with spaces) to length at least l *)
let rec pad (l : int) s = 
  if l <= 0 || l <= String.length(s) then s 
  else extra_spaces s (l - String.length s)

(* prints the list of rankings out *)
let print_rankings sl = 
  let padlen = max (String.length "Candidate") (max_len sl) in
  let () = print_endline ((pad padlen "Candidate") ^ " \t Score") in
  let () = print_endline ((String.make padlen '-') ^ " \t -----") in
  let rec p_loop = function [] -> () 
  | (c,s)::t -> let () = print_endline ((pad padlen c) ^ " \t " ^ (string_of_float s)) in p_loop t
in p_loop sl
