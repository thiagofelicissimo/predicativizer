open Str
   
type lvl = Var of string | Max of lvl * lvl | Lzero | Lsucc of lvl

                                                             
let rec find_y l x =
  match l with
  | [] -> None
  | (x', y') :: l' -> if x' = x then Some y' else find_y l' x
                                                             
let rec print_lvl l =
  match l with
  | Var s -> s
  | Max (s1, s2) -> "(cts.max " ^ (print_lvl s1) ^ " " ^ (print_lvl s2) ^ ")"
  | Lzero -> "(cts.enum cts.uzero)"
  | Lsucc s -> "(cts.succ " ^ (print_lvl s) ^ ")"
                                                             
let str_to_constraint str =
  let match_equal = Str.regexp "\\[\\] \\([A-Za-z0-9_\\?]+\\).\\?\\([A-Za-z0-9_\\?]+\\) --> \\([A-Za-z0-9_\\?]+\\).\\?\\([A-Za-z0-9_\\?]+\\)." in
  let match_ax = Str.regexp "Axiom \\([A-Za-z0-9_\\?]+\\).\\?\\([A-Za-z0-9_\\?]+\\) \\([A-Za-z0-9_\\?]+\\).\\?\\([A-Za-z0-9_\\?]+\\)" in
  let match_rule = Str.regexp "Rule \\([A-Za-z0-9_\\?]+\\).\\?\\([A-Za-z0-9_\\?]+\\) \\([A-Za-z0-9_\\?]+\\).\\?\\([A-Za-z0-9_\\?]+\\) \\([A-Za-z0-9_\\?]+\\).\\?\\([A-Za-z0-9_\\?]+\\)" in
  try
    let _ = search_forward match_ax str 0 in
    let t1 = "l" ^ matched_group 2 str in
    let t2 = "l" ^ matched_group 4 str in
    Some (Lsucc (Var t1), Var t2)
  with
    _ -> try
      let _ = search_forward match_rule str 0 in
      let t1 = "l" ^ matched_group 2 str in
      let t2 = "l" ^ matched_group 4 str in
      let t3 = "l" ^ matched_group 6 str in
      Some (Max (Var t1, Var t2), Var t3)
    with
      _ -> try
        let _ = search_forward match_equal str 0 in
        let t1 = "l" ^ matched_group 2 str in
        let t2 = "l" ^ matched_group 4 str in
        Some (Var t1, Var t2)
      with _ -> None
           

let read_constraints line_list =
  List.fold_left
    (fun acc s ->
      match str_to_constraint s with
      | None -> acc
      | Some x -> x :: acc)
    []
    line_list

let read_lines fp =
  let lines = ref [] in
  try
    while true do
      lines := (input_line fp) :: !lines
    done; !lines
  with
    _ -> !lines


let rec appears_in (name : string) (i : lvl) =
  match i with
  | Var s -> name = s
  | Max (s,k) -> (appears_in name s) || (appears_in name k)
  | _ -> false

let rec substitute (t : lvl) (n : string) (u : lvl) =
  match t with
  | Var m -> if n = m then u else Var m
  | Lsucc t' -> Lsucc (substitute t' n u)
  | Max (t1, t2) -> Max (substitute t1 n u, substitute t2 n u)
  | _ -> t
       
let rec unify (subst : (string * lvl) list) (l : (lvl * lvl) list) : (string * lvl) list option =
  match l with
  | [] -> Some subst
  | (Var m, t) :: l' ->
     if Var m = t then unify subst l'
     else if appears_in m t then None
     else
       let new_subst = (m, t) :: (List.map (fun (n, s) -> n, substitute s m t) subst) in
       let new_l = List.map (fun (x,y) -> substitute x m t, substitute y m t) l in
       unify new_subst new_l
  | (t, Var m) :: l' -> unify subst ((Var m, t) :: l')
  | (Lsucc t1, Lsucc t2) :: l' -> unify subst ((t1, t2) :: l')
  | (Max (t1, s1), Max (t2, s2)) :: l' -> unify subst ((t1, t2) :: (s1, s2) :: l')
  | _ :: l -> None


let rec substitute_in_string (subst : (string * lvl) list) (s : string) =
  let reg = Str.regexp "\\([A-Za-z0-9_\\?]+\\).\\?\\([A-Za-z0-9_\\?]+\\)" in
  let reg_of file lvl = Str.regexp (file ^ ".\\?" ^ lvl) in  
  let s = ref s in
  try while true do
        let _ = search_forward reg !s 0 in
        let file_id = matched_group 1 !s in
        let lvl_id = matched_group 2 !s in
        let exp =
          match find_y subst ("l" ^ lvl_id) with
          | Some y -> print_lvl y
          | None -> "l" ^ lvl_id in
        s := global_replace (reg_of file_id lvl_id) exp !s
      done; !s
  with _ -> !s

let rec substitute_in_file subst fin fout =
  try
    while true do
      let s = input_line fin in
      let s' = substitute_in_string subst s in
      Printf.fprintf fout "%s\n" s'
    done
  with
    _ -> ()

exception E
        
let () =
  let input_file_name = Sys.argv.(1) in
  let constraints_file_name = Sys.argv.(2) in
  let input_file = open_in input_file_name in
  let constraints_file = open_in constraints_file_name in
  let output_file = open_out "out.dk" in
  let lines = read_lines constraints_file in
  let consts = read_constraints lines in
  let subst =
    match unify [] consts with
    | Some x -> x
    | _ -> raise E in
  substitute_in_file subst input_file output_file;
  close_out output_file;
  close_in input_file;
  close_in constraints_file
