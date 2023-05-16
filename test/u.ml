open Minikanren.Mk
open Minikanren.Ck
open Minikanren.Fd
open Minikanren.Examples

let print_sep name =
  print_string (Printf.sprintf "=========%s========\n" name)

let print_s ans = Printf.printf "Answers: %d\n" (List.length ans);
  List.iter (fun t -> print_string ((string_of_logic_term t) ^ "\n")) ans

let test ?limit:(limit = -1) name f =
  let time = Unix.time() in
  let _ = print_sep name in
  let q = fresh () in
  let s = run limit q (f q) in
  print_string (Printf.sprintf "Execution time: %f\n" (Unix.time() -. time));
  print_s s

  
  let _ = test ~limit:(-1) "reverso" (task_100rev_parpar 100)


