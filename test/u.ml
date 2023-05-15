open Minikanren.Mk
open Minikanren.Ck
open Minikanren.Fd
open Minikanren.Examples

let print_sep name =
  print_string (Printf.sprintf "=========%s========\n" name)

let print_s = List.iter (fun t -> print_string ((string_of_logic_term t) ^ "\n"))

let test ?limit:(limit = -1) name f =
  let time = Unix.time() in
  let _ = print_sep name in
  let q = fresh () in
  let s = run limit q (f q) in
  print_string (Printf.sprintf "Execution time: %f\n" (Unix.time() -. time));
  print_s s

let _ = test ~limit:(-1) "reverso-parallel" smm_par


