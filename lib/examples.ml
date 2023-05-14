open Mk
open Ck
open Fd

(* relations *)

let rec appendo l s out : 'a -> 'a stream =
  conde [
    [eq l (List []); eq s out];
    [
      fun ss ->
        let a, d = fresh (), fresh () in
        all [
          eq (Cons (a, d)) l;
          (fun ss ->
            let res = fresh () in
            all [
              eq (Cons (a, res)) out;
              appendo d s res;
            ] ss)
        ] ss
    ]
  ]

let rec reverso l out =
  conde [
    [eq l (List []); eq out (List [])];
    [ fun ss ->
        let h, t = fresh (), fresh () in
        all [
          eq (Cons (h, t)) l;
          (fun ss ->
            let aa = fresh () in
            all [
              appendo aa (Cons (h, List [])) out;
              reverso t aa;
            ] ss)
        ] ss
    ]
  ]


let rec anyo t = conde [
    [t];
    [fun ss -> all [anyo t] ss]
  ]



(* tests *)

let test_anyo q = anyo (
  conde (
    [
    [eq (const_int 1) q];
    [eq (const_int 2) q];
    [eq (const_int 3) q];
    [anyo succeed]
  ])
)

let l =
  (Cons ((const_int 1),
    (Cons ((const_int 2),
      (Cons ((const_int 3),
        (Cons ((const_int 4), List []))))))))

let test_appendo4 q = 
  let a, b = fresh (), fresh () in
  [
    appendo a l l;
    eq q (Cons (a, b))
  ]

let lst len sym = List.fold_right (fun _ acc -> Cons (const_char sym , acc)) (List.init len (fun _ -> sym )) (List [])

let test_rev3 q = [ reverso q (Cons (const_int 1, (Cons (const_int 2, (Cons (const_int 3, List []))))) ) ]

(* ET: 8-9s *)
let task_2rev10_par q = [condePar [[reverso q (lst 100 'x')];[reverso q (lst 100 'a')]]]

(* ET: 15-16s *)
let task_2rev10_nonpar q = [conde [[reverso q (lst 100 'x')];[reverso q (lst 100 'a')]]]
