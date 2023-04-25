open Minikanren.Mk
open Minikanren.Ck
open Minikanren.Fd

let print_sep name =
  print_string (Printf.sprintf "=========%s========\n" name)

let print_s = List.iter (fun t -> print_string ((string_of_logic_term t) ^ "\n"))

let test ?limit:(limit = -1) name f =
  let _ = print_sep name in
  let time = Sys.time() in
  let q = fresh () in
  let s = run limit q (f q) in
  print_string (Printf.sprintf "Execution time: %f\n" (Sys.time() -. time));
  print_s s

let _ = test "miniKanren" (fun q ->
  let x = fresh () in
  [
    conde [
      [succeed];
      [eq q (const_bool true)];
      [
        eq q (List [const_bool true; const_int 1; x]);
        eq x (const_int 10)
      ];
      (let x = fresh () in [eq q x]);
      [eq x q; eq x (const_str "x");];
      [fail; eq q (const_int 1)]
    ]
  ]
)


let _ = use_fd ()

let _ = test "infd" (fun q -> [infd [q] [1; 2; 3]])

let _ = test "neqfd" (fun q ->
  let x = fresh () in
  let y = fresh () in
  [
    infd [x] (range 2 4);
    infd [y] (range 1 3);
    neqfd x y;
    eq q (List [x; y])
  ])

let _ = test "lefd" (fun q ->
  let x = fresh () in
  let y = fresh () in
  [
    infd [x] (make_dom [2; 3; 4]);
    infd [y] (make_dom [1; 2; 3]);
    lefd x y;
    eq q (List [x; y])
  ])

let _ = test "plusfd" (fun q ->
  let x = fresh () in
  let y = fresh () in
  let z = fresh () in
  [
    infd [x] (range 2 8);
    infd [y] (range 1 3);
    infd [z] (range 5 6);
    plusfd x y z;
    eq q (List [x; y; z])
  ])

let _ = test "all_difffd" (fun q ->
  let x = fresh () in
  let y = fresh () in
  let z = fresh () in
  [
    eq q (List [x; y; z]);
    infd [x] (range 1 3);
    infd [y] (range 1 2);
    infd [z] (range 1 1);
    all_difffd q;
  ])

let add_digits aug add cin cout digit =
  let par_sum = fresh () in
  let sum = fresh () in
  all [
    domfd par_sum (range 0 18);
    domfd sum (range 0 19);
    plusfd aug add par_sum;
    plusfd par_sum cin sum;
    conde [
      [
        ltfd (const_int 9) sum;
        eq cout (const_int 1);
        plusfd digit (const_int 10) sum
      ];
      [
        lefd sum (const_int 9);
        eq cout (const_int 0);
        eq digit sum
      ]
    ]
  ]

let _ = test "send-more-money"(fun letters ->
  match fresh_n 11 with
  | [s; e; n; d; m; o; r; y; c0; c1; c2] ->
     [
       eq letters (List [s; e; n; d; m; o; r; y]);
       all_difffd letters;
       infd [s; m] (range 1 9);
       infd [e; n; d; o; r; y] (range 0 9);
       infd [c0; c1; c2] (range 0 1);
       add_digits s m c2 m o;
       add_digits e o c1 c2 n;
       add_digits n r c0 c1 e;
       add_digits d e (const_int 0) c0 y;
     ]
  | _ -> failwith "Fresh_n failed"
)
         

let diag qi qj d rng =
  let qi_d = fresh () in
  let qj_d = fresh () in
  all [
    infd [qi_d; qj_d] rng;
    plusfd qi d qi_d;
    neqfd qi_d qj;
    plusfd qj d qj_d;
    neqfd qj_d qi;
  ]

let diagonals n r =
  let rec helper r i s j =
    match r with
      | [] | [_] -> succeed
      | qi::(y::rtl) ->
        match s with
          | [] -> helper (y::rtl) (i + 1) rtl (i + 2)
          | qj::stl ->
            all [
              diag qi qj (const_int (j - i)) (range 0 (2 * n));
              helper r i stl (j + 1)
            ]
  in helper r 0 (List.tl r) 1

let n_queens q n =
  let rec helper i l =
    if i = 0 then
      all [
        all_difffd (List l);
        diagonals n l;
        eq q (List l);
      ]
    else
      let x = fresh () in
      all [
        infd [x] (range 1 n);
        helper (i - 1) (x::l)
      ]
  in helper n []

let _ = test ~limit:5 "eight-queens" (fun q -> [n_queens q 8])



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

let l =
  (Cons ((const_int 1),
    (Cons ((const_int 2),
      (Cons ((const_int 3),
        (Cons ((const_int 4), List []))))))))

let _ = 
  test ~limit:5 "appendo" (fun q ->
  let a, b = fresh (), fresh () in
  [
    appendo a l l;
    eq q (Cons (a, b))
  ]
)



let rec reverso xy yx = 
  conde [
    [eq xy (List []); eq yx xy];
    [
      fun ss ->
        let h, t = fresh(), fresh() in
        all [
          eq xy (Cons (h, t));
          fun ss ->
            let tr = fresh() in
            all [
              reverso t tr;
              fun ss -> 
                all [
                  appendo tr (Cons (h, List[])) yx;
                ] ss
            ] ss
        ] ss
    ]
  ]

let _ = test ~limit:2 "reverso" (fun q -> [reverso q l])
(* 
let rec anyo t = 
  conde [
    [t];
    [fun ss -> all [anyo t] ss]
  ]

let _ = test ~limit:10 "interleaving with anyo" (fun q ->
  [
    anyo (
      conde (
        [
        [eq (const_int 1) q];
        [eq (const_int 2) q];
        [eq (const_int 3) q];
        [anyo succeed]
      ])
    )
]) *)


