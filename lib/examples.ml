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

(* send + more = money. 72 answers *)
let smm_nonpar letters = 
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
    ] in
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



let task_2rev_par len q = [condePar [[reverso q (lst len 'x')];[reverso q (lst len 'a')]]]

let task_2rev_nonpar len q = [conde [[reverso q (lst len 'y')];[reverso q (lst len 'b')]]]

let task_10rev_par len q = [condePar [
  [reverso q (lst len '1')];
  [reverso q (lst len '2')];
  [reverso q (lst len '3')];
  [reverso q (lst len '4')];
  [reverso q (lst len '5')];

  [reverso q (lst len '6')];
  [reverso q (lst len '7')];
  [reverso q (lst len '8')];
  [reverso q (lst len '9')];
  [reverso q (lst len '0')];
]]

let task_10rev_nonpar len q = [conde [
  [reverso q (lst len '1')];
  [reverso q (lst len '2')];
  [reverso q (lst len '3')];
  [reverso q (lst len '4')];
  [reverso q (lst len '5')];

  [reverso q (lst len '6')];
  [reverso q (lst len '7')];
  [reverso q (lst len '8')];
  [reverso q (lst len '9')];
  [reverso q (lst len '0')];
]]



let task_2app_par len q = [condePar [
  [appendo (lst len '1') (lst len 'a') q];
  [appendo (lst len '2') (lst len 'b') q]
  ]]


let task_2app_nonpar len q = [conde [
  [appendo (lst len '1') (lst len 'a') q];
  [appendo (lst len '2') (lst len 'b') q]
  ]]



let task_10app_par len q = [condePar [
  [appendo (lst len '1') (lst len 'a') q];
  [appendo (lst len '2') (lst len 'b') q];
  [appendo (lst len '3') (lst len 'c') q];
  [appendo (lst len '4') (lst len 'd') q];
  [appendo (lst len '5') (lst len 'e') q];
  [appendo (lst len '6') (lst len 'f') q];
  [appendo (lst len '7') (lst len 'g') q];
  [appendo (lst len '8') (lst len 'h') q];
  [appendo (lst len '9') (lst len 'i') q];
  [appendo (lst len '0') (lst len 'j') q];
  ]]

let task_10app_nonpar len q = [conde [
  [appendo (lst len '1') (lst len 'a') q];
  [appendo (lst len '2') (lst len 'b') q];
  [appendo (lst len '3') (lst len 'c') q];
  [appendo (lst len '4') (lst len 'd') q];
  [appendo (lst len '5') (lst len 'e') q];
  [appendo (lst len '6') (lst len 'f') q];
  [appendo (lst len '7') (lst len 'g') q];
  [appendo (lst len '8') (lst len 'h') q];
  [appendo (lst len '9') (lst len 'i') q];
  [appendo (lst len '0') (lst len 'j') q];
  ]]


  let smm_par letters = 
    let add_digits aug add cin cout digit =
      let par_sum = fresh () in
      let sum = fresh () in
      all [
        domfd par_sum (range 0 18);
        domfd sum (range 0 19);
        plusfd aug add par_sum;
        plusfd par_sum cin sum;
        condePar [
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
      ] in
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
