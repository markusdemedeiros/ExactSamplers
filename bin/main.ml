Random.self_init () ;;

let rec pow x n
  = match n with
    | 0 -> 1
    | i -> x * (pow x (i - 1)) ;;

(** Bit sampler *)
let urand_bit_sampler : unit -> bool
  = (fun _ -> Random.int 2 = 0) ;;

(** Discrete uniform sampler from [0.. b-1] *)
let mk_urand_nat_sampler : int -> (unit -> int)
  = fun b -> (Random.self_init (); (fun _ -> Random.int b));;

let urand_nat = fun b -> mk_urand_nat_sampler b () ;;

(** Additional tracking for *)

let global_bit_stats = ref (Array.make 0 0) ;;

let alloc_urand_stat : unit -> int
  = fun _ -> begin
            global_bit_stats := Array.append (!global_bit_stats) (Array.make 1 0) ;
            (Array.length (!global_bit_stats) - 1)
           end ;;

let show_stats : unit -> string
  = fun _ -> Array.fold_left (fun s -> fun i -> Printf.sprintf "%s%d\n" s i) "" (!global_bit_stats)

(** Lazily sampled uniform reals *)

type sample_cache = {
  mutable value : Z.t;
  mutable bits : int;
  bit_sampler : unit -> bool;
  global_stats_index: int;
}


let default_cache s ix
  = { value = (Z.of_int 0); bits = 0; bit_sampler = s; global_stats_index = ix } ;;

(*  Tests for the gaussian distributioan show that almost all sampled bits use precision < 8, *)
(*  so we might eliminate resamplings by starting out with more bits. This adds approximations, *)
(*  but does not impact the other operations. From the zarith implementation[1] big ints *)
(*  are represented as ocaml ints when small enough *)
(*  [1] https://github.com/ocaml/Zarith/blob/5f8e5e2ded3eaea063ac1a48029a9572069e1af8/caml_z.c#L127C3-L128C70 *)

let eager_cache s ix width
  = { value = (Z.of_int (Random.int (pow 2 (2 * width)))); bits = width; bit_sampler = s; global_stats_index = ix } ;;

(* A bit width whose values can fit inside a single ocaml int *)
let eager_cache_size =
  if Sys.int_size < 32
     then 4 (* 2 * 8 = 16 <= 31 *)
     else 5 (* 2 * 16 = 32 <= 63 *) ;;

let default_eager_cache s ix = eager_cache s ix eager_cache_size ;;

let sample_bit (c : sample_cache ref) : unit =
    begin
      !c.bits <- (!c.bits + 1) ;
      if (!c.bit_sampler ())
        then !c.value <- (Z.succ (Z.mul (Z.of_int 2) (!c.value)))
        else !c.value <- (Z.mul (Z.of_int 2) (!c.value)) ;
      if (!c.bit_sampler ())
        then !c.value <- (Z.succ (Z.mul (Z.of_int 2) (!c.value)))
        else !c.value <- (Z.mul (Z.of_int 2) (!c.value)) ;
      (!global_bit_stats).(!c.global_stats_index) <- (!c.bits) ;
    end ;;

(* Update a sample_cache to contain at least p bits *)
let rec sample_prec (c : sample_cache ref) (p : int) : unit =
    if p <= (!c.bits)
      then ()
      else begin sample_bit c; sample_prec c p end ;;

let unif (s : unit -> bool) : unit -> CReal.t
  = fun _ -> let stat_ix = alloc_urand_stat () in
           let c = ref (default_eager_cache s stat_ix) in
           (!global_bit_stats).(!c.global_stats_index) <- (!c.bits) ;
           CReal.create (fun p ->
               (* update the cache to contain at least p bits *)
               let () = sample_prec c p in
               (* divide the right value to obtain the right integer *)
               CReal.fdiv_Bexp (!c.value) ((!c.bits) - p)) ;;

let urand_unif : unit -> CReal.t
  = fun _ -> unif urand_bit_sampler () ;;


(** Distributions *)
(* CReal implementation for: *)
(*  Sampling exactly from the normal distribution, Charles F. F. Karney, 2013 *)
(*  https://arxiv.org/abs/1303.6257 *)


 (** Exponential distribution (Von Neumann's method) *)

let rec sample_run (i : int) (ui : CReal.t) : int
 = let ui1 = urand_unif () in
   if (CReal.compare ui ui1 == -1)
      then i
      else sample_run (i + 1) (ui1) ;;

let sample_exp =
  let rec vn_rec (l : int) : CReal.t
    = let x = urand_unif () in
      let n = sample_run 0 x in
      if (n mod 2 == 1)
        then vn_rec (l + 1)
        else (CReal.add x (CReal.of_int l))
  in fun _ -> vn_rec 0 ;;


(** Half exponential Bernoulli trial *)

let sample_half_exp_bern  =
  fun _ ->
    let n = sample_run 0 (CReal.div CReal.one CReal.two) in
    (n mod 2 == 0) ;;

(** Gaussian distribution *)

let h_run
 = fun _ ->
    let rec h_run_rec (i : int) = fun _ -> if (sample_half_exp_bern ()) then h_run_rec (i + 1) () else i in
    h_run_rec 0 () ;;

let rec h_run_len
 = fun i ->
    (i == 0) || if sample_half_exp_bern () then (h_run_len (i - 1)) else false ;;

let rec sample_k
  = fun _ ->
      let k = h_run () in
      if h_run_len (k * (k - 1))
        then k
        else sample_k () ;;

let sC : int -> int = fun m ->
  let s = urand_nat m in
  match s with
    | 0 -> -1
    | 1 -> 0
    | _ -> 1 ;;

let sT (x: CReal.t) (y: CReal.t) (z: CReal.t) (r : CReal.t) (f: int) : bool
 = (CReal.compare z y == 1) ||
   (f == -1) ||
   (f == 0 && (CReal.compare r x == 1)) ;;

(*
(* z > y -> go to B4 (true) *)
Printf.printf "%b =? true \n" (sT CReal.two CReal.zero CReal.one (CReal.of_int 3) 0) ;;
(* z < y; f < 0 -> go to b4 (true) *)
Printf.printf "%b =? true \n" (sT CReal.two CReal.one CReal.zero (CReal.of_int 3) (-1)) ;;
(* z < y; f > 0 -> continue (false) *)
Printf.printf "%b =? false \n" (sT CReal.two CReal.one CReal.zero (CReal.of_int 3) (1)) ;;
(* z < y; f = 0; r < x -> continue (false)*)
Printf.printf "%b =? false \n" (sT CReal.two CReal.one CReal.zero (CReal.of_int 1) (0)) ;;
(* z < y; f = 0; r > x -> go to b4 (true)  *)
Printf.printf "%b =? true \n" (sT CReal.two CReal.one CReal.zero (CReal.of_int 3) (0)) ;;
*)

let sB
  = (fun x -> fun k -> fun _ ->
      let rec b_rec
          = (fun y -> fun n ->
              let z = urand_unif () in
              let f = sC (2 * k + 2) in
              let r = urand_unif () in
              if (sT x y z r f)
                then (n mod 2 == 0)
                else (b_rec z (n + 1)))
      in b_rec x 0) ;;

let n_accept (x : CReal.t)
  = fun k ->
      let rec n4_rec
        = fun i -> if i == 0
                    then true
                    else  if sB x k ()
                          then n4_rec (i - 1)
                          else false in
      n4_rec (k + 1) ;;

let rec sample_gaussian =
  fun _ ->
    let k = sample_k () in
    let x = urand_unif () in
    if n_accept x k
      then
        let y = CReal.add x (CReal.of_int k) in
        if urand_bit_sampler ()
          then y
          else CReal.neg y
      else sample_gaussian () ;;

(** Laplacian distribution *)

let sample_laplace (lambda : CReal.t) =
  fun _ ->
    let y = CReal.mul lambda (CReal.ln (urand_unif ())) in
    if urand_bit_sampler ()
      then y
      else CReal.neg y ;;

(** Rejection samplers on the unit interval *)

let rec urand_rejection_sampler (maxv : CReal.t) (pdf : CReal.t -> CReal.t) =
  fun _ ->
    let sample = urand_unif () in
    let value  = CReal.mul maxv (urand_unif ()) in
    if CReal.compare (pdf sample) value == -1
      then urand_rejection_sampler maxv pdf ()
      else sample ;;

let quad_pdf (x : CReal.t) = CReal.mul (CReal.of_int 3) (CReal.mul x x) ;;



let () =
    let c = urand_rejection_sampler (CReal.of_int 3) quad_pdf () in
    let _ = (CReal.to_string c 10) in
    Printf.printf "%s,\n" (CReal.to_string c 10) ;; 
    (*  *Printf.printf "%s" (show_stats ()) ;; *)



(*
let () =
    let c = sample_gaussian () in
    let _ = (CReal.to_string c 10) in
    (*  *Printf.printf "%s,\n" (CReal.to_string c 10) ;; *)
    Printf.printf "%s" (show_stats ()) ;;
*)

(*
let rec profiler n =
  if n == 0
    then ()
    else let _ = (CReal.to_string (sample_gaussian ()) 10) in
         profiler (n - 1) ;;

profiler 10000 ;;
*)

