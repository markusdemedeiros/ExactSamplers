(** Distributions *)

(* Sources *)
(*  Sampling exactly from the normal distribution, Charles F. F. Karney, 2013 *)
(*  https://arxiv.org/abs/1303.6257 *)
(*  Huber, M.L. (2016). Perfect Simulation (1st ed.). Chapman and Hall/CRC. *)
(*  https://doi.org/10.1201/b19235 *)


Random.self_init ()

let rec pow x n
  = match n with
    | 0 -> 1
    | i -> x * (pow x (i - 1))

let creal_lt x y : bool =
  CReal.compare x y < 0

let bool_to_string b = if b then "true" else "false"

let get_prec (c : CReal.t) : int =
  match (c.cache) with
  | Some (v, _) -> v
  | None -> -1

let get_cache_val (c : CReal.t) =
  match (c.cache) with
  | Some (_, v) -> v
  | None -> Z.zero

 (* Allegedly... *)
 (* |4^n\cdot x - \ap{x}{n}| < 1 *)
 (* So, *)
 (* | 4^(get_prec c) x - (get_cache_val c) | < 1 *)
 (* get_cache_val c -1 < 4^(get_prec c) x < get_cache_val c + 1 *)
 (* (get_cache_val c - 1) / 4^(get_prec c) <  x < (get_cache_val c + 1) / 4^(get_prec c) *)

let exact_lb (c : CReal.t) : Q.t =
  Q.make
    (Z.sub (get_cache_val c) (Z.one))
    (Z.pow (Z.of_int 4) (get_prec c))

let exact_ub (c : CReal.t) : Q.t =
  Q.make
    (Z.add (get_cache_val c) (Z.one)) 
    (Z.pow (Z.of_int 4) (get_prec c))

let show_creal_data c =
  let d = get_prec c in
  let x = Z.to_string (get_cache_val c) in
  Printf.sprintf "%d,%s," d x

let show_creal_bounds c =
  let lb = Q.to_float (exact_lb c) in
  let ub = Q.to_float (exact_ub c) in
  Printf.sprintf "%f,%f," lb ub

let show_creal_list (ident : string) (l : CReal.t list) : string =
  let l1 = List.map show_creal_bounds l in
  let l2 = List.map (Printf.sprintf "%s,%s" ident) l1 in
  String.concat "\n" l2



(** Base random samplers *)

type 'a sampler = unit -> 'a

let urand_bit_sampler : bool sampler
  = fun _ -> Random.int 2 = 0

(* Discrete uniform sampler over [0..b-1] *)
let urand_nat_sampler (b : int) : int sampler
  = fun _ -> Random.int b




(** Debug information *)

type debug_info = int

let default_debug_info : debug_info
  = 0

let global_debug_array : debug_info array ref
  = ref (Array.make 0 default_debug_info)

(* Add a new statistic for a urand, returning index into the global list *)
let alloc_urand_debug_info : unit -> int
  = fun _ ->
      begin
        global_debug_array := Array.append (!global_debug_array) (Array.make 1 default_debug_info) ;
        Array.length (!global_debug_array) - 1
      end




(** Lazily sampled uniform reals *)
type sample_cache = {
  (* value, bits embedding of the CReal value and bits  *)
  mutable value : Z.t;
  mutable crumbs: int;

  (* program to generate new bits *)
  bit_sampler : bool sampler;

  (* index into the global stats database *)
  global_debug_index: int;
}


(*  Tests for the gaussian distribution show that almost all sampled bits use precision < 8, *)
(*  so we might eliminate resamplings by starting out with more bits. This adds approximations, *)
(*  but does not impact the other operations. From the zarith implementation[1] big ints *)
(*  are represented as ocaml ints when small enough *)
(*  [1] https://github.com/ocaml/Zarith/blob/5f8e5e2ded3eaea063ac1a48029a9572069e1af8/caml_z.c#L127C3-L128C70 *)

(* A bit width whose values can fit inside a single ocaml int *)
let eager_cache_size =
  if Sys.int_size < 32
     then 4 (* 2 * 8 = 16 <= 31 *)
     else 5 (* 2 * 16 = 32 <= 63 *)

(* urand with empty cache *)
let default_cache s ix = {
  value = (Z.of_int 0);
  crumbs = 0;
  bit_sampler = s;
  global_debug_index = ix
}

(* urand with presampled bits *)
let eager_cache s ix = {
  value = (Z.of_int (Random.int (pow 2 (2 * eager_cache_size))));
  crumbs = eager_cache_size;
  bit_sampler = s;
  global_debug_index = ix
}




(** Update a sample_cache with two additional bits *)
let sample_bit (c : sample_cache ref) : unit =
    begin
      (* Increment the precision that is being stored  *)
      !c.crumbs <- !c.crumbs + 1 ;
      let update _ =
        let c_shift_left = Z.mul (Z.of_int 2) (!c.value) in
        if !c.bit_sampler ()
          then !c.value <- Z.succ c_shift_left
          else !c.value <- c_shift_left  in
      update () ;
      update () ;
      (!global_debug_array).(!c.global_debug_index) <- !c.crumbs ;
    end



(** Update a sample_cache to contain at least p bits *)
let rec sample_prec (c : sample_cache ref) (p : int) : unit =
    if p <= (!c.crumbs)
      then ()
      else begin sample_bit c; sample_prec c p end

let unif (s : bool sampler) : CReal.t sampler
  = fun _ -> let stat_ix = alloc_urand_debug_info () in
           let c = ref (eager_cache s stat_ix) in
           (!global_debug_array).(!c.global_debug_index) <- (!c.crumbs) ;
           CReal.create (fun p ->
               (* update the cache to contain at least p bits *)
               let () = sample_prec c p in
               (* divide the right value to obtain the right integer *)
               CReal.fdiv_Bexp (!c.value) ((!c.crumbs) - p))

let urand_unif : CReal.t sampler
  = fun _ -> unif urand_bit_sampler ()


 (** Exponential distribution (Von Neumann's method) *)

let rec sample_run (i : int) (ui : CReal.t) : int
 = let ui1 = urand_unif () in
   if (CReal.compare ui ui1 == -1)
      then i
      else sample_run (i + 1) (ui1)

let sample_exp : CReal.t sampler =
  let rec vn_rec (l : int) : CReal.t
    = let x = urand_unif () in
      let n = sample_run 0 x in
      if (n mod 2 == 1)
        then vn_rec (l + 1)
        else (CReal.add x (CReal.of_int l))
  in fun _ -> vn_rec 0


(** Half exponential Bernoulli trial *)

let sample_half_exp_bern : bool sampler =
  fun _ ->
    let n = sample_run 0 (CReal.div CReal.one CReal.two) in
    (n mod 2 == 0)

(** Gaussian distribution *)

let rec sample_gaussian : CReal.t sampler =
  let h_run
   = fun _ ->
      let rec h_run_rec (i : int) = fun _ -> if (sample_half_exp_bern ()) then h_run_rec (i + 1) () else i in
      h_run_rec 0 () in
  let rec h_run_len
   = fun i ->
      (i == 0) || if sample_half_exp_bern () then (h_run_len (i - 1)) else false in
  let rec sample_k
    = fun _ ->
        let k = h_run () in
        if h_run_len (k * (k - 1))
          then k
          else sample_k () in
  let sC : int -> int = fun m ->
    let s = urand_nat_sampler m () in
    match s with
      | 0 -> -1
      | 1 -> 0
      | _ -> 1 in
  let sT (x: CReal.t) (y: CReal.t) (z: CReal.t) (r : CReal.t) (f: int) : bool
   = (CReal.compare z y == 1) ||
     (f == -1) ||
     (f == 0 && (CReal.compare r x == 1)) in
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
        in b_rec x 0) in
  let n_accept (x : CReal.t)
    = fun k ->
        let rec n4_rec
          = fun i -> if i == 0
                      then true
                      else  if sB x k ()
                            then n4_rec (i - 1)
                            else false in
        n4_rec (k + 1) in
  fun _ ->
    let k = sample_k () in
    let x = urand_unif () in
    if n_accept x k
      then
        let y = CReal.add x (CReal.of_int k) in
        if urand_bit_sampler ()
          then y
          else CReal.neg y
      else sample_gaussian ()





(** Laplacian distribution *)

let sample_laplace (lambda : CReal.t) =
  fun _ ->
    let y = CReal.mul lambda (CReal.ln (urand_unif ())) in
    if urand_bit_sampler ()
      then y
      else CReal.neg y

(*

(** Rejection samplers on the unit interval *)

let rec urand_rejection_sampler (maxv : CReal.t) (pdf : CReal.t -> CReal.t) =
  fun _ ->
    let sample = urand_unif () in
    let value  = CReal.mul maxv (urand_unif ()) in
    if CReal.compare (pdf sample) value == -1
      then urand_rejection_sampler maxv pdf ()
      else sample

let quad_pdf (x : CReal.t) = CReal.mul (CReal.of_int 3) (CReal.mul x x)

*)




(** Random sorted list *)


let list_sorting_experiment (n : int) (c : out_channel) =
  let l : CReal.t list ref = ref [] in
  for i = 1 to n do
    let new_unif = urand_unif () in
    (* Hack to make sure we always sample at least one crumb *)
    (* May sample more than one crumb *)
    let _ = CReal.approx new_unif 1 in
    let l_sorted = List.sort CReal.compare (new_unif :: !l) in
    l := l_sorted ;
    Printf.fprintf c "%s\n" (show_creal_list (string_of_int i) !l) ;
  done ;;


list_sorting_experiment 20 stdout







(** 1D Brownian Motion *)


(*
type brownian_sequence = {
  mutable hist : CReal.t list;
}


let b = ref { hist = [CReal.zero] }

let sim_N : int = 1000

let prt_state (i : int) (b : brownian_sequence) =
  let f (c : CReal.t) =
    ( let s = Z.to_string (get_cache_val c) in
      let v = get_prec c in
      Printf.printf "%d,%d,%s\n" i v s) in
  let _ = List.map f b.hist in ()

*)



(** Experiments *)

let file = "out.csv"










(*
Printf.printf "iterate,quad-digits,approx\n" ;;
for j = 0 to 10 do
  !b.hist <- [CReal.zero] ;
  for i = 0 to (sim_N - 1) do
    (* Generate a new sample *)
    let d = CReal.sqrt (CReal.div CReal.one (CReal.of_int sim_N)) in
    let bnm1 = List.nth (!b.hist) (List.length (!b.hist) - 1) in
    let z = sample_gaussian () in
    let bn = CReal.add bnm1 (CReal.mul z d) in
    !b.hist <- List.append !b.hist [bn] ;
  done ;
  let _ = CReal.to_string (List.nth (!b.hist) (List.length (!b.hist) - 1)) 10 in
  prt_state j (!b)
done
*)


(*  let _ = CReal.to_string (List.nth !b.hist ((List.length !b.hist) - 1)) 10 ;; *)
(*  let _ = prt_state (!b) ;; *)

(*
let blast = (List.nth !b.hist ((List.length !b.hist) - 1)) ;;
let _ = Printf.sprintf "%d, %s" (get_prec blast) (Z.to_string (get_cache_val blast)) ;;
*)



(*
let v _ =
    let c = sample_gaussian () in
    let _ = (CReal.to_string c 10) in
    (*  *Printf.printf "%s,\n" (CReal.to_string c 10) ;; *)
    Printf.printf "%s" (show_stats ()) ;;

v () ;;
*)
(*
let rec profiler n =
  if n == 0
    then ()
    else let _ = (CReal.to_string (sample_gaussian ()) 10) in
         profiler (n - 1) ;;

profiler 10000 ;;
*)
