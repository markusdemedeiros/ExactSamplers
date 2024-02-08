let _ : CReal.t = CReal.zero ;;

(* Lazily sampled uniform reals *)

let mk_urand_bit_sampler : unit -> (unit -> bool)
  = fun _ -> (Random.self_init (); (fun _ -> Random.int 2 = 0));;

type sample_cache = {
  mutable value : Z.t;
  mutable bits : int;
  bit_sampler : unit -> bool;
}

let sample_bit (c : sample_cache ref) : unit =
    begin
      !c.bits <- (!c.bits + 1) ;
      if (!c.bit_sampler ())
        then !c.value <- (Z.succ (Z.mul (Z.of_int 2) (!c.value)))
        else !c.value <- (Z.mul (Z.of_int 2) (!c.value)) ;
      if (!c.bit_sampler ())
        then !c.value <- (Z.succ (Z.mul (Z.of_int 2) (!c.value)))
        else !c.value <- (Z.mul (Z.of_int 2) (!c.value)) ;
    end ;;

(* Update a sample_cache to contain at least p bits *)
let rec sample_prec (c : sample_cache ref) (p : int) : unit =
    if p <= (!c.bits)
      then ()
      else begin sample_bit c; sample_prec c p end ;;


let unif (s : unit -> bool) : unit -> CReal.t
  = fun _ ->
        let c = ref { value = (Z.of_int 0); bits = 0; bit_sampler = s } in
        CReal.create (fun p ->
            (* update the cache to contain at least p bits *)
            let () = sample_prec c p in
            (* divide the right value to obtain the right integer *)
            CReal.fdiv_Bexp (!c.value) ((!c.bits) - p)) ;;

let urand_unif : unit -> CReal.t
  = fun _ -> let u = mk_urand_bit_sampler () in unif u () ;;


let () =
    let c = urand_unif () in
    Printf.printf "%s,\n" (CReal.to_string c 30) ;;
