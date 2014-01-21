(*
  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.
*)

(* tail-rec for range, from ocaml tutorial *)

let range a b =
   let rec range2 a b accum =
      if b < a then accum
      else range2 a (b-1) (b :: accum) in
   range2 a b [];;

print_int (List.fold_left (+) 0 (List.filter (fun x -> (x mod 3) = 0 || (x mod 5) = 0) (range 1 999)))
