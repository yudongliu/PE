(* The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
*)

let smallestFactor n =
    let rec aux d n =
      if n = 1 then 1 else
        if n mod d = 0 then d else aux (d+1) n
    in
    aux 2 n;;

let rec max_factor prod =
   let f = smallestFactor prod in
   if f >= prod then prod
   else max_factor (prod/f);;

(* Need to use big_int for this to work *)
print_int (max_factor 13195)
