(*
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
*)

let fib_max = 4000000

let rec sum_fib_even fib_a fib_b acc =
   let new_fib = fib_a + fib_b in
   if new_fib > fib_max then acc
   else if (new_fib mod 2) = 0 then sum_fib_even fib_b new_fib (acc + new_fib)
   else sum_fib_even fib_b new_fib acc;;

print_int (sum_fib_even 1 1 0)
