(* A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
*)

let reverse str =
  let len = String.length str in
  let res = String.create len in
  for i = 0 to pred len do
    let j = pred len - i in
    res.[i] <- str.[j]
  done;
  (res);;

let isPalindrome n =
   let str1 = string_of_int n in
   let str2 = reverse str1 in
   if str1 = str2 then true
   else false;;

let pal = ref 0
let im = ref 0 and jm = ref 0;;

(*
let rec inner_loop i j =
   let prod = i * j in
   if j < 100 then 0
   else if isPalindrome prod then
           (pal := prod; jm := j; 1)
   else inner_loop i j-1
   in

let rec outer_loop i =
   let _ = inner_loop i 999 in
   if i < 100 then 0
   else if i < !jm then 1
   else outer_loop i-1
   in

let _ = outer_loop 999;

*)
  for i = 999 downto 100 do
    for j = 999 downto 100 do
      let prod = i * j in
      if prod > !pal && isPalindrome prod then pal := prod
    done
  done;
print_int !pal
