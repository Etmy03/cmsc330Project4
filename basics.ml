open Funs

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
    match tup with 
    | (a, b, c) -> (c, b, a) ;;

let is_even x = 
    let x' = if x < 0 then -x else x in 
    match x' mod 2 with 
    | 1 -> false 
    | 0 -> true ;;

let volume x y = 
    match (x,y) with 
    | ((a1, b1, c1), (a2, b2, c2)) -> 
    (if a2>a1 then (a2 - a1) else (a1 - a2)) * 
    (if b2>b1 then (b2 - b1) else (b1 - b2)) * 
    (if c2>c1 then (c2 - c1) else (c1 - c2)) ;;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibonacci(n - 1) + fibonacci(n - 2) ;;

let rec log x y = 
    let y' = y/x in 
    match y' with
    | 0 -> 0
    | 1 -> 1
    | _ -> 1 + log x y' ;;

let rec gcf x y = 
    if x == y then x 
    else
        match y with
        | 0 -> x
        | _ -> gcf y (x mod y) ;;


let rec maxFuncChain init funcs = 
    let rec my_func acc = function
    | [] -> acc
    | f :: lst -> my_func (f acc) lst
    in
    
    my_func init funcs ;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec reverse lst = 
    match lst with
    | [] -> []
    | x :: newLst -> reverse newLst @ [x] ;;

let rec zip lst1 lst2 = 
    match (lst1, lst2) with
    | ([], []) -> []
    | ([], [x, y]) -> []
    | ([x, y], []) -> []
    | ((a, b)::l1, (c, d)::l2) -> (a,b,c,d) :: zip l1 l2 ;;

let rec is_palindrome lst = 
    (reverse lst) = lst ;; 

    (*let rec palin ls1 ls2 =
        match (ls1, ls2) with
        | ([], []) -> true
        | ([], [x]) -> true
        | ([x], []) -> true
        | ([], _::_) -> false
        | (_::_, []) -> false
        | (x::l1, y::l2) -> if x <> y then false else palin l1 l2
        in 

        palin (reverse lst) lst;;*)

let is_prime num =
  match num with
  | num when num <= 1 -> false
  | num when num <= 3 -> true
  | num when num mod 2 = 0 || num mod 3 = 0 -> false
  | _ ->
    let rec is_prime_check x =
      if x * x > num then true
      else if num mod x = 0 then false
      else is_prime_check (x + 6)
    in
    is_prime_check 5 ;;

let rec prime_list lst =
  match lst with
  | [] -> []
  | x :: newLst when is_prime x -> x :: prime_list newLst
  | _ :: newLst -> prime_list newLst ;;

let rec square_primes lst = 
    let lst2 = prime_list lst in 
    let rec mult acc = function
    | [] -> acc
    | x :: newLst -> mult ((x, x * 2) :: acc) newLst
    in
    reverse (mult [] lst2) ;;

let rec partition p lst = 
    let rec my_func l1 l2 l3 =
    match l1 with
    | [] -> (reverse l2, reverse l3)
    | x :: newLst -> if p x then my_func newLst (x::l2) l3 
    else my_func newLst l2 (x::l3)
    in 
    my_func lst [] [] ;;

(*****************)
(* Part 4: HOF *)
(*****************)

let is_present lst x = 
    map (fun i -> if i = x then 1 else 0) lst;;

let count_occ lst target = 
    let l1 = is_present lst target in
    fold (fun x y -> x+y) 0 l1;;
  
let jumping_tuples lst1 lst2 = failwith "unimplemented"

let addgenerator x = 
    fun y -> fold_right (fun i j -> i+j) [x] y;;

let uniq lst = 
    fold (fun acc x -> 
    if (count_occ acc x)>0 then acc else x :: acc) [] lst ;;

let ap fns args = 
    let apply_fn fn_list arg =
    List.map (fun fn -> fn arg) fn_list
    in
    List.fold_right (fun arg acc -> (apply_fn fns arg) @ acc) args []

