(*Expr.ml*)

(**
   @author Chopin Morgan   @since 19/02/2007
   @version 7.2.11
*)


type expr = X | Y | Sin of expr | Cos of expr | Moyenne of expr * expr | Mult of expr * expr;;

let pi = 3.141592654;;


let rec string_of_expr expr = match expr with
| X -> "x"
| Y -> "y"
| Sin e -> "sin(pi*"^(string_of_expr e)^")"
| Cos e -> "cos(pi*"^(string_of_expr e)^")"
| Moyenne (a,b) -> "("^(string_of_expr a)^"+"^(string_of_expr b)^") / 2"
| Mult (a,b) -> (string_of_expr a)^"*"^(string_of_expr b)


let rec eval expr (x,y) = match expr with
  | X -> x
  | Y -> y
  | Sin a -> sin(pi *. (eval a (x,y)))
  | Cos a -> cos(pi *. (eval a (x,y)))
  | Moyenne (a,b) -> ((eval a (x,y)) +. (eval b (x,y))) /. 2.
  | Mult (a,b) -> (eval a (x,y)) *. (eval b (x,y))


let rec random_expr p =
  let m = (Random.int 4) in
    if (p=0) then match m with
      | 0 | 1 -> X
      | _  -> Y
    else match m with
      | 0 -> (Sin (random_expr (p - 1)))
      | 1 -> (Cos (random_expr (p - 1)))
      | 2 -> (Moyenne ( (random_expr (p - 1)), (random_expr (p - 1)) ))
      | _ -> (Mult ( (random_expr (p - 1)), (random_expr (p - 1)) ))
