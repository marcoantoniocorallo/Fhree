(** Utilities and support functions *)
open Syntax;;
open Exceptions;;

(**
  length tuple: returns the number of elements of tuple
  @param tuple : an unmutable sequence of values
  @return the number of elements of tuple
*)
let length (tuple : 'a sequence) : int =
  let rec f t n = match t with
    |Nil -> n
    |Cons(_,xs) -> f xs (n+1)
  in f tuple 0
;;

(** get tuple i: returns the i-th element of tuple
  @param tuple : an unmutable sequence of values
  @param index : a 0-based index
  @return the i-th element of tuple
  @raise Invalid_argument if index > (length tuple)
*)
let get (tuple : 'a sequence) (index : int) : 'a = 
  let rec g t i = match t,i with
    |Cons(x,xs),0 -> x
    |Cons(x,xs),n -> g xs (n-1)
    |_,_ -> raise (Invalid_argument "Index Out Of Bound! in utils:get")
  in g tuple index
;;

(** reverse seq: returns the reversed sequence of values
  @param t : the sequence of values
  @return the reversed sequence of values    
*)
let reverse (t : 'a sequence) : 'a sequence =
	let rec r t acc = match t with
		| Nil -> acc
		| Cons(x,xs) -> r xs (Cons(x,acc))
	in r t Nil
;;

(** string_of_value v: returns a string s representing v
  @param v : a value to be parsed
  @return a string representing v
*)
let rec string_of_value (v : value) : string = match v with
  |Int k -> string_of_int k
  |Float k -> string_of_float k
  |Bool k -> string_of_bool k
  |Char k -> String.make 1 k
  |String s -> s
  |Tuple(t) -> string_of_collection t
  |ListV(l) -> string_of_collection l
  |Closure(f, x, body, env) -> "Closure of "^f^" "^x
and string_of_collection t = 
    let rec f t acc = match t with
      |Nil -> acc
      |Cons(x, xs) -> f xs (acc^(string_of_value x)^" ")
    in f t ""
;;

(** string_of_position p returns a string representing the position p
 *  @param p : position 
 *  @return : a string (row,column) representing the position p
 *)
let string_of_position p =
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  Printf.sprintf "(%d, %d)" line_number column
;;

(** string_of_loc (startp,endp) returns a string representing the pair
 *  @param (startp, endp): pair of Lexing.position objects, 
           representing respectively the start point and the end point of the token
    @return : a string (startp.row, startp.column) - (endp.row, endp.column)
 *)
let string_of_loc (startp, endp) =
  let sp = string_of_position startp in
  let ep = string_of_position endp in
  Printf.sprintf "%s-%s" sp ep
;;