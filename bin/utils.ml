(** Utilities and support functions *)
open Syntax;;

(** get tuple i: returns the i-th element of tuple
  @param tuple : an unmutable sequence of values
  @param index : a 0-based index
  @return the i-th element of tuple
  @raise Invalid_argument if index > (length tuple)
*)
let get (tuple : 'a list) (index : int) : 'a = 
  let rec g t i = match t,i with
    | x::_,0 -> x
    | _::xs,n -> g xs (n-1)
    | _,_ -> raise (Exceptions.Index_Out_Of_Bound "Index Out Of Bound! in utils:get")
  in g tuple index
;;

(** string_of_value v: returns a string s representing v
  @param v : a value to be parsed
  @return a string representing v
*)
let rec string_of_value (v : value) : string = match v with
  | Unit -> ""
  | Int k -> string_of_int k
  | Float k -> string_of_float k
  | Bool k -> string_of_bool k
  | Char k -> String.make 1 k
  | String s -> s
  | Tuple(c)  
  | ListV(c) -> List.map string_of_value c |> String.concat "; "
  | Closure(f, _, _, _) -> "Closure of "^f
;;

(** string_of_ttype t: returns a string s representing t
  @param t : a ttype to be parsed
  @return a string representing t
*)
let rec string_of_ttype (t : ttype) : string = match t with
  | Tunit -> "unit"
  | Tint -> "int"
  | Tbool -> "bool"
  | Tfloat -> "float"
  | Tchar -> "char"
  | Tstring -> "string"
  | Tfun(t1,t2) -> ((string_of_ttype t1)^" -> "^(string_of_ttype t2))
  | Ttuple tt -> List.map string_of_ttype tt |> String.concat "; "
  | Tlist tt -> (if (Option.is_some tt) then (string_of_ttype (Option.get tt)) else "empty")^" list"
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