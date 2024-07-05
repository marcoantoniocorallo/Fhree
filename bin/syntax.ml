(** Syntax of the language : definition of ADT, EVT, types and Environment *)

(** 
	An environment is a map from identifier to "something".
	In the semantics this something is a value (what the identifier is bound to).
	In the type system this "something" is a type.
  In CFA this is a label.
	For simplicity we represent the environment as an association list, i.e., a list of pair (identifier, data).
 *)
type 'v env = (string * 'v) list
[@@deriving show]

(**
  Given an environment {env} and an identifier {x} it returns the data {x} is bound to.
  If there is no binding, it raises an exception.
*)
let rec lookup env x =
  match env with
  | []        -> raise(Exceptions.Binding_Error(x ^ " not found"))
  | (y, v)::r -> if x=y then v else lookup r x
;;

(* alias: identifiers are strings *)
type ide = string
[@@deriving show]

(** Located node *)
type 'a located = { loc : Lexing.position * Lexing.position [@opaque]; value : 'a}
[@@deriving show]

(** Algebraic Data Types *)
type exp =
	| Empty 																					(* Empty Expression *)
	| CstI of int                              	 			(* Integer constants *)
	| CstB of bool                               			(* Boolean constants *)
	| CstF of float															 			(* Float constants *)
	| CstC of char															 			(* Char literals *)
	| CstS of string														 			(* String literals *)

	| Uop of ide * located_exp												(* Unary operators *)
	| Bop of located_exp * ide * located_exp         	(* Binary operators *)

	| Var of ide                              	 			(* Variables/Identifiers *)
	| Let of ide * ttype option * located_exp * located_exp        	
                                                    (* Typed declaration *)
	| If of located_exp * located_exp * located_exp   (* If-then-else *)
	| Fun of ide * ide * ttype * located_exp 					(* Fun expr (f, x, type of f, fBody)  *)
	| Call of located_exp * located_exp               (* Fun application *)

	| Tup of located_exp list			 				 	 					(* Heterogeneous Fixed-length list of expressions *)
	| Proj of located_exp * located_exp               (* i-th element of tuple *)

	| Lst of located_exp list 		 					 					(* Homogeneous List of expressions *)
	| Cons_op of located_exp * located_exp						(* Concatenates an exp in head of a list *)
	| Head of located_exp															(* Return the first element of a list *)
	| Tail of located_exp															(* Return the list without the first el *)
	| IsEmpty of located_exp													(* Tests if a list is empty *)
	
	| NativeFunction of ( value -> value ) * ide option 
																										(* (ocaml code, arg_name) *)
	[@@deriving show]

(** Types definition *)
and ttype = 
	| Tunit																						(*  Type unit *)
  | Tint                                            (*  Type int *)
  | Tbool                                           (*  Type bool *)
  | Tfloat                                          (*  Type float *)
  | Tchar                                           (*  Type char *)
  | Tstring                                         (*  Type string *)
  | Tfun of ttype * ttype                           (*  Type of function *)
  | Ttuple of ttype list                        		(*  Compound type: tuple *)
  | Tlist of ttype option                           (*  Compound type: list *)
	[@@deriving show]

(** Expressible and denotable values. 
 *  A runtime value is an integer, a boolean, a float, a char, a string,
 *	a tuple or a list of values or a function closure.
 *)
and value =
	| Unit																									(* evaluation of an empty program *)
	| Int of int
	| Bool of bool
	| Float of float
	| Char of char
	| String of string
	| Closure of string * string * located_exp * value env	(* (f, x, fBody, fDeclEnv) *)
	| Tuple of value list   																(* Heterogeneous fixed-length tuple of values*)
	| ListV of value list   																(* Homogeneous list of values *)
	[@@deriving show]
	
and located_exp = exp located                 			(* ( exp * location ) *)
[@@deriving show]
;;
