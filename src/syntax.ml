(** Syntax of the language : definition of ADT, EVT, types and Environment 
 *
 *  Design choices:
 *	- there is not a construct for representing functions, 
 *	  there's instead a construct for declaring functions;
 *  	that is: I can declare and call functions, but there's not anonymous function.
 *	  this allows however to declare recursive function without using a dedicated construct.
 *  - The language requires type annotation for function parameter and return type
 *  - The construct Proj t i takes a tuple and an integer literal ! 
 *    That's the only way to perform a type analysis without define complex analyses.    
 *)

(** 
	An environment is a map from identifier to "something".
	In the semantics this something is a value (what the identifier is bound to).
	In the type system this "something" is a type.
  In CFA this something is a label.
	For simplicity we represent the environment as an association list, i.e., a list of pair (identifier, data).
 *)
type 'v env = (string * 'v) list

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

(** Located node *)
type 'a located = { loc : Lexing.position * Lexing.position; value : 'a}

(** Recursive definition of polymorphic data structures *)
type 'a sequence = 
  | Nil                                             (* Represents the empty sequence *)
  | Cons of 'a * 'a sequence                        (* Item of the sequence *)
[@@deriving show]
;;

(** Algebraic Data Types *)
type exp =
	| CstI of int                              	 			(* Integer constants *)
	| CstB of bool                               			(* Boolean constants *)
	| CstF of float															 			(* Float constants *)
	| CstC of char															 			(* Char literals *)
	| CstS of string														 			(* String literals *)
	| Not of located_exp															(* Negation of a bool exp *)
  | Neg of located_exp															(* Negation of an int exp *)
	| Var of ide                              	 			(* Variables/Identifiers *)
	| Let of ide * ttype option * located_exp * located_exp        	
                                                    (* Typed declaration *)
	| Prim of located_exp * ide * located_exp         (* Op Primitives *)
	| If of located_exp * located_exp * located_exp   (* If-then-else *)
	| Letfun of ide * ide * ttype * located_exp * located_exp 
                                                    (* Fun declaration 
                                                    (f, x, type of f, fBody, letBody)  *)
	| Call of located_exp * located_exp               (* Fun application *)
	| Tup of located_exp sequence			 				 	 			(* Heterogeneous Fixed-length list of expressions *)
	| Proj of located_exp * located_exp               (* i-th element of tuple *)
	| Lst of located_exp sequence 		 					 			(* Homogeneous List of expressions *)
	| Cons_op of located_exp * located_exp						(* Concatenates an exp in head of a list *)
	| Head of located_exp															(* Return the first element of a list *)
	| Tail of located_exp															(* Return the list without the first el *)

(** Types definition *)
and ttype = 
  | Tint                                            (*  Type int *)
  | Tbool                                           (*  Type bool *)
  | Tfloat                                          (*  Type float *)
  | Tchar                                           (*  Type char *)
  | Tstring                                         (*  Type string *)
  | Tfun of ttype * ttype                           (*  Type of function *)
  | Ttuple of ttype sequence                        (*  Compound type: tuple *)
  | Tlist of ttype option                           (*  Compound type: list *)

and located_exp = exp located                 			(* ( exp * location ) *)

(** Expressible and denotable values. 
 *  A runtime value is an integer, a boolean, a float, a char, a string,
 *	a tuple or a list of values or a function closure.
 *)
and value =
	| Int of int
	| Bool of bool
	| Float of float
	| Char of char
	| String of string
	| Closure of string * string * located_exp * value env	(* (f, x, fBody, fDeclEnv) *)
	| Tuple of value sequence   														(* Heterogeneous fixed-length tuple of values*)
	| ListV of value sequence   														(* Homogeneous list of values *)
;;