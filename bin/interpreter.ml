(** Interpreter for the language. *)

open Syntax;;	(* ADT, EVT, types and env *)
open Utils;;	(* Facilities and auxiliary functions *)
open Eval_ops;;
open Exceptions;;

(**
	Interpreter for expression. 
	Given a located_exp {e} and an enviroment {env} that closes {e},
	this function evaluates {e} and returns the result of the computation.
	Note: this function implements the big-step operational semantics with environment.
  Note: type annotations are here ignored: they are already checked by the type checker.
 *)
let rec eval (e : located_exp) (env : value env) : value = match e.value with
	| CstI i -> Int i
	| CstB b -> Bool b
	| CstF f -> Float f
	| CstC c -> Char c
	| CstS s -> String s
	|	Neg x	 -> 
		(match eval x env with 
		| Int n -> Int (-n)
		| Float n -> Float (-.n)
		| _ as c -> raise (Type_system_Failed("eval:Neg of "^(string_of_value c)
                ^" at Token: "^(string_of_loc (e.loc) ) ) ) )
  |	Not x	 ->
		(match eval x env with 
		| Bool n -> Bool (not n)
		| _ as c -> raise (Type_system_Failed("eval:Not of "^(string_of_value c)
                ^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Var x  -> lookup env x
	| Prim(e1, op, e2) -> 
    let v1 = eval e1 env in 
    let v2 = eval e2 env in 
    (try eval_op v1 op v2 with |_ -> raise(Unsupported_Primitive("eval:Prim of "^op
                                    ^" at Token: "^(string_of_loc (e.loc) ) ) ))
	| Let(x, _, eRhs, letBody) ->
		let xVal = eval eRhs env in
		let letEnv = (x, xVal) :: env in
		eval letBody letEnv
	| If(e1, e2, e3) ->
		let evaluated = eval e1 env in 
		(match evaluated with
		| Bool true -> eval e2 env
		| Bool false -> eval e3 env
		| _     ->  raise (Type_system_Failed("eval:If non-bool guard - "
                ^(string_of_value evaluated)^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Fun(f, x, _, fBody) -> Closure(f, x, fBody, env)
	| Call(eFun, eArg) ->
		let fClosure = eval eFun env in
		(match fClosure with
		| Closure (f, x, fBody, fDeclEnv) ->
			let xVal = eval eArg env in
			let fBodyEnv = (x, xVal) :: (f, fClosure) :: fDeclEnv
			in eval fBody fBodyEnv
		| _ ->  raise (Type_system_Failed("eval:Call: a function was expected! "
            ^(string_of_value fClosure)^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Tup(tuple) -> 
		let evaluateTuple t = 
			let rec f t acc = match t with
				| [] -> Tuple(List.rev acc)
				| x::xs -> f xs (eval x env::acc)
			in f t []
		in evaluateTuple tuple
  | Proj(t,i) -> 
    let tuple = eval t env in 
    let index = eval i env in 
    (match tuple, index with 
    | Tuple(t), Int n -> get t n
    | _, _ -> raise (Type_system_Failed("eval:Proj a tuple and an integer was expected - "
    ^(string_of_value tuple)^" - "^(string_of_value index)^" at Token: "^(string_of_loc (e.loc) ) ) ))
	| Lst(list) -> 
		let evaluateList l = 
			let rec f l acc = match l with
				| [] -> ListV(List.rev acc)
				| x::xs -> f xs (eval x env::acc)
			in f l []
		in evaluateList list
	| Cons_op(e, l) ->
		let v1 = eval e env in 
		let v2 = eval l env in
		(match v1, v2 with
		| _,  ListV([])   ->  ListV([v1])
		| x1, ListV(x2::xs) ->  ListV(x1::x2::xs)
		| _,_ ->  raise (Type_system_Failed("eval:cons a list was expected - "^(string_of_value v1)
              ^" - "^(string_of_value v2)^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Head(l) ->
		let list = eval l env in 
		(match list with
		| ListV(x::_) -> x
		| _ ->  raise (Type_system_Failed("eval:Head - "^(string_of_value list)
            ^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Tail(l) -> let list = eval l env in 
		(match list with
		| ListV(_::xs) -> ListV(xs)
		| _ ->  raise (Type_system_Failed("eval:Tail - "^(string_of_value list)
            ^" at Token: "^(string_of_loc (e.loc) ) ) ) )
;;