(** Type system: type algorithm with a type annotation for the definition of function
 *  the definition of types is in file exp.ml
 *)

open Exceptions
open Syntax
open Utils

(** The type environment.  
 *  It contains the type of primitives operators and of other primitive functions.
 *)
let type_env = [
  "+",  Tfun(Tint, Tfun(Tint, Tint));         (* int -> int -> int *)
  "-",  Tfun(Tint, Tfun(Tint, Tint));         (* int -> int -> int *)
  "/",  Tfun(Tint, Tfun(Tint, Tint));         (* int -> int -> int *)
  "*",  Tfun(Tint, Tfun(Tint, Tint));         (* int -> int -> int *)
  "%",  Tfun(Tint, Tfun(Tint, Tint));         (* int -> int -> int *)
  "+.", Tfun(Tfloat, Tfun(Tfloat, Tfloat));   (* float -> float -> float *)
  "-.", Tfun(Tfloat, Tfun(Tfloat, Tfloat));   (* float -> float -> float *)
  "/.", Tfun(Tfloat, Tfun(Tfloat, Tfloat));   (* float -> float -> float *)
  "*.", Tfun(Tfloat, Tfun(Tfloat, Tfloat));   (* float -> float -> float *)
  "&&", Tfun(Tbool, Tfun(Tbool, Tbool));      (* bool -> bool -> bool *)
  "||", Tfun(Tbool, Tfun(Tbool, Tbool));      (* bool -> bool -> bool *)
  "^",  Tfun(Tstring, Tfun(Tstring, Tstring));(* string -> string -> string *)
]

(** Typing rule in a given type environment gamma *)
let rec type_of (gamma : ttype env) (e : located_exp) : ttype =
  match e.value with
  | CstI(_) -> Tint
  | CstB(_) -> Tbool
  | CstF(_) -> Tfloat
  | CstC(_) -> Tchar
  | CstS(_) -> Tstring
  | Not(x) -> 
    (match type_of gamma x with 
		| Tbool -> Tbool
		| _ -> raise (Type_Error ("Not of bad type - at Token: "^(string_of_loc (e.loc)))) )
  | Neg(x) -> 
    (match type_of gamma x with 
    | Tint -> Tint
    | Tfloat -> Tfloat
    | _ -> raise (Type_Error ("Not of bad type - at Token: "^(string_of_loc (e.loc)))) )
  | Var(x)  -> lookup gamma x
  (* Define equality and comparison for each simple type *)
  | Prim(e1, "=", e2)
  | Prim(e1, "<", e2)
  | Prim(e1, "<=", e2)
  | Prim(e1, ">", e2)
  | Prim(e1, ">=", e2)
  | Prim(e1, "<>", e2) ->
    let t1 = type_of gamma e1 in
    let t2 = type_of gamma e2 in
    (match t1, t2 with
    | Tint, Tint
    | Tchar, Tchar
    | Tfloat, Tfloat
    | Tstring, Tstring
    | Tbool, Tbool -> Tbool
    | Ttuple(_), Ttuple(_)
    | Tlist(_), Tlist(_) ->     raise (Type_Error ("Equality of compound values"
                                ^(string_of_loc (e.loc))))
    | Tfun(_,_), Tfun(_,_) ->   raise (Type_Error ("Equality of functional values"
                                ^(string_of_loc (e.loc))))
    | _, _ -> raise (Type_Error ("Error in the arguments of equality"^(string_of_loc (e.loc)))))
  | Prim(e1, op, e2) ->
    let t1 = type_of gamma e1 in
    let t2 = type_of gamma e2 in
    let top = lookup gamma op in
    (match top with
    | Tfun(t1', Tfun(t2', tr')) ->
      if (t1' = t1 && t2' = t2) then tr'
      else raise (Type_Error ("Error in the arguments of "^op^(string_of_loc (e.loc))))
    | _ ->  raise(Error_of_Inconsistence("Inconsistence in Prim "^op
            ^(string_of_loc (e.loc)))) )
  | Let(x, t, e1, e2) ->
    (match t with 
    | Some tt -> let t1 = type_of gamma e1 in 
      if t1 = tt then type_of ((x,t1)::gamma) e2
      else raise(Type_Error("Bad type annotation at "^(string_of_loc (e.loc))))
    | None -> let t1 = type_of gamma e1 in type_of ((x,t1)::gamma) e2)
  | If(e1, e2, e3) ->
    if (type_of gamma e1) = Tbool then
      let t2 = type_of gamma e2 in
      let t3 = type_of gamma e3 in
      if t2 = t3 then t2 else raise (Type_Error 
        ("\"IF-Rule\": branches have different types - at Token: "^(string_of_loc (e.loc))))
    else
      raise (Type_Error ("\"IF-Rule\": if with no a boolean guard"^(string_of_loc (e.loc))))
    (* x : tx, Γ |- e : te *)
    (* Fun(x,tx, e) -> Tfun(tx, type_of ((x, tx) :: gamma) e) *)
  | Fun(f, x, fun_type, body) ->
    (match fun_type with (Tfun(t1,t2) as t) ->
      let gamma' = (f, t) :: (x, t1) :: gamma in
      if (type_of gamma' body) = t2 then t
      else
      raise (Type_Error("\"TFun-Rule\": Return type does not match"^(string_of_loc (e.loc))))
    |_ -> raise (Type_Error("\"TFun-Rule\": Function type does not match"^(string_of_loc (e.loc)))) )
  | Call(e1, e2) ->
    let t1 = type_of gamma e1 in
    let t2 = type_of gamma e2 in
    (match t1 with
    | Tfun(tx, tr) ->
      if tx = t2 then tr
      else raise (Type_Error("fuctional application: argument type mismatch"^(string_of_loc (e.loc))))
    | _ -> raise (Type_Error("application to a non functional value"^(string_of_loc (e.loc)))))
  | Tup(tuple) ->
    let type_of_tuple t = 
      let rec f t acc = match t with
        | [] -> Ttuple(List.rev acc)
        | x::xs -> f xs ((type_of gamma x::acc))
      in f t []
    in type_of_tuple tuple
  | Proj(tup,i) ->
    let type_of_tuple = type_of gamma tup in 
    let type_of_i = type_of gamma i in 
    (match type_of_tuple, type_of_i with
    | Ttuple(types), Tint -> 
      (match i.value with CstI x -> get types x 
      |_ -> raise(Type_Error("A constant int was expected in projection of tuple! "
            ^" at Token: "^(string_of_loc (e.loc) ))) )
    | Ttuple(_), _ -> raise(Type_Error("An integer was expected in projection of tuple! "
                      ^" at Token: "^(string_of_loc (e.loc) )))
    | _, _ -> raise(Type_Error("A tuple was expected in projection of tuple! "
              ^" at Token: "^(string_of_loc (e.loc) ))) )
  | Lst(list) -> 
    (match list with 
    |[] -> Tlist None 
    |x::_ -> Tlist (Some(type_of gamma x)))
  | Cons_op(e, l) -> (* 'a -> 'a list -> 'a list *)
    let type_of_l = type_of gamma l in 
    let type_of_e = type_of gamma e in 
    (match type_of_l, type_of_e with
    |Tlist(Some t1),t2 -> 
      if t1=t2 then Tfun(type_of_e,Tfun(type_of_l,type_of_l)) 
      else raise(Type_Error("Type error attempting to insert a bad value in a list!"
                            ^(string_of_loc (e.loc))))
    |Tlist(None),_ -> Tfun(type_of_e,Tfun(type_of_l,Tlist(Some type_of_e)))
    |_,_ -> raise(Type_Error("Cons of a non-list value!"^(string_of_loc (e.loc)))) )
  | Head(l) -> (* 'a list -> 'a *)
    let type_of_l = type_of gamma l in 
    (match type_of_l with
    |Tlist(Some t) -> t
    |Tlist(None) -> raise(Type_Error("Type error attempting to pop an element from an empty list!"
                    ^(string_of_loc (e.loc))))
    |_ -> raise(Type_Error("Head of a non-list value!"^(string_of_loc (e.loc)))) )
  | Tail(l) -> (* 'a list -> 'a *)
    let type_of_l = type_of gamma l in 
    (match type_of_l with
    |Tlist(Some t) -> t
    |Tlist(None) -> raise(Type_Error("Type error attempting to tail an empty list!"
                                    ^(string_of_loc (e.loc))))
    |_ -> raise(Type_Error("Tail of a non-list value!"^(string_of_loc (e.loc)))) )

let type_check e = type_of type_env e