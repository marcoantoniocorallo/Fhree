(** Control-Flow-Analysis goals:
  * - Determine for each sub-expression e, a set of function F such that 
  *   e may evaluates to at run-time
  * - Determine where the flow of control may be transferred when e is a function application
  *)

  open Syntax
  
  (** Type representing the label associated to each sub-expression *)
  type label = int
  [@@deriving show, eq, ord]
  
  (**
   *  To make our analysis precise, we usually assume that bound variables
   *  are distinct. To achieve that we represent a variable as an identifier and a unique 
   *  label. Thus, the pair (name, id) are unique inside the expression to analyze. 
   *  Actually, id is the declaration site, i.e., the label of the program point where 
   *  the variable is declared. 
   *  For example, {name="x";id=5} denotes the variables x declared in the program point 5.
   *)
  type var = { name : string ; id : label }
  [@@deriving show, eq, ord]
  
  (** An annotated expression is made of a term and of a unique label *)
  type aexpr = { t : term ; l : label }
  [@@deriving show]
  
  (* Redefine the syntax for carrying out an annotated expression rather than a node of AST *)
  and term =
    | EmptyProgram
    | CstI of int
    | CstB of bool
    | CstF of float															 			
    | CstC of char															 			
    | CstS of string														 			
    | Uop of ide * aexpr
    | Bop of string * aexpr * aexpr
    | Var of var
    | Let of var * aexpr * aexpr
    | If of aexpr * aexpr * aexpr
    | Fun of var * var * aexpr
    | Lambda of var * aexpr     
    | Call of aexpr * aexpr
    | Tup of aexpr list			 				 	 			
    | Proj of aexpr * aexpr               
    | Lst of aexpr list 		 					 			
    | Cons_op of aexpr * aexpr						
    | Head of aexpr															
    | Tail of aexpr				
    | IsEmpty of aexpr
  [@@deriving show]
  
  (** [to_aexpr e] returns an annotated version of e. *)
  let to_aexpr (e : Syntax.located_exp) : aexpr =
    let counter = ref(0) in                           (* counter to generated unique labels *)
    let next_label () = incr counter; !counter in     (* returns a new label *)
    let mk_aexpr t = { t = t ; l = next_label ()} in  (* build an annotated expression from a term *)
  
    (* tail recursive helper function to carry out the annotation. 
     * env is an enrivonment storing for each variable the corresponding unique id.
     *)
    let rec transform env (exp : Syntax.located_exp) = match exp.value with
    | Syntax.EmptyProgram -> EmptyProgram |> mk_aexpr
    | Syntax.CstI(i) -> CstI(i) |> mk_aexpr
    | Syntax.CstB(b) -> CstB(b) |> mk_aexpr
    | Syntax.CstC(i) -> CstC(i) |> mk_aexpr
    | Syntax.CstF(i) -> CstF(i) |> mk_aexpr
    | Syntax.CstS(i) -> CstS(i) |> mk_aexpr
    | Syntax.Uop(op, x) -> Uop(op, transform env x) |> mk_aexpr
    (* looks in the environment for the program point in which has been defined id *)
    | Syntax.Var(id) -> Var({name = id; id = Syntax.lookup env id }) |> mk_aexpr
    | Syntax.Let(x, _, e1, e2) ->
      let ae1 = transform env e1 in
      let var_id = next_label() in
      let ae2 = transform ((x, var_id)::env) e2 in
      Let({name = x; id = var_id}, ae1, ae2) |> mk_aexpr
    | Syntax.Bop(e1, op, e2) ->
      let ae1 = transform env e1 in
      let ae2 = transform env e2 in
      Bop(op, ae1, ae2) |> mk_aexpr
    | Syntax.If(e1, e2, e3) ->
      let ae1 = transform env e1 in
      let ae2 = transform env e2 in
      let ae3 = transform env e3 in
      If(ae1, ae2, ae3) |> mk_aexpr
    | Syntax.Fun(f, x, _, body) ->
      let fvar = { name = f; id = next_label () } in
      let xvar = { name = x; id = next_label () } in
      let abody = transform ((fvar.name, fvar.id)::(xvar.name, xvar.id)::env) body in
      Fun(fvar, xvar, abody) |> mk_aexpr
    | Syntax.Call(e1, e2) ->
      let ae1 = transform env e1 in
      let ae2 = transform env e2 in
      Call(ae1, ae2) |> mk_aexpr
    | Syntax.Tup(t) ->
      let rec f t acc = match t with
      | [] -> Tup(List.rev acc)
      | x::xs -> f xs (transform env x::acc)
      in f t [] |> mk_aexpr
    | Syntax.Proj(t,i) -> 
      let at = transform env t in 
      let ai = transform env i in 
      Proj(at,ai) |> mk_aexpr
    | Syntax.Lst(l) -> 
      let rec f t acc = match t with
      | [] -> Lst(List.rev acc)
      | x::xs -> f xs (transform env x::acc)
      in f l [] |> mk_aexpr
    | Syntax.Cons_op(e,l) -> 
      let ae = transform env e in 
      let al = transform env l in 
      Cons_op(ae,al) |> mk_aexpr
    | Syntax.Head(e) -> 
      let ae = transform env e in
      Head(ae) |> mk_aexpr
    | Syntax.Tail(e) ->
      let ae = transform env e in
      Tail(ae) |> mk_aexpr
    | Syntax.IsEmpty(e) ->
      let ae = transform env e in 
      IsEmpty(ae) |> mk_aexpr
    in transform [] e
  
  (** The solver is parametric on the type of variables. 
   *  The module Var defines a type t that express both cache entries and env entries.
   *)
  module Var =
    struct
      type t = 
             (* IdVar(v) is a variable representing the entry in the environment for the identifier v *)
             | IdVar of var [@printer fun fmt v -> Format.fprintf fmt "r(%s:%d)" v.name v.id]
             (* CacheVar(l) is a variable representing the entry in the cache for the label l *)
             | CacheVar of label [@printer fun fmt c -> Format.fprintf fmt "C(%d)" c]
             (* ReachVar(l) is a variable representing the entry in the reachability table for the label l *)
             | ReachVar of label [@printer fun fmt c -> Format.fprintf fmt "R(%d)" c]
        [@@deriving show{ with_path = false }, eq, ord]
    end
  
  (** The solver is parametric on the type of tokens. 
   *  The module Token defines a type t that represent the name and the definition site of a function.
   *)
  module Token =
  struct
    type t = var
      [@@deriving show, eq, ord]
  end
  
  (** Solver is an instantiation of the CFA solver for the language fun *)
  module Solver = Cfa_solver.Make(Token)(Var)
  
  (** Lambda star: returns a list of the all function definition occurring in e *)
  let lambdas e =
    let rec f_aux e acc =
      match e.t with
      | EmptyProgram
      | CstI(_)
      | CstB(_)
      | CstC(_)
      | CstF(_)
      | CstS(_)
      | Var(_)   -> acc
      | Uop(_, e) -> f_aux e acc
      | Let(_, e1, e2)
      | Bop(_, e1, e2)
      | Call(e1, e2) -> acc |> f_aux e1 |> f_aux e2
      | If(e1, e2, e3) ->  acc |> f_aux e1 |> f_aux e2 |> f_aux e3
      | Fun(f, x, body) ->
        (f,x,body)::acc |> f_aux body
      | Lambda(x, body) -> ({name = ""; id = (e.l);},x,body)::acc |> f_aux body
      | Tup(t)
      | Lst(t) -> 
        let rec f t acc = match t with
        | [] -> acc
        | x::xs -> f xs (f_aux x acc)
        in f t acc
      | Proj(t,_) -> f_aux t acc
      | Cons_op(e,l) -> f_aux l acc |> f_aux e
      | Head(l)
      | Tail(l) 
      | IsEmpty(l) -> f_aux l acc
    in f_aux e []

  (** Constraint generator: returns the CFA constraints for the annotated expression ae. *)
  let constrs_of_aexpr aexp = 
    let reachable = {name = "on"; id = 0} in (* dummy token {on} for the abstract reachability *) 
    let lambda_star = lambdas aexp in
    let open Solver in
    let rec f_aux e acc =
      match e.t with
      | EmptyProgram
      | CstI(_)
      | CstB(_)
      | CstF(_)
      | CstC(_)
      | CstS(_) -> acc
      | Uop(_,e) -> f_aux e acc
      | Var(v) -> (IdVar(v) @< CacheVar(e.l)):: acc
      | Bop(_, e1, e2) -> acc |> f_aux e1 |> f_aux e2
      | Let(x, e1, e2) ->
        let acc' = (CacheVar(e1.l) @< IdVar(x)) :: (CacheVar(e2.l) @< CacheVar(e.l)) :: acc in
        acc' |> f_aux e1 |> f_aux e2
      | If(e1, e2, e3) ->
        let acc' = (CacheVar(e2.l) @< CacheVar(e.l)) :: (CacheVar(e3.l) @< CacheVar(e.l)) :: acc in
        acc' |> f_aux e1 |> f_aux e2 |> f_aux e3
      | Fun(f, _, body) -> 
        let acc' = 
            ( [f] @^ IdVar(f) ) :: acc in 
            (* maps each constraint cc in body to a constraint ( {on} < R(body.label) ==> cc )*)
            (addHyp ([reachable] @^ ReachVar(body.l)) (f_aux body []))@acc'
      | Lambda(_, body) -> 
        let acc' = 
          ( [{name = ""; id = (e.l);}] @^ CacheVar(e.l) ) :: acc in 
          (addHyp ([reachable] @^ ReachVar(body.l)) (f_aux body []))@acc'
          |> f_aux body
      | Call(e1, e2) ->
        let acc' = List.fold_left (fun cs (f,x,e') ->
            (* for each fun: if fun in e1.label ==> {on} < R(body.label) *)
            ([([f] @^ CacheVar(e1.l))] @~~> ([reachable] @^ ReachVar(e'.l))) ::
            ([([f] @^ CacheVar(e1.l))] @~~> (CacheVar(e2.l) @< IdVar(x))) ::
            ([([f] @^ CacheVar(e1.l))] @~~> (CacheVar(e'.l) @< CacheVar(e.l))) :: cs
          ) acc lambda_star in
          acc' |> f_aux e1 |> f_aux e2
      | Tup(t) 
      | Lst(t) -> 
        let rec f t acc = match t with
        | [] -> acc
        | x::xs -> f xs (f_aux x acc)
        in f t acc
      | Proj(t,_) -> f_aux t acc
      | Cons_op(e,l) -> f_aux l acc |> f_aux e
      | Head(l)
      | Tail(l) 
      | IsEmpty(l) -> f_aux l acc
    in f_aux aexp []