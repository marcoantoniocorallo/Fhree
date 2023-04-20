(** The solver is parametric on the type of constraint variables and on the type of tokens. 
 *  The module type OrderedType represent the requirement we asked 
 *  on the type implementing constraint variables and tokens.
*)
module type OrderedType = sig
  type t
    [@@deriving show,ord]
end

(** The functor that allow us to create a solver.
 *  Constraint variables denote set of tokens. 
 *  A solution is an assignment from variables to set of tokens.
 *)
module Make(Token : OrderedType)(Var : OrderedType) =
struct

  (** The constraints the solver is able to handle *)
  type t = 
  | IsIn of Token.t list * Var.t   (* IsIn(t,v) means {t} ⊆ v *)
  | SubSeteq of Var.t * Var.t (* Subseteq(v1, v2) means v1 ⊆ v2 *)
  | Impl of t list * t 
    (*  Impl(ts,t,v) means ts ⟼ t ⊆ v 
     *  where ts is like {t1,...,tn}1 ⊆ v1 ^ {t1,...,tn}2 ⊆ v2 ^ ... ^ {t1,...,tn}n ⊆ vn 
     *  and t is either a set of tokens or a variable 
     *  [], T(t), v   = t ⊆ v   = IsIn
     *  [], V(v1), v2 = v1 ⊆ v2 = SubSeteq
     *  (ti,vi), t, v = ti ⊆ vi => t ⊆ v *) 
  [@@deriving show { with_path = false },ord]

  (** A pretty printer *)
  let rec pp fmt v = match v with
    | IsIn(t, v) ->
      Format.fprintf fmt "{ %s } ⊆ %s" 
      (List.map (fun x -> Token.show x) t |> String.concat "^") (Var.show v)
    | SubSeteq(v1, v2) ->
      Format.fprintf fmt "%s ⊆ %s" (Var.show v1) (Var.show v2)
    | Impl(hyp,concl) ->
      List.iter (pp fmt) hyp; Format.fprintf fmt " ⟼ "; pp fmt concl

  (** A pretty printer *)
  let show v =
    pp (Format.str_formatter) v;
    Format.flush_str_formatter ()

  (** [t @^ v] builds the constraint  {t} ⊆ v *)
  let (@^) token var = IsIn(token, var)

  (** [v1 @< v2] builds the constraint v1 ⊆ v2 *)
  let (@<) var1 var2 = SubSeteq(var1, var2)

  (** [(t @^ v) @~~> (v1 @< v2)] builds the constraint {t} ⊆ v ==> v1 ⊆ v2 *)
  let (@~~>) hyp concl = match (hyp, concl) with
    |(l, SubSeteq(_,_)) 
    |(l, IsIn(_,_)) -> 
      if List.for_all (function |IsIn(_,_)|SubSeteq(_,_)->true |_->false) l 
      then Impl(hyp,concl) 
      else failwith ("Malformed constraint: "
      ^(String.concat " ^ " (List.map show hyp))^" ==> "^(show concl))
    | _ -> failwith ("Malformed constraint"
      ^(String.concat " ^ " (List.map show hyp))^" ==> "^(show concl))

  (** addHyp hyp cc adds the hypothesis hyp to each constraint in cc *)
  let addHyp hyp cc = 
    let f c = match c with
    | IsIn(_,_)
    | SubSeteq(_,_) -> [hyp] @~~> c 
    | Impl(l, concl) -> (hyp::l) @~~> concl
    in List.map f cc

  (** A set of tokens used inside the solve function *)
  module TokSet = Set.Make(Token)

  (** Worklist algorithm: [solve cc] returns the solution of the constraints cc. *)
  let solve constraints : (Var.t * Token.t Seq.t) Seq.t =
    let worklist = Queue.create () in          (* W *)
    let data_field = Hashtbl.create (List.length constraints) in  (* D *)
    let edge_cc = Hashtbl.create (List.length constraints) in     (* E *)

    (* The add sub-procedure *)
    let add p d =
      let d = Option.value d ~default:TokSet.empty in
      let dp = Hashtbl.find_opt data_field p |> Option.value ~default:TokSet.empty in
      if not (TokSet.subset d dp) then
        begin
        Hashtbl.replace data_field p (TokSet.union d dp);
        Queue.push p worklist
        end
    in

    (** Step 2: store in E[p] the constraints affected by p *)
    constraints |> List.iter (function
        | IsIn(tk, p) -> add p (Some (TokSet.of_list tk))
        | SubSeteq(p1, p2) as cc -> Hashtbl.add edge_cc p1 cc
        | Impl(hyp, concl) as cc -> 
          List.iter (
            function 
            | IsIn(tk,p) -> Hashtbl.add edge_cc p cc
            (* add the constraints in p1, in order to satisfy the hyp in the fixpoint step *)
            | SubSeteq(p1,p2) -> Hashtbl.add edge_cc p1 cc 
            |_ -> assert false
          ) hyp;
          match concl with
          | IsIn(tk,p) -> Hashtbl.add edge_cc p cc
          | SubSeteq(p1,p2) -> Hashtbl.add edge_cc p1 cc
          | _ -> failwith "The conclusion of the constraint must be either IsIn or SubSeteq."
      ) ;

    (** Step 3: fixpoint iteration. 
     *  Propagate the information about tokens until no further changes occur. 
     *)  
    while not(Queue.is_empty worklist) do
      let q = Queue.pop worklist in
      let cc = Hashtbl.find_all edge_cc q in
      (* Check if the and clauses of the hypoteses are satisfied *)
      let check_hyp = (function 
          | IsIn(tok,p) -> 
            (match Hashtbl.find_opt data_field p with 
            | Some(dp) -> List.for_all (fun x -> TokSet.mem x dp) tok
            | _ -> false )
          | SubSeteq(p1,p2) -> 
            (match (Hashtbl.find_opt data_field p1, Hashtbl.find_opt data_field p2) with 
            | (Some(dp1),Some(dp2)) -> TokSet.subset dp1 dp2
            | _,_ -> assert false)
          | _ -> assert false) in 
      cc |> List.iter (function
          | SubSeteq(p1, p2) -> add p2 (Hashtbl.find_opt data_field p1)
          (* Check hypotheses, and then add lhs's information to rhs's datafield *)
          | Impl(hyp, IsIn(ts,p)) -> 
            if List.for_all check_hyp hyp then 
              add p (Some (TokSet.of_list ts))
          | Impl(hyp, SubSeteq(p1, p2)) -> 
            if List.for_all check_hyp hyp then 
              add p2 (Hashtbl.find_opt data_field p1)
          | _ -> assert false )
    done;

    (** Step 4: build the final solution *)
    Hashtbl.to_seq data_field |> Seq.map (fun (v, s) -> (v, TokSet.to_seq s))
end