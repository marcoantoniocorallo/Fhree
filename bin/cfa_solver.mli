module type OrderedType = sig
  type t
    [@@deriving show,ord]
end

module Make : functor (Token : OrderedType) (Var : OrderedType) ->
  sig
    type t 
    [@@deriving show { with_path = false },ord]
      
    
    val ( @^ ) : Token.t list -> Var.t -> t
    val ( @< ) : Var.t -> Var.t -> t
    val ( @~~> ) : t list -> t -> t
    
    val addHyp : t -> t list -> t list
    
    val solve : t list -> (Var.t * Token.t Seq.t) Seq.t
  end