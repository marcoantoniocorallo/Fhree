%{
    open Syntax

    (** Infix operator for creating a located exp from an exp and a location *)
    let (|@|) value loc = { value = value; loc = loc }

    (* Facilities for computing currying *)
    let fold_types l t_final = 
        List.fold_right (fun t final -> Tfun(t, final) ) l t_final;;

    let fold_lambdas ides types f_body t_final loc = 
      let count = ref (List.length types) in 
      List.fold_right2 (
        fun x t body -> 
          decr count;
          Fun(
            "",
            x,
            Tfun(t, ( fold_types (List.filteri (fun i _ -> i>=(!count) ) (List.tl types) ) t_final ) ), 
            (body|@|loc)
          )
        ) (List.tl ides) (List.tl types) (f_body.value)
    ;;

    let curry l t e loc = 
      let ides, types = List.split l in 
      let first_arg = List.hd ides in
      let types_folded = List.fold_right (fun t final -> Tfun(t, final) ) types t in
      let lambdas_folded = fold_lambdas ides types e t loc in 
      (first_arg, types_folded, lambdas_folded)
    ;;

%}

(** Tokens definition *)
%token <int> INT
%token <char> CHAR
%token <float> FLOAT
%token <bool> BOOL 
%token <string> STRING
%token <string> ID
%token IF THEN ELSE
%token TINT TBOOL TFLOAT TCHAR TSTRING TLIST
%token LET IN 
%token FUN LAMBDA PIPE "|>"
%token LPAREN "(" RPAREN ")"
%token LBRACKET "[" RBRACKET "]"
%token PLUS "+" MINUS "-" TIMES "*" DIV "/" MOD "%"
%token FPLUS "+." FMINUS "-." FTIMES "*." FDIV "/."
%token LESS "<" LEQ "<=" GREATER ">" GEQ ">=" EQ "=" NEQ "<>"
%token AND "&&" OR "||" NOT "!" CONCAT "^"
%token PROJ
%token CONS_OP "::" HEAD "hd" TAIL "tl" MAP "map" IS_EMPTY "is_empty"
%token COMMA "," COLON ":" ARROW "->"
%token EOF

(** 
 * Associativity and precedence of operators 
 * https://v2.ocaml.org/manual/expr.html#ss:precedence-and-associativity
 *)

%nonassoc prec_let (* shifts! *)
%right "->" 
%left "=" "<" ">" "<=" ">=" "<>" "&&" "||"
%left "+" "-" "^" "+." "-."
%left "*" "/" "%" "*." "/."

%start <Syntax.located_exp> main

%%

main:
| e = expr EOF 
    { e }

| EOF
    { EmptyProgram |@| $loc }

expr:
| e = simple_expr
    { e }

| "!" e = simple_expr
    { Not(e) |@| $loc }

| "-" e = simple_expr
    { Not(e) |@| $loc }

| e1 = expr op = binop e2 = expr
    { Prim(e1, op, e2) |@| $loc }

| IF guard = expr THEN e1 = expr ELSE e2 = expr      %prec prec_let
    { If(guard,e1,e2) |@| $loc }

| LET id = ID t = option(preceded(":", ptype)) "=" e1 = expr IN e2 = expr   %prec prec_let
    { Let(id, t, e1, e2) |@| $loc }

| LET FUN f = ID l = nonempty_list(delimited("(",separated_pair(ID, ":", ptype),")")) 
    ":" t_res = ptype "=" e1 = expr IN e2 = expr  %prec prec_let
    { 
      let (first_arg, types_folded, lambdas_folded) = curry l t_res e1 ($loc) in 
      Let(f, Some (types_folded), 
        Fun(f, first_arg, types_folded, (lambdas_folded |@| $loc)) |@| $loc,
        e2
      ) |@| $loc 
    }

| FUN f = ID l = nonempty_list(delimited("(",separated_pair(ID, ":", ptype),")")) 
    ":" t_res = ptype "=" e1 = expr %prec prec_let
    { 
      let (first_arg, types_folded, lambdas_folded) = curry l t_res e1 ($loc) in 
      Fun(f, first_arg, types_folded, (lambdas_folded |@| $loc)) |@| $loc
    }

| LAMBDA l = nonempty_list(delimited("(",separated_pair(ID, ":", ptype),")")) 
    ":" t_res = ptype "->" e = expr  %prec prec_let
    { 
      let (first_arg, types_folded, lambdas_folded) = curry l t_res e ($loc) in 
      Fun("", first_arg, types_folded, (lambdas_folded |@| $loc)) |@| $loc 
    }
    
| f = func
    { f }

| PROJ t = simple_expr i = simple_expr                             
    { Proj(t,i) |@| $loc }

| e = simple_expr "::" l = simple_expr
    { Cons_op(e,l) |@| $loc }

| HEAD l = simple_expr
    { Head(l) |@| $loc }

| TAIL l = simple_expr
    { Tail(l) |@| $loc }

| IS_EMPTY l = simple_expr
    { IsEmpty(l) |@| $loc }

| MAP l = simple_expr
    { IsEmpty(l) |@| $loc }
    
(** simple_expr is a syntactical category used for disambiguing the grammar. *)
simple_expr:
| "(" e = expr ")"
    { e }

| i = INT
    { CstI(i) |@| $loc }

| b = BOOL
    { CstB(b) |@| $loc }

| c = CHAR
    { CstC(c) |@| $loc }

| f = FLOAT
    { CstF(f) |@| $loc }

| s = STRING
    { CstS(s) |@| $loc } 

| id = ID
    { Var(id) |@| $loc }

| t = tuple
    { Tup(t) |@| $loc }

| l = lst 
    { Lst(l) |@| $loc }
    
(** Fun Call and composition *)
(** The arg of the function is a simple_expr, that is an identifier, a literal,
    or a complex expression surrounded by parentheses 
    Note: function application is left associative! *)
func:
| f = simple_expr e = simple_expr
    { Call(f,e) |@| $loc }

| f = func e = simple_expr
    { Call(f,e) |@| $loc }

| e = simple_expr "|>" f = simple_expr
    { Call(f,e) |@| $loc }

| e = func "|>" f = simple_expr
    { Call(f,e) |@| $loc }

(** A tuple must have at least two elements  *)
tuple:
| "(" e = expr "," s = sequence ")"
    { e::s }

(** A list can be an empty list or a list of any size *)
lst:
| "[" "]"
    { [] }

| "[" s = sequence "]"
    { s }

sequence:
| e = expr "," s = sequence
    { e::s }

| e = expr 
    { e::[] }

(** Syntactical categories for types definition *)
ptype:
  | t = simple_ptype
    { t }

  | "(" t = ptype "*" s = ptype_sequence ")"
    { Ttuple(t::s) }

  | t1 = ptype "->" t2 = ptype
    { Tfun(t1, t2) }

(** simple_type is a syntactical category used for disambiguing the grammar. 
 * In particular, it disambiguise the derivation " type -> type list "
 *)
simple_ptype:
  | TINT
    { Tint }

  | TBOOL
    { Tbool }

  | TCHAR
    { Tchar }

  | TFLOAT
    { Tfloat }

  | TSTRING
    { Tstring }

  | t=option(simple_ptype) TLIST
    { Tlist(t) }

  | "(" t=ptype ")"
    { t }

ptype_sequence:
  | t = ptype "*" s = ptype_sequence
    { t::s }

  | t = ptype 
    { t::[] }

%inline binop:
| "+"   { "+" }
| "-"   { "-" }
| "*"   { "*" }
| "/"   { "/" }
| "+."  { "+." }
| "-."  { "-." }
| "*."  { "*." }
| "/."  { "/." }
| "%"   { "%" }
| "<"   { "<" }
| ">"   { ">" }
| "<="  { "<=" }
| ">="  { ">=" }
| "="   { "=" }
| "<>"  { "<>" }
| "^"   { "^" }
| "&&"  { "&&" }
| "||"  { "||" }