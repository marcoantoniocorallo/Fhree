%{
    open Syntax
    open Type_system

    (** Infix operator for creating a located exp from an exp and a location *)
    let (|@|) value loc = { value = value; loc = loc }

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
%token FUN LAMBDA
%token LPAREN "(" RPAREN ")"
%token LBRACKET "[" RBRACKET "]"
%token PLUS "+" MINUS "-" TIMES "*" DIV "/" MOD "%"
%token FPLUS "+." FMINUS "-." FTIMES "*." FDIV "/."
%token LESS "<" LEQ "<=" GREATER ">" GEQ ">=" EQ "=" NEQ "<>"
%token AND "&&" OR "||" NOT "!" CONCAT "^"
%token PROJ
%token CONS_OP "::" HEAD "hd" TAIL "tl"
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
| FUN f = ID "(" n = ID ":" t1 = ptype ")" ":" t2 = ptype "=" e1 = expr IN e2 = expr  %prec prec_let
    { Letfun(f, n, Tfun(t1, t2), e1, e2) |@| $loc }
| LAMBDA "(" n = ID ":" t1 = ptype ")" ":" t2 = ptype "->" e = expr  %prec prec_let
    { Lambda(n, Tfun(t1, t2), e) |@| $loc }
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

(** A tuple must have at least two elements  *)
tuple:
| "(" e = expr "," s = sequence ")"
    { Cons(e, s) }

(** A list can be an empty list or a list of any size *)
lst:
| "[" "]"
    { Nil }
| "[" s = sequence "]"
    { s }

sequence:
| e = expr "," s = sequence
    { Cons(e, s) }
| e = expr 
    { Cons(e, Nil) }

(** Syntactical categories for types definition *)
ptype:
  | t = simple_ptype
    { t }
  | "(" t = ptype "*" s = ptype_sequence ")"
    { Ttuple(Cons(t,s)) }
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
    { Cons(t, s) }
  | t = ptype 
    { Cons(t, Nil) }

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
