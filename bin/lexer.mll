(** Lexer for language FUN
 * Design choice:
 * -A float f is recognized iff Stdlib.float_of_string recognizes f. That is:
 *  "The format of decimal floating-point numbers is [-] dd.ddd (e|E) [+|-] dd , 
 *  where d stands for a decimal digit.
 *  :
 *  In both cases, at least one of the integer and fractional parts must be given; 
 *  the exponent part is optional."
 * -Keywords are stored in a dictionary;
 * -There are both C-like single-line comments, and Ocaml-like comments ( // and (*...*) )
 * -The syntax of tuples and lists differ in the type of the brackets, 
    not in the separator of elements:
    ("this", "is", "a", "tuple")
    ["this", "is", "a", "list"]
 *)

(** Header: define tokens, keywords and utilities for strings *)
{
	open Exceptions
  open Parser
  open Utils

	let create_hashtable size init =
		let tbl = Hashtbl.create size in
		List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
		tbl

	let keyword_table =
		create_hashtable 22 [
			("if", 	IF);
			("then",THEN);
			("else",ELSE);
			("let",	LET);
			("in",	IN);
			("fun",	FUN);
      ("lambda",LAMBDA);
			("!", NOT);
      ("&&", AND);
      ("||", OR);
      ("not", NOT);
      ("and", AND);
      ("or", OR);
	  	("proj", PROJ);
	  	("hd", HEAD);
		  ("tl", TAIL);
      ("is_empty", IS_EMPTY);
      ("int", TINT);
      ("char", TCHAR);
      ("float", TFLOAT);
      ("bool", TBOOL);
      ("string", TSTRING);
      ("list", TLIST)
		]

}

(** Definition section *)
let digit 	= ['0'-'9']
let integer = digit+ 
let sign    = ('+' | '-')
let float 	= digit+ '.'? digit* (('e'|'E') sign? digit+)? |
							digit* '.'? digit+ (('e'|'E') sign? digit+)?
let bool 		= ("true"|"false")
let char		= "'" [^ '''] "'"
let string  = "\"" [^ '"']* "\""
let id 			= ['a'-'z' 'A'-'Z' '_']['a'-'z' '0'-'9' '_']*
let white   = [' ' '\t']

rule tokenize = parse
	| integer as inum		{ INT(int_of_string inum)}
  | float as fnum 		{ FLOAT(float_of_string fnum) }
	| bool as b					{ BOOL (bool_of_string b) }
	| char as c					{ CHAR (c.[1]) }
	| string as s				{ STRING (String.sub s 1 ((String.length s)-2)) }
	| id as word        {
												try Hashtbl.find keyword_table word
												with Not_found -> ID word
											}
  | ','               { COMMA }
  | '^'               { CONCAT }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIV }
  | "+."              { FPLUS }
  | "-."              { FMINUS }
  | "*."              { FTIMES }
  | "/."              { FDIV }
  | '='               { EQ }
  | "<>"              { NEQ }
  | '<'               { LESS }
  | '>'               { GREATER }
  | "<="              { LEQ }
  | ">="              { GEQ }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '['               { LBRACKET }
  | ']'               { RBRACKET }
  | "::"              { CONS_OP }
  | ":"               { COLON }
  | "->"              { ARROW }
  | "|>"              { PIPE }
	| "(*"							{ comments 0 lexbuf }
  | "//" [^ '\n']*    (* eat up one-line comments *)
  | white             (* eat up whitespace *)
											{ tokenize lexbuf }
  | '\n'              { Lexing.new_line lexbuf; tokenize lexbuf }
  | eof               { EOF }
	| _ 			          { raise (Lexing_Error("Unexpected character: "^(Lexing.lexeme lexbuf)^" at "^
												(string_of_position (Lexing.lexeme_start_p lexbuf))))
											}

and comments level = parse
	| "*)"  		        { if level = 0 then tokenize lexbuf else comments (level-1) lexbuf }
  | "(*"    	        { comments (level+1) lexbuf }
	|'\n'      	        { Lexing.new_line lexbuf; comments level lexbuf }
  | _					        { comments level lexbuf }
  | eof               { raise (Lexing_Error ("Non-closed comment !!!")) }
