open Syntax;;
open Exceptions;;

let get_int (_ : value) : value = 
	let input = read_line() in 
	(match int_of_string_opt input with
	| Some i -> Int i
	| None -> raise (IO_Type_Error("get_int: parsed non-int value"))
	)
;;

let get_float (_ : value) : value = 
	let input = read_line() in 
	(match float_of_string_opt input with
	| Some i -> Float i
	| None -> raise (IO_Type_Error("get_float: parsed non-float value"))
	)
;;

let get_bool (_ : value) : value = 
	let input = read_line() in 
	(match bool_of_string_opt input with
	| Some i -> Bool i
	| None -> raise (IO_Type_Error("get_bool: parsed non-bool value"))
	)
;;

let get_char (_ : value) : value = 
	let input = read_line() in 
	(match String.length input with
	| 1 -> Char (input.[0])
	| _ -> raise (IO_Type_Error("get_char: parsed non-char value"))
	)
;;

let get_string (_ : value) : value = String (read_line())
;;

let print_int (x : value) : value = 
	(match x with
	| Int s -> print_int s
	| _ -> raise (Exceptions.Type_system_Failed("print_int: int expected"))
	); print_newline();
	Unit
;;

let print_float (x : value) : value = 
	(match x with
	| Float s -> print_float s
	| _ -> raise (Exceptions.Type_system_Failed("print_float: float expected"))
	); print_newline();
	Unit
;;

let print_char (x : value) : value = 
	(match x with
	| Char s -> print_char s
	| _ -> raise (Exceptions.Type_system_Failed("print_char: char expected"))
	); print_newline();
	Unit
;;

let print_bool (x : value) : value = 
	(match x with
	| Bool b -> (print_string (string_of_bool b))
	| _ -> raise (Exceptions.Type_system_Failed("print_bool: bool expected"))
	); print_newline();
	Unit
;;

let print_string (x : value) : value = 
	(match x with
	| String s -> print_string s
	| _ -> raise (Exceptions.Type_system_Failed("print_string: string expected"))
	); print_newline();
	Unit
;;

let get_int = (
	"get_int", 
	Closure(
		"get_int", 
		"",
		{ 
			value = 
			NativeFunction(get_int, None)
			; 
			loc = (Lexing.dummy_pos, Lexing.dummy_pos) 
		},
		[]
	)
);;

let get_float = (
	"get_float", 
	Closure(
		"get_float", 
		"",
		{ 
			value = 
			NativeFunction(get_float, None)
			; 
			loc = (Lexing.dummy_pos, Lexing.dummy_pos) 
		},
		[]
	)
);;

let get_char = (
	"get_char", 
	Closure(
		"get_char", 
		"",
		{ 
			value = 
			NativeFunction(get_char, None)
			; 
			loc = (Lexing.dummy_pos, Lexing.dummy_pos) 
		},
		[]
	)
);;

let get_bool = (
	"get_bool", 
	Closure(
		"get_bool", 
		"",
		{ 
			value = 
			NativeFunction(get_bool, None)
			; 
			loc = (Lexing.dummy_pos, Lexing.dummy_pos) 
		},
		[]
	)
);;

let get_string = (
	"get_string", 
	Closure(
		"get_string", 
		"",
		{ 
			value = 
			NativeFunction(get_string, None)
			; 
			loc = (Lexing.dummy_pos, Lexing.dummy_pos) 
		},
		[]
	)
);;

let print_int = (
	"print_int",
	Closure(
		"print_int", 
		"nf_arg",
		{ 
			value = 
			NativeFunction(print_int, Some "nf_arg")
			; 
			loc = (Lexing.dummy_pos, Lexing.dummy_pos) 
		},
		[]
	)
);;

let print_float = (
	"print_float",
	Closure(
		"print_float", 
		"nf_arg",
		{ 
			value = 
			NativeFunction(print_float, Some "nf_arg")
			; 
			loc = (Lexing.dummy_pos, Lexing.dummy_pos) 
		},
		[]
	)
);;

let print_char = (
	"print_char",
	Closure(
		"print_char", 
		"nf_arg",
		{ 
			value = 
			NativeFunction(print_char, Some "nf_arg")
			; 
			loc = (Lexing.dummy_pos, Lexing.dummy_pos) 
		},
		[]
	)
);;

let print_bool = (
	"print_bool",
	Closure(
		"print_bool", 
		"nf_arg",
		{ 
			value = 
			NativeFunction(print_bool, Some "nf_arg")
			; 
			loc = (Lexing.dummy_pos, Lexing.dummy_pos) 
		},
		[]
	)
);;

let print_string = (
	"print_string",
	Closure(
		"print_string", 
		"nf_arg",
		{ 
			value = 
			NativeFunction(print_string, Some "nf_arg")
			; 
			loc = (Lexing.dummy_pos, Lexing.dummy_pos) 
		},
		[]
	)
);;

let env = 
	get_int :: 
	get_float ::
	get_char ::
	get_bool ::
	get_string ::
	print_int :: 
	print_float ::
	print_char ::
	print_bool ::
	print_string :: []