(** Some test cases. *)

open Exp;;
open Values;;
open Env;;
open Interpreter;;
open Utils;;

(* Evaluate in empty environment: program must have no free variables: *)
let run e = eval e [];;

(* Examples in abstract syntax *)
let ex1 =	Letfun("f1", "x", Prim(Var "x", "+", CstI 1),
									Call(Var "f1", CstI 12)
			)
;;

(* Factorial *)
let ex2 = 	Let("n", CstI 5,
							Letfun("fac", "x",
								If(
									Prim(Var "x", "=", CstI 0),
									CstI 1,
									Prim(Var "x", "*",
													Call(Var "fac",
															Prim(Var "x", "-", CstI 1)
													)
									)
								),
								Call(Var "fac", Var "n")
							)
			)
;;

let ex3 = Letfun("tw", "g",
						Letfun("app", "x", 
							Call(Var "g", Call(Var "g", Var "x") ),
								Var "app"
						),
					 Letfun("mul3", "y", Prim(CstI 3, "*", Var "y"),
									Call(Call(Var "tw", Var "mul3"), CstI 11)))
;;

let ex4 = Letfun("tw", "g",
					 Letfun("app", "x", Call(Var "g", Call(Var "g", Var "x")),
									Var "app"),
					 Letfun("mul3", "y", Prim(CstI 3, "*", Var "y"),
									Call(Var "tw", Var "mul3")))
;;

(* Factorial(5) *)
(* print_endline (string_of_value	(eval ex2 [("n", Int 5)]));; *)

(* I can specify the argument of factorial, manually binding it in the env! 
 * To do this anywhere, you have to remove the binding <n,5> from the code, beca!
 *)
(* print_endline (string_of_value	(eval ex2 [("n",Int 10)]));; *)

let ex5 = 
	Let
	(
		"tupla",
		Tup
		(
			Item
			(
				CstI 5,
				Item
				(
					CstB true,
					EmptyExp
				)
			)
		),
		Var("tupla")
	)
;;

print_endline (string_of_value (run ex5));;

(* let tupla = (5,10) 
	 tupla(0)	 
*)
let ex6 =	Let
					(
						"tupla",
						Tup(
							Item(
								CstI(5), 
								Item(
									CstI(10), 
									EmptyExp
								)
							)
						),
						Proj(
							Var("tupla"),
							CstI(0)
						)
			)
;;

print_endline (string_of_value (run ex6));; 

(*
	 let x = 2 in 
	 let tupla = (1, x, (let f x = x+20 in f 5 ) ) in
	 tupla(x)
*)
let ex7 = 
	Let
	(
		"x",
		CstI(2),
		Let
		(
			"tupla",
			Tup(
				Item
				(
					CstI(1),
					Item
					(
						Var("x"),
						Item
						(
							Letfun
							(
								"f",
								"x",
								Prim
								(
									Var("x"),
									"+",
									CstI(20)
								),
								Call
								(
									Var("f"),
									CstI(5)
								)
							),
							EmptyExp
						)
					)
				)
			),
			Proj
			(
				Var("tupla"),
				Var("x")
			)
		)
	)
;;

print_endline (string_of_value (run ex7));; 

let ex8 =	Let
					(
						"list",
						Lst(
							Item(
								CstI(1), 
								Item(
									CstI(2),
									Item(
										CstI(3),
										EmptyExp
									)
								)
							)
						),
						Var("list")
					)
;;

print_endline (string_of_value (run ex8));; 

let ex9 =	Let
					(
						"list",
						Lst(
							Item(
								CstI(1), 
								Item(
									CstI(2),
									Item(
										CstI(3),
										EmptyExp
									)
								)
							)
						),
						Cons
						(
							CstI(0),
							Var("list")
						)
					)
;;

print_endline (string_of_value (run ex9));; 

let ex10 =	Let
					(
						"list",
						Lst(
							Item(
								CstI(1), 
								Item(
									CstI(2),
									Item(
										CstI(3),
										EmptyExp
									)
								)
							)
						),
						Cons
						(
							CstB(true),
							Var("list")
						)
					)
;;

(* print_endline (string_of_value (run ex10));; *)

let ex11 =	Let
					(
						"list",
						Lst(
							Item(
								CstI(1), 
								Item(
									CstI(2),
									Item(
										CstI(3),
										EmptyExp
									)
								)
							)
						),
						Head(Var("list"))
					)
;;

print_endline (string_of_value (run ex11));; 

let ex12 =	Let
					(
						"list",
						Lst(
							EmptyExp
						),
						Head(Var("list"))
					)
;;

(* print_endline (string_of_value (run ex12));; *)

let ex13 =	Let
					(
						"list",
						Lst(
							Item(
								CstI(1), 
								Item(
									CstI(2),
									Item(
										CstI(3),
										EmptyExp
									)
								)
							)
						),
						Tail(Var("list"))
					)
;;

print_endline (string_of_value (run ex13));; 

let ex14 =	Let
					(
						"list",
						Lst(
							Item(
								CstI(1), 
								EmptyExp
							)
						),
						Tail(Var("list"))
					)
;;

print_endline (string_of_value (run ex14));; 
