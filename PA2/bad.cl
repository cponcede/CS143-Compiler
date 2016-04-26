
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

(* Bad Let Tests *)
class F {
ana(): Int {
	(let x:Int <- 1, y:Int <-, z:Int in 2)+3
};
};

class G {
ana(): Int {
	(let x:Int <- 1, y:In, z:Int in 2)+3
};
};

class H {
ana(): Int {
	(let x:Int <- 1, y:Int <-3, z:Int in 2)+3
};
};

(* Should skip just bad expression in block *)
class I {
ana(): Int {
	var_a <- 0;
	kjhgukyauisdhah;
	var_b <- 1;
};
};

(* error:  closing brace is missing *)
Class E inherits A {
;