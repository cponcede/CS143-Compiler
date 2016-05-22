
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main {
  main():Int { 0 };
};

class A {
	a : Int <- 5;
	b : Int;
	c : Int;

	fun():Bool { true };
};

class B inherits A {
	d : String;
	e : Int;
};

class C {
	help : Int <- 5;
	me : Int <- 4;
};