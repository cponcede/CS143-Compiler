(* Test takes advantage of dispatch to void to debug *)
class Main {
	a: A;
	main(): Int {
		{
			if 1 = 1 then a.fun() else 3 fi; -- change condition from true to false to test both sides
			4;
		}
	};
};


class A {
	b : Int;
	fun() : Int {b};
};