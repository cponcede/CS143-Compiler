class Main {
	a: A;
	x : Int <- 10;
	main(): Int {
		{
			if isvoid a then 1 else a.fun() fi;
			if isvoid x then a.fun() else 1 fi;
		}
	};
};


class A {
	b : Int;
	fun() : Int {b};
};