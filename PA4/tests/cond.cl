class Main {
	a: A;
	main(): Int {
		{
			if 1 = 3 then a.fun() else 3 fi;
			4;
		}
	};
};

class A {
	b : Int;
	fun() : Int {b};
};