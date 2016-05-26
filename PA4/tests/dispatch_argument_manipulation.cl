class Main {
	a: A;

	check_argument_manipulation(x: Int) : Int {
		x <- 8
	};

	main(): Int {
		let x: Int <- 0 in 
		{
			check_argument_manipulation(x);
			if (x = 0) then a.fun() else 9 fi;
		}
	};
};

class A {
	b : Int;
	fun() : Int {b};
};