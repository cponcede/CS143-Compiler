class Main {
	a: A;

	check_argument_passing(x: Int, y: Int) : Int {
		if (y = 5) then a.fun() else 3 fi -- change to six or seven to see if it fails
	};

	main(): Int {
		{	
			check_argument_passing(6, 5);
		}
	};
};

class A {
	b : Int;
	fun() : Int {b};
};