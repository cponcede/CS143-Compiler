class Main {
	a: A;

	check_argument_passing(x: Int) : Int {
		if (x = 6) then a.fun() else 3 fi -- change to six or seven to see if it fails
	};

	main(): Int {
		{	
			check_argument_passing(4);
			check_argument_passing(5);
			check_argument_passing(6);
		}
	};
};

class A {
	b : Int;
	fun() : Int {b};
};