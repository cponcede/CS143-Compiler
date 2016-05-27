class Main {
	io : IO <- new IO;
	a : A;
	test_a: A;
	s : String;
	main(): Int {
		{
			io.out_string("try to make B\n");
			a <- new B;
			io.out_string("made a new B\n");
			test_a <- a.get_a();
			io.out_string("got a\n");
			s <- test_a.fun();
			io.out_string("got the string\n");
			io.out_string(s);
			5;
		}
	};
};

class A {
	my_a: A <- new SELF_TYPE;
	s: String;

	get_a(): A {my_a};

	fun(): String {
		{
			"fun";
			"times";
		}
	};
};

class B inherits A {
	b: Int;
	c: Int;
	fun(): String {
		{
			"times";
			"fun";
		}
	};
};