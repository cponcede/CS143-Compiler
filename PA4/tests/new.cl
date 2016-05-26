
class Main {
	res : Int <- 22;
	a : A;
	x : Int <- 1;
	a1 : A;
	io : IO;

	main(): Int {
		{
			a <- new A;
			x <- a.fun();
			if x = 10 then 1 else a1.fun() fi;
			io <- new IO;
			io.out_string("I'm working!");
			a.fun();
			io.out_string("TAKE 2");
			a.fun();
			5;
		}
	};
};

class A {
	b : Int <- 10;
	fun() : Int {b};
};