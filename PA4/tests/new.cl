
class Main {
	a : A;
	x : Int;
	main(): Int {
		{
			a <- new A;
			x <- a.fun();
		}
	};
};

class A {
	b : Int <- 10;
	fun() : Int {b};
};