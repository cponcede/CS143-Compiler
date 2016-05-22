Class Main {
	self : Int;
	a : A;
	main():A { 
	  a <- new A

	};
};

Class A {
	a : Int;
	x(x : Int, y: Int):Object {
		a <- true
	};
};

Class B inherits A {
	x(x: Int, y : Bool) : Object {
		true
	};
};

Class C {
	test() : Int {
		true
	};
};