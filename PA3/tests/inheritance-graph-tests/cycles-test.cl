class C inherits B{
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

class A inherits C {
	c : Int;
};

class B inherits A {
	d :  Int;
};

Class Main {
	main():C {
	  (new C).init(1,true)
	};
};
