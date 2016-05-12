(* This is a simple test of proper and improper inheritence *)

class A {
	a : Int;
	b : Bool;
	a(x:Int, y:Int, z:Int):Int { 
		z <- 4
	};

	c (x:Int, y:Int):Bool {
		true
	};
};

class B inherits A {
	b : Int;
	a (x:Int, y:Int):Int {
		y
	};
};

class C inherits B {
	x_attr : Int;
	y_attr : Int;
	test():Bool {
		{
		x <- 3;
		y <- 4;
		c(x,y) = true;
		}
	};
};