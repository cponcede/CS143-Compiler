class C {
	a : Int;
	b : Bool;

	fun(y : Int, x : Bool) : C {
		{
			y <- ~y;
			y <- ~false;
			y <- ~"hello";

			x <- 1 = 2;
			x <- 3 = "hello";
			x <- (4 + true) = (3 + "string");

			x <- x < y;
			x <- x < x;
			x <- "hi" < x;

			x <- not x;
			x <- not y;
			x <- not "hi";
			self;
		}
	};

	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

Class Main {
	main():C {
	  (new C).init(1,true)
	};
};