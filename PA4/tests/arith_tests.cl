class Main {
	x : Int <- 10;
	y: Int <- 0;
	a: A;
	main(): Int {
		{
			y <- x + y;
			x <- x * x;
			x <- x / y;
			y <- y - 1;
			let a : Int <- 0 in x <- a;
			if (x < 0) then a.fun() else 4 fi;
			if y = 9 then 5 else a.fun() fi;
		}
	};
};

class A {
	b : Int;
	fun() : Int {b};
};