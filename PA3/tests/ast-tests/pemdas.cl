class C {
	a : Int;
	b : Bool;
	
	pemdas_fun (): Int {
		{
			let c : Int in {
				c <- "String" + 1;
				c <- 5 + false;
				c <- 3 + 3;

				c <- "String" - 1;
				c <- 5 - false;
				c <- 3 - 3;

				c <- "String" * 1;
				c <- 5 * false;
				c <- 3 * 3;

				c <- "String" / 1;
				c <- 5 / false;
				c <- 3 / 3;

				c <- (1 + 4) * 3;
			};
			
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
