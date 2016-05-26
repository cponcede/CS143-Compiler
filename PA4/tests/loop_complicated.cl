class Main {
	a: A;
	y: Int <- 0;
	main(): Int {
		{
			while y < 9 loop -- change 9 to 7 and see if a.fun crashes or not to test
			{
				{
					-- if y = 8 then a.fun() else 3 fi;
					y <- y + 1;
				};
				
			}
			pool;
			5;
		}
	};
};

class A {
	b : Int;
	fun() : Int {b};
};