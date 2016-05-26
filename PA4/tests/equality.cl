class Main {
	a: Int <- 4;
	b: Bool;
	c: Bool;
	d: Int <- 5;

	main(): Int {
		{
			b <- a < d;
			c <- a <= d;
			b <- b = c;
			5;
		}
	};
};