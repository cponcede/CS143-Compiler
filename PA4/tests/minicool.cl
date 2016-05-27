class Main inherits IO {
	str : String;
	io : IO;
	main() : SELF_TYPE {{
		out_string("LINE 5\n");
		str <- (new Object).type_name().substr(4,1);
		out_string("LINE 7\n");
		io <- out_string(str);
		out_string("LINE 9\n");
		io.out_string((isvoid self).type_name().substr(1,3));
		out_string("LINE 11\n");
	}};
};

class A {
	fun(a : Int, b : Int, c : Int) : String { {
		(new IO).out_string("Entering fun\n");
		(new IO).out_int(a);
		(new IO).out_string("\n");
		(new IO).out_int(b);
		(new IO).out_string("\n");
		(new IO).out_int(c);
		(new IO).out_string("\n");
		if a + b + c < 0 then "NEG\n" else "POS\n" fi;
	}};
};