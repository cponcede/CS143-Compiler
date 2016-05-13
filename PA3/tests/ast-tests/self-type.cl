class C {
	a : Int;
	b : Bool;
	c : SELF_TYPE;

	fun(x: Int, y : SELF_TYPE, z : Int) : Int {
		{
			5;
		}
	};

	self_type_return(x : Int) : SELF_TYPE {
		{
			(new SELF_TYPE).init(1, true);
			let test : SELF_TYPE in {
				1;
			};

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

(*

Class No_class inherits C {
	a: Int;
};

Class Int inherits X {
	a: Int;
};


Class SELF_TYPE inherits Int {
	a: Int;
};

Class G inherits SELF_TYPE {
	a: Int;
};

*)

Class Main {
	main():C {
	  (new C).init(1,true)
	};
};