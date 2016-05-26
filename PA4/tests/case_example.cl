class Main {
	d : Int <- 10;
	a : A;
	x : Int <- 0;
	true_bool : Bool <- true;

  main():Int { {
  	case d + 1 of
  		y : Object => a.fun();
  		b : Bool => a.fun();
  		c : Int => x <- c;
  	esac;
  	if x = 11 then 1 else a.fun() fi;
  }};

};

class A {
	b : Int;
	fun() : Int {b};
};