class Main {
	d : Int <- 10;
	io : IO <- new IO;
	x : Int <- 0;
	true_bool : Bool <- true;

  main():Int { {
  	case d of
      a : A => io.out_string("Chose A branch");
  		b : Bool => io.out_string("Chose Bool branch");
  		c : Int => io.out_string("Chose Int branch");
      f: B => io.out_string("Chose B branch");
  	esac;
    5;
  }};

};

class A {

};

class B inherits A {

};

class C inherits B {

};

