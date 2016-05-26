class Main {
	d : Int <- 10;
	a : A;
	true_bool : Bool <- true;

  main():Int { {
  	let b : Int <- 1 in
  		let c : Int in {
  			c <- d + b;
  			if not (c = 11) then a.fun() else 1 fi; 
  		};
  	let x : Bool in {
  		x <- true_bool;
  		if x then 0 else a.fun() fi;
  	};

  }};

};

class A {
	b : Int;
	fun() : Int {b};
};