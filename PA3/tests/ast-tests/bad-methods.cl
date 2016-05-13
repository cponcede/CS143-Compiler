class A {
	self:Int;
	x(self:Int):Bool {
		true
	};
};

class Main {
	a : Int <- new A;
	main():Bool {
		let self:Int in true
	};
};

class B inherits A {

};