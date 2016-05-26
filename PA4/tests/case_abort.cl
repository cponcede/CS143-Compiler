class Main {
  d : Int <- 10;
  a : A;
  x : Int <- 0;
  true_bool : Bool <- true;

  main():Int { {
    case d + 1 of
      y : String => a.fun();
      b : Bool => a.fun();
    esac;
  }};

};

class A {
  b : Int;
  fun() : Int {b};
};