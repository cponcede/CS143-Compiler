#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <string>
#include <map>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  void add_class(Class_);
	Symbol inherits_from(Symbol);
	bool is_present_once(Symbol);
  bool check_symbol_for_cycles(Symbol);
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  bool has_cycles();
  bool all_defined();

  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  std::map <Symbol, Symbol> inheritance_map;
};




#endif

