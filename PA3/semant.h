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
  bool is_present_once(Symbol);
  bool check_symbol_for_cycles(Symbol);
  ostream& error_stream;
  
public:
  ClassTable(Classes);
  Symbol inherits_from(Symbol);
  int errors() { return semant_errors; }
  bool has_cycles();
  bool all_defined();
  bool class_defined(Symbol target);
  
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  std::map <Symbol, Symbol> inheritance_map;
};

class MethodInfo {
private:
  Symbol return_type;
public:
  std::vector<Symbol> argument_types;
  std::vector<Symbol> argument_names;
  MethodInfo (Feature meth);
  Symbol get_return_type ();
  Symbol get_nth_argument_type (int n);
  Symbol get_nth_argument_name (int n);
};

class ClassMethodInfo {
private:
public:
  ClassMethodInfo(Class_);
  Symbol get_method_return_type (Symbol method_name);
  Symbol get_nth_argument_type (Symbol method_name, int n);
  Symbol get_nth_argument_name (Symbol method_name, int n);
  std::map<Symbol, MethodInfo* > method_map;
  std::map<Symbol, Symbol> attribute_map;

};

class MethodTypeEnvironment {
private:
  ostream& error_stream;
  int semant_errors;
  void install_basic_classes ();
  std::map <Symbol, ClassMethodInfo* > environment_map;
public:
  MethodTypeEnvironment (Classes);
  Symbol get_return_type (Symbol class_name, Symbol method_name);
  Symbol get_nth_argument_type (int n, Symbol class_name, Symbol method_name);
  Symbol get_nth_argument_name (int n, Symbol class_name, Symbol method_name);
  bool has_attribute (Symbol class_name, Symbol attr_name);
  std::map<Symbol,Symbol> *get_class_attributes (Symbol class_name);
  void dump_type_environment (void);
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  int errors() { return semant_errors; }
};



#endif

