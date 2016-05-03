#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <string>
#include <unordered_map>
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


class GraphNode{
private:
	bool inherits;
	std::string class_name;
	std::string class_inherited_from;
public:
	GraphNode(std::string class_name, std::string class_inherited_from);
	std::string get_class_name();
	std::string get_class_inherited_from();
};

class ClassGraph {
public:
	ClassGraph();
	void add_class(GraphNode&);
	bool has_cycles();
	bool all_defined();
private:
	std::unordered_map <std::string, std::string> node_map;
};

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  bool has_cycles();
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};




#endif

