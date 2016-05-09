
#include <set>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
arg,
arg2,
Bool,
concat,
cool_abort,
copy,
Int,
in_int,
in_string,
IO,
length,
Main,
main_meth,
No_class,
No_type,
Object,
out_int,
out_string,
prim_slot,
self,
SELF_TYPE,
Str,
str_field,
substr,
type_name,
val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any
  //   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}


/*
 * Adds a class to the current ClassTable with the name class_name and
 * the ancestor inherits_from. Returns whether or not it was successful.
 */
void ClassTable::add_class(Class_ current_class) {
  Symbol class_name = current_class->get_name();
  Symbol inherits_from = current_class->get_parent();
  
  /* Ensure no class is defined more than once. */
  if (this->inheritance_map[class_name]) {
    semant_error(current_class) << "Class with name " << class_name << " is defined multiple times." << endl;
    return;
  }
  /* Ensure no class attempts to inherit from basic classes other than Object. */
  if (inherits_from == Bool || inherits_from == Int || inherits_from == Str) {
    semant_error(current_class) << "Class with name: " << class_name << " attempts to inherit from "
    << "basic class with name: " << inherits_from << endl;
    return;
  }
  this->inheritance_map[class_name] = inherits_from;
  return;
}

Symbol ClassTable::inherits_from(Symbol class_name) {
  return inheritance_map[class_name];
}

/*
 * Something cannot possibly have two associated values, and we already check for
 * double entry, so this just returns true if there is a key in our inheritance_map
 * for the given Symbol.
 */
bool ClassTable::is_present_once(Symbol class_name) {
  return inheritance_map.find(class_name) != inheritance_map.end();
}

bool ClassTable::check_symbol_for_cycles(Symbol class_name) {
  Symbol current_name = class_name;
  std::set <Symbol> seen_set;
  while (current_name != No_class) {
    if (seen_set.find(current_name) != seen_set.end()) {
      semant_error() << "Class with name: " << class_name << " (or an ancestor of it)"
      << " is in an inheritance cycle." << endl;
      return false;
    }
    seen_set.insert(current_name);
    current_name = inheritance_map[current_name];
  }
  
  return true;
}

/*
 * This method will operate under the assumption that all methods are defined.
 * We will check for cycles on only otherwise wellformed class tables.
 */
bool ClassTable::has_cycles() {
  bool cycles_present = true;
  for (std::map<Symbol, Symbol>::iterator it = this->inheritance_map.begin();
       it != this->inheritance_map.end(); ++it) {
    if (!check_symbol_for_cycles(it->first)) cycles_present = false;
  }
  return cycles_present;
}

/*
 * This method looks at a class table and makes sure that all classes mentioned
 * do, in fact, exist. Returns whether or not all classes do exist.
 */
bool ClassTable::all_defined() {
  bool all_defined = true;
  for (std::map<Symbol, Symbol>::iterator it = this->inheritance_map.begin();
       it != this->inheritance_map.end(); ++it) {
    
    /* Continue if Object, since no inheritance happens. */
    if (it->first == Object && it->second == No_class) continue;
    
    if (!is_present_once(it->second)) {
      all_defined = false;
      semant_error() << "Class " << it->first << " inherits from an undefined class " << it->second << endl;
    }
  }
  
  return all_defined;
}


ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
  
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ current_class = classes->nth(i);
    add_class(current_class);
  }
  install_basic_classes();
}

void ClassTable::install_basic_classes() {
  
  // The tree package uses these globals to annotate the classes built below.
  // curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");
  
  // The following demonstrates how to create dummy parse trees to
  // refer to basic Cool classes.  There's no need for method
  // bodies -- these are already built into the runtime system.
  
  // IMPORTANT: The results of the following expressions are
  // stored in local variables.  You will want to do something
  // with those variables at the end of this method to make this
  // code meaningful.
  
  //
  // The Object class has no parent class. Its methods are
  //        abort() : Object    aborts the program
  //        type_name() : Str   returns a string representation of class name
  //        copy() : SELF_TYPE  returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  
  Class_ Object_class =
  class_(Object,
         No_class,
         append_Features(
                         append_Features(
                                         single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                         single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                         single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
         filename);
  
  add_class(Object_class);
  
  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE       writes a string to the output
  //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
  //        in_string() : Str                 reads a string from the input
  //        in_int() : Int                      "   an int     "  "     "
  //
  Class_ IO_class =
  class_(IO,
         Object,
         append_Features(
                         append_Features(
                                         append_Features(
                                                         single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                                                SELF_TYPE, no_expr())),
                                                         single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                                                SELF_TYPE, no_expr()))),
                                         single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                         single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
         filename);
  
  add_class(IO_class);
  
  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  Class_ Int_class =
  class_(Int,
         Object,
         single_Features(attr(val, prim_slot, no_expr())),
         filename);
  
  add_class(Int_class);
  
  //
  // Bool also has only the "val" slot.
  //
  Class_ Bool_class =
  class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
  add_class(Bool_class);
  
  //
  // The class Str has a number of slots and operations:
  //       val                                  the length of the string
  //       str_field                            the string itself
  //       length() : Int                       returns length of the string
  //       concat(arg: Str) : Str               performs string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring selection
  //
  Class_ Str_class =
  class_(Str,
         Object,
         append_Features(
                         append_Features(
                                         append_Features(
                                                         append_Features(
                                                                         single_Features(attr(val, Int, no_expr())),
                                                                         single_Features(attr(str_field, prim_slot, no_expr()))),
                                                         single_Features(method(length, nil_Formals(), Int, no_expr()))),
                                         single_Features(method(concat,
                                                                single_Formals(formal(arg, Str)),
                                                                Str,
                                                                no_expr()))),
                         single_Features(method(substr,
                                                append_Formals(single_Formals(formal(arg, Int)),
                                                               single_Formals(formal(arg2, Int))),
                                                Str,
                                                no_expr()))),
         filename);
  
  add_class(Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{
  return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
  error_stream << filename << ":" << t->get_line_number() << ": ";
  return semant_error();
}

ostream& ClassTable::semant_error()
{
  semant_errors++;
  return error_stream;
}


void exit_gracefully_if_errors(ClassTable *classtable) {
  if (classtable->errors()) {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
}

/*
 class MethodInfo {
 private:
 Symbol return_type;
 std::vector<Symbol> arguments;
 public:
 MethodInfo (method_class);
 Symbol get_return_type ();
 Symbol get_nth_argument_type (int n);
 };
 
 class MethodTypeEnvironment {
 private:
 std::map <Symbol, std::map<Symbol, MethodInfo> > environment_map;
 void add_class (Symbol class_name);
 void add_method (Symbol class_name, Symbol method_name);
 public:
 MethodTypeEnvironment (Classes);
 Symbol get_return_type (Symbol class_name, Symbol method_name);
 Symbol get_nth_argument_type (int n, Symbol class_name, Symbol method_name);
 };
 */

MethodInfo::MethodInfo (method_class meth) {
  this->return_type = meth.get_return_type ();
  Formals argument_list = meth.get_formals ();
  for (int i = argument_list->first(); argument_list->more(i); i = argument_list->next(i)) {
    this->arguments.push_back (argument_list->nth(i)->get_type());
  }
}

/*   This is the entry point to the semantic checker.
 
 Your checker should do the following two things:
 
 1) Check that the program is semantically correct
 2) Decorate the abstract syntax tree with type information
 by setting the `type' field in each Expression node.
 (see `tree.h')
 
 You are free to first do 1), make sure you catch all semantic
 errors. Part 2) can be done in a second stage, when you want
 to build mycoolc.
 */
void program_class::semant()
{
  initialize_constants();
  
  /* ClassTable constructor may do some semantic analysis */
  ClassTable *classtable = new ClassTable(classes);
  exit_gracefully_if_errors(classtable);
  
  if (!classtable->all_defined()) exit_gracefully_if_errors(classtable);
  if (classtable->has_cycles()) exit_gracefully_if_errors(classtable);
  
  /* some semantic analysis code may go here */
  
  if (classtable->errors()) {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
}


