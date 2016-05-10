#include <set>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "assert.h"
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

Class_ create_object_class (Symbol filename) {
  return class_(Object,
                No_class,
                append_Features(
                         append_Features(
                                         single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                         single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                         single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
                filename);
}

Class_ create_io_class (Symbol filename) {
  return class_(IO,
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
}

Class_ create_int_class (Symbol filename) {
  return class_(Int,
                Object,
                single_Features(attr(val, prim_slot, no_expr())),
                filename);
}

Class_ create_bool_class (Symbol filename) {
  return class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
}

Class_ create_str_class (Symbol filename) {
  return class_(Str,
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
  
  Class_ Object_class = create_object_class (filename);
  add_class(Object_class);
  
  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE       writes a string to the output
  //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
  //        in_string() : Str                 reads a string from the input
  //        in_int() : Int                      "   an int     "  "     "
  //
  Class_ IO_class = create_io_class (filename);
  add_class(IO_class);
  
  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  Class_ Int_class = create_int_class (filename);
  add_class(Int_class);
  
  //
  // Bool also has only the "val" slot.
  //
  Class_ Bool_class = create_bool_class (filename);
  add_class(Bool_class);
  
  //
  // The class Str has a number of slots and operations:
  //       val                                  the length of the string
  //       str_field                            the string itself
  //       length() : Int                       returns length of the string
  //       concat(arg: Str) : Str               performs string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring selection
  //
  Class_ Str_class = create_str_class (filename);
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
 class MethodTypeEnvironment {
 TODO: Symbol get_nth_argument_type (int n, Symbol class_name, Symbol method_name);
 };
 */

MethodTypeEnvironment::MethodTypeEnvironment (Classes classes) : semant_errors(0) , error_stream(cerr) {
  install_basic_classes();
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ cl = classes->nth(i);
    ClassMethodInfo *new_class_info = new ClassMethodInfo(cl);
    this->environment_map[cl->get_name()] = new_class_info;
  }
}

void MethodTypeEnvironment::install_basic_classes() {
  Symbol filename = stringtable.lookup_string("<basic class>");

  ClassMethodInfo *object_class_info = new ClassMethodInfo(create_object_class(filename));
  this->environment_map[Object] = object_class_info;
  ClassMethodInfo *int_class_info = new ClassMethodInfo(create_int_class(filename));
  this->environment_map[Int] = int_class_info;
  ClassMethodInfo *str_class_info = new ClassMethodInfo(create_str_class(filename));
  this->environment_map[Str] = str_class_info;
  ClassMethodInfo *bool_class_info = new ClassMethodInfo(create_bool_class(filename));
  this->environment_map[Bool] = bool_class_info;
  ClassMethodInfo *io_class_info = new ClassMethodInfo(create_io_class(filename));
  this->environment_map[IO] = io_class_info;
}

Symbol MethodTypeEnvironment::get_return_type (Class_ cl, Symbol method_name) {
  Symbol return_type = NULL;
  while (cl->get_name() != No_class) {
    Symbol class_name = cl->get_name();
    ClassMethodInfo *cminfo = this->environment_map[class_name];
    Symbol return_type = cminfo->get_method_return_type(method_name);
    if (return_type) break;
  }

  return return_type;
}

/* The same error reporting functions provided for use in ClassTable,
   but adapted to work for MethodTypeEnvironment instead. */
ostream& MethodTypeEnvironment::semant_error(Class_ c)
{
  return semant_error(c->get_filename(),c);
}

ostream& MethodTypeEnvironment::semant_error(Symbol filename, tree_node *t)
{
  error_stream << filename << ":" << t->get_line_number() << ": ";
  return semant_error();
}

ostream& MethodTypeEnvironment::semant_error()
{
  semant_errors++;
  return error_stream;
}

void MethodTypeEnvironment::dump_type_environment () {
  cout << "We are beginning our dump of the type environment" << endl;

  for (std::map<Symbol, ClassMethodInfo *>::iterator it = this->environment_map.begin();
       it != this->environment_map.end(); ++it) {
    cout << "Now looking at the class named: " << it->first << endl;

    for (std::map<Symbol, MethodInfo *>::iterator mit = it->second->method_map.begin();
       mit != it->second->method_map.end(); ++mit) {

      cout << "Now looking at the method named: " << mit->first;
      cout << " and return type: " << mit->second->get_return_type() << endl;
      cout << " it has arguments of type: ";
      for (int i = 0; i < mit->second->arguments.size(); i++) {
        cout << mit->second->arguments[i] << ", ";
      }
      cout << endl;

    }
  }
}

ClassMethodInfo::ClassMethodInfo(Class_ cl) {
  Features class_features = cl->get_features();
  for (int j = class_features->first(); class_features->more(j); j = class_features->next(j)) {
    Feature feat = class_features->nth(j);
    if (!feat->is_method())
      continue;
    Symbol method_name = feat->get_name();

    cout << "Creating method info in class: " << cl->get_name() << endl;
    MethodInfo *new_method_info = new MethodInfo(feat);
    this->method_map[method_name] = new_method_info;
  }
}

Symbol ClassMethodInfo::get_method_return_type (Symbol method_name) {
  if (this->method_map.find(method_name) == this->method_map.end())
    return NULL;
  MethodInfo *method_info = this->method_map[method_name];
  return method_info->get_return_type();
}

Symbol ClassMethodInfo::get_nth_argument_type (Symbol method_name, int n) {
  if (this->method_map.find(method_name) == this->method_map.end())
    return NULL;
  MethodInfo *method_info = this->method_map[method_name];
  return method_info->get_nth_argument_type(n);
}

MethodInfo::MethodInfo (Feature meth) {
  this->return_type = meth->get_type ();
  cout << "Creating a method named: " << meth->get_name() << " with type: " << this->return_type << endl;
  Formals argument_list = meth->get_formals ();
  for (int i = argument_list->first(); argument_list->more(i); i = argument_list->next(i)) {
    this->arguments.push_back (argument_list->nth(i)->get_type());
  }
}

Symbol MethodInfo::get_return_type () { 
  return this->return_type;
}

/* Returns the type of the nth formal of the given method. Returns
   NULL If no such nth formal exists. */
Symbol MethodInfo::get_nth_argument_type (int n) {
  if (this->arguments.size() <= n) {
    return NULL;
  }
  return this->arguments[n];
}

void report_error() {
  cout << "WE FOUND AN ERROR" << endl;
  return;
}

bool class__class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating class " << name << endl;
  st->enterscope();
  bool result = true;
  /* First, go through attributes to verify and add to symbol table. */
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    if (!features->nth(i)->is_method()) {
      features->nth(i)->verify_type (st,mte);
    }
  }

  /* Second, go through and verify methods. */  
  for(int j = features->first(); features->more(j); j = features->next(j)) {
    if (!features->nth(j)->is_method()) continue;
    if (!features->nth(j)->verify_type(st, mte)) result = false;
  }
    
  st->exitscope();
  return result;
}

bool method_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating method " << name << endl;
  st->enterscope();
  bool result = true;
  for(int i = formals->first(); formals->more(i); i = formals->next(i))
    if (!formals->nth(i)->verify_type(st, mte)) result = false;
  if (!expr->verify_type(st, mte)) result = false;
  if (!expr->get_type() == return_type) {
    report_error();
    result = false;
  }
  st->exitscope();
  return result;
}


bool attr_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  /* TODO:
    return true if the expression is either no_expr_class or
    matches with the type declared in the attribute

    If this is the case, set object identifier to be of type T in st
    */
  cout << "Evaluating attribute " << name << endl;
  if (!init->verify_type(st, mte)) return false;
  if (init->get_type() != type_decl && init->get_type() != No_type) {
    report_error();
    return false;
  }
  st->addid(name, &type_decl);
  return true;
}

bool formal_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  /* TODO: figure out what to do for formal class. */
  cout << "Evaluating formal class: " << name << endl;
   /*
   dump_line(stream,n,this);
   stream << pad(n) << "_formal\n";
   dump_Symbol(stream, n+2, name);
   dump_Symbol(stream, n+2, type_decl);
   */
  return true;
}

bool branch_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "evaluating branch class" << endl;
  bool result = true;
  if (!expr->verify_type(st, mte)) result = false;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_branch\n";
   dump_Symbol(stream, n+2, name);
   dump_Symbol(stream, n+2, type_decl);
   expr->dump_with_types(stream, n+2);
   */

  return result;

}

//
// assign_class::dump_with_types prints "assign" and then (indented)
// the variable being assigned, the expression, and finally the type
// of the result.  Note the call to dump_type (see above) at the
// end of the method.
//
bool assign_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "evaluating assign class" << endl;
  bool result = true;
  if (!expr->verify_type(st, mte)) return false;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_assign\n";
   dump_Symbol(stream, n+2, name);
   expr->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

//
// static_dispatch_class::dump_with_types prints the expression,
// static dispatch class, function name, and actual arguments
// of any static dispatch.  
//
bool static_dispatch_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating static dispatch class" << endl;
  bool result = true;
  if (!expr->verify_type(st, mte)) result = false;
  for(int i = actual->first(); actual->more(i); i = actual->next(i))
    if (!actual->nth(i)->verify_type(st, mte)) result = false;

  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_static_dispatch\n";
   expr->dump_with_types(stream, n+2);
   dump_Symbol(stream, n+2, type_name);
   dump_Symbol(stream, n+2, name);
   stream << pad(n+2) << "(\n";
   for(int i = actual->first(); actual->more(i); i = actual->next(i))
     actual->nth(i)->dump_with_types(stream, n+2);
   stream << pad(n+2) << ")\n";
   dump_type(stream,n);
   */
  return result;
}

//
//   dispatch_class::dump_with_types is similar to 
//   static_dispatch_class::dump_with_types 
//
bool dispatch_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating dispatch class" << endl;
  bool result = true;
  if (!expr->verify_type(st, mte)) result = false;
  for(int i = actual->first(); actual->more(i); i = actual->next(i))
    if (!actual->nth(i)->verify_type(st, mte)) result = false;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_dispatch\n";
   expr->dump_with_types(stream, n+2);
   dump_Symbol(stream, n+2, name);
   stream << pad(n+2) << "(\n";
   for(int i = actual->first(); actual->more(i); i = actual->next(i))
     actual->nth(i)->dump_with_types(stream, n+2);
   stream << pad(n+2) << ")\n";
   dump_type(stream,n);
   */
  return result;
}

//
// cond_class::dump_with_types dumps each of the three expressions
// in the conditional and then the type of the entire expression.
//
bool cond_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating cond class" << endl;
  bool result = true;
  if (!pred->verify_type(st, mte) 
      || !then_exp->verify_type(st, mte)
      || !else_exp->verify_type(st, mte)) result = false;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_cond\n";
   pred->dump_with_types(stream, n+2);
   then_exp->dump_with_types(stream, n+2);
   else_exp->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

//
// loop_class::dump_with_types dumps the predicate and then the
// body of the loop, and finally the type of the entire expression.
//
bool loop_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating loop class" << endl;
  bool result = true;
  if (!pred->verify_type(st, mte) || !body->verify_type(st, mte)) result = false;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_loop\n";
   pred->dump_with_types(stream, n+2);
   body->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

//
//  typcase_class::dump_with_types dumps each branch of the
//  the Case_ one at a time.  The type of the entire expression
//  is dumped at the end.
//
bool typcase_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating typecase class" << endl;
  bool result = true;
  if (!expr->verify_type(st, mte)) result = false;
  for (int i = cases->first(); cases->more(i); i = cases->next(i))
    if (!cases->nth(i)->verify_type(st, mte)) result = false;
  

  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_typcase\n";
   expr->dump_with_types(stream, n+2);
   for(int i = cases->first(); cases->more(i); i = cases->next(i))
     cases->nth(i)->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

//
//  The rest of the cases for Expression are very straightforward
//  and introduce nothing that isn't already in the code discussed
//  above.
//
bool block_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating block class" << endl;
  bool result = true;
  for (int i = body->first(); body->more(i); i = body->next(i))
    if (!body->nth(i)->verify_type(st, mte)) result = false;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_block\n";
   for(int i = body->first(); body->more(i); i = body->next(i))
     body->nth(i)->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

bool let_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating let class" << endl;
  bool result = true;
  if (!init->verify_type(st, mte)) result = false;
  if (!body->verify_type(st, mte)) result = false;

  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_let\n";
   dump_Symbol(stream, n+2, identifier);
   dump_Symbol(stream, n+2, type_decl);
   init->dump_with_types(stream, n+2);
   body->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

bool plus_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating plus class" << endl;
  bool result = true;
  if (!e1->verify_type(st, mte)) result = false;
  if (!e2->verify_type(st, mte)) result = false;

  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_plus\n";
   e1->dump_with_types(stream, n+2);
   e2->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

bool sub_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating sub class" << endl;
  bool result = true;
  if (!e1->verify_type(st, mte)) result = false;
  if (!e2->verify_type(st, mte)) result = false;

  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_sub\n";
   e1->dump_with_types(stream, n+2);
   e2->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

bool mul_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating mul class" << endl;
  bool result = true;
  if (!e1->verify_type(st, mte)) result = false;
  if (!e2->verify_type(st, mte)) result = false;

  /* 
   dump_line(stream,n,this);
   stream << pad(n) << "_mul\n";
   e1->dump_with_types(stream, n+2);
   e2->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

bool divide_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating divide class" << endl;
  bool result = true;
  if (!e1->verify_type(st, mte)) result = false;
  if (!e2->verify_type(st, mte)) result = false;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_divide\n";
   e1->dump_with_types(stream, n+2);
   e2->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

bool neg_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating neg class" << endl;
  bool result = true;
  if (!e1->verify_type(st, mte)) result = false;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_neg\n";
   e1->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

bool lt_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating lt class" << endl;
  bool result = true;
  if (!e1->verify_type(st, mte)) result = false;
  if (!e2->verify_type(st, mte)) result = false;

  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_lt\n";
   e1->dump_with_types(stream, n+2);
   e2->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}


bool eq_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating eq class" << endl;
  bool result = true;
  if (!e1->verify_type(st, mte)) result = false;
  if (!e2->verify_type(st, mte)) result = false;

  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_eq\n";
   e1->dump_with_types(stream, n+2);
   e2->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

bool leq_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating leq class" << endl;
  bool result = true;
  if (!e1->verify_type(st, mte)) result = false;
  if (!e2->verify_type(st, mte)) result = false;

  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_leq\n";
   e1->dump_with_types(stream, n+2);
   e2->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

bool comp_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating comp class" << endl;
  bool result = true;
  if (!e1->verify_type(st, mte)) result = false;

  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_comp\n";
   e1->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

bool int_const_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  /* TODO: Figure out what to do with const class. */
  cout << "Evaluating int_const_class class" << endl;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_int\n";
   dump_Symbol(stream, n+2, token);
   dump_type(stream,n);
   */
  return true;
}

bool bool_const_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  /* TODO: Figure out what to do with const class. */
  cout << "Evaluating bool_const_class class" << endl;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_bool\n";
   dump_Boolean(stream, n+2, val);
   dump_type(stream,n);
   */
  return true;
}

bool string_const_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  /* TODO: Figure out what to do with const class. */
  cout << "Evaluating string_const_class class" << endl;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_string\n";
   stream << pad(n+2) << "\"";
   print_escaped_string(stream,token->get_string());
   stream << "\"\n";
   dump_type(stream,n);
   */
  return true;
}

bool new__class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  /* TODO: Figure out what to do with new class. */
  cout << "Evaluating new class" << endl;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_new\n";
   dump_Symbol(stream, n+2, type_name);
   dump_type(stream,n);
   */
  return true;
}

bool isvoid_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  cout << "Evaluating isvoid class" << endl;
  bool result = true;
  if (!e1->verify_type(st, mte)) result = false;

  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_isvoid\n";
   e1->dump_with_types(stream, n+2);
   dump_type(stream,n);
   */
  return result;
}

bool no_expr_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  /* TODO: Figure out what to do with no expr class. */
  cout << "Evaluating no_expr_class class" << endl;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_no_expr\n";
   dump_type(stream,n);
   */
  return true;
}

bool object_class::verify_type(SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte)
{
  /* TODO: Figure out what to do with object class. */
  cout << "Evaluating object class" << endl;
  /*
   dump_line(stream,n,this);
   stream << pad(n) << "_object\n";
   dump_Symbol(stream, n+2, name);
   dump_type(stream,n);
   */
  return true;
}

void dump_program_tree (Classes classes, SymbolTable<Symbol, Symbol> *st, MethodTypeEnvironment *mte) {
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ current_class = classes->nth(i);
    cout << "BEFORE" << endl;
    current_class->verify_type(st, mte);
    cout << "AFTER" << endl;
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
  printf ("WHERE AM I?\n");
  MethodTypeEnvironment *mte = new MethodTypeEnvironment (classes);
  mte->dump_type_environment();
  cout << "AFTER DUMP" << endl;
  SymbolTable<Symbol, Symbol> *st = new SymbolTable<Symbol, Symbol>();
  dump_program_tree(classes, st, mte);
  if (classtable->errors()) {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
}


