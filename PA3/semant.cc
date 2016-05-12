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

/* Static global variables. */
static MethodTypeEnvironment *mte;
static SymbolTable<Symbol, Entry> *st;
static ClassTable *classtable;
Class_ current_class;

/*
 * Adds a class to the current ClassTable with the name class_name and
 * the ancestor inherits_from. Returns whether or not it was successful.
 */
void ClassTable::add_class(Class_ current_class) {
  Symbol class_name = current_class->get_name();
  Symbol inherits_from = current_class->get_parent();
  
  /* Ensure no class is defined more than once. */
  if (class_name == SELF_TYPE) {
    semant_error(current_class) << "A class cannot have name SELF_TYPE" << endl;
    return;
  }
  if (inherits_from == SELF_TYPE) {
    semant_error(current_class) << "Class " << class_name << " incorrectly inherits from SELF_TYPE" << endl;
  }
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

/* Checks the provided class and its superclasses until a method matching the name provided is found.
   Returns the nth argument type of that method. */
Symbol MethodTypeEnvironment::get_nth_argument_type (int n, Symbol class_name, Symbol method_name) {
  Symbol argument_type = NULL;
  while (class_name != No_class) {
    if (this->environment_map.find(class_name) == this->environment_map.end())
      return NULL;
    ClassMethodInfo *cminfo = this->environment_map[class_name];
    argument_type = cminfo->get_nth_argument_type(method_name, n);
    if (argument_type) return argument_type;
    class_name = classtable->inherits_from(class_name);
  }
  return argument_type;
}

/* Checks the provided class and its superclasses until a method matching the name provided
 * is found. Returns the return value of that method. */
Symbol MethodTypeEnvironment::get_return_type (Symbol class_name, Symbol method_name) {
  Symbol return_type = NULL;
  while (class_name != No_class) {
    if (this->environment_map.find(class_name) == this->environment_map.end())
      return NULL;
    ClassMethodInfo *cminfo = this->environment_map[class_name];
    Symbol return_type = cminfo->get_method_return_type(method_name);
    if (return_type) {
      cout << "Found return type " << return_type << " for class " << class_name << ", method " << method_name << endl;
      return return_type;
    }
    class_name = classtable->inherits_from(class_name);
  }
  return return_type;
}

std::map<Symbol, Symbol> *MethodTypeEnvironment::get_class_attributes (Symbol class_name) {
  if (this->environment_map.find(class_name) == this->environment_map.end())
      return NULL;
  ClassMethodInfo *cminfo = this->environment_map[class_name];
  return &cminfo->attribute_map;
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

bool MethodTypeEnvironment::has_attribute (Symbol class_name, Symbol attr_name) {
  while (class_name != No_class) {
    ClassMethodInfo *cminfo = this->environment_map[class_name];
    if (cminfo->attribute_map.find(attr_name) != cminfo->attribute_map.end())
      return true;
    class_name = classtable->inherits_from(class_name);
  }
  return false;
}

ClassMethodInfo::ClassMethodInfo(Class_ cl) {
  Features class_features = cl->get_features();
  for (int j = class_features->first(); class_features->more(j); j = class_features->next(j)) {
    Feature feat = class_features->nth(j);
    if (!feat->is_method()) {
      Symbol attr_name = feat->get_name();
      Symbol attr_type = feat->get_type();
      this->attribute_map[attr_name] = attr_type;
      continue;
    }
    Symbol method_name = feat->get_name();

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

/* Returns true if child_class is a subclass of ancestor or ancestor itself. */
bool is_subclass (Symbol child_class, Symbol ancestor) {
  if (child_class == NULL || ancestor == NULL) {
    cerr << "Null class passed into is_subclass." << endl;
    return false;
  }
  if (ancestor == SELF_TYPE) return false;
  if (child_class == SELF_TYPE)
    child_class = current_class->get_name();
  if (child_class == No_type)
    return true; 
  Symbol parent = child_class;
  while (true) {
    if (parent == ancestor) {
      cout << "is_subclass(" << child_class << ", " << ancestor << ") is true" << endl;
      return true;
    }
    if (parent == No_class)
      return false;
    parent = classtable->inherits_from(parent);
  }
}

/* Returns the first common ancestor class between the two
 * provided classes. 
 */
Symbol lub (Symbol class_one, Symbol class_two) {
  /* Handle SELF_TYPE cases. */
  if (class_one == SELF_TYPE && class_two == SELF_TYPE)
    return SELF_TYPE;
  if (class_one == SELF_TYPE)
    return lub(current_class->get_name(), class_two);
  if (class_two == SELF_TYPE)
    return lub (class_one, current_class->get_name());

  std::set<Symbol> class_one_ancestors;
  Symbol class_one_parent = class_one;
  while (class_one_parent != No_class) {
    class_one_ancestors.insert(class_one_parent);
    class_one_parent = classtable->inherits_from(class_one_parent);
  }
  Symbol class_two_parent = class_two;
  while (class_two_parent != No_class) {
    if (class_one_ancestors.find(class_two_parent) != class_one_ancestors.end())
      return class_two_parent;
    class_two_parent = classtable->inherits_from(class_two_parent);
  }
  return No_class;
}

void add_superclass_attributes_to_scope() {
  Symbol class_name = classtable->inherits_from(current_class->get_name());
  while (class_name != No_class) {
    std::map<Symbol, Symbol> *attributes = mte->get_class_attributes(class_name);
    for (std::map<Symbol, Symbol>::iterator it = attributes->begin();
       it != attributes->end(); ++it) {
      Symbol name = it->first;
      Symbol type = it->second;
      st->addid(name,type);
    }
    class_name = classtable->inherits_from(class_name);
  }
}


bool class__class::verify_type()
{
  cout << "Evaluating class " << name << endl;
  st->enterscope();
  current_class = this;
  bool result = true;
  /* First, go through attributes to verify and add to symbol table. */
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    if (!features->nth(i)->is_method()) {
      if (!features->nth(i)->verify_type())
        result = false;

    }
  }

  add_superclass_attributes_to_scope();

  /* Second, go through and verify methods. */  
  for(int j = features->first(); features->more(j); j = features->next(j)) {
    if (!features->nth(j)->is_method()) continue;
    if (!features->nth(j)->verify_type()) {
      result = false;
    }
  }
    
  st->exitscope();
  return result;
}

bool verify_proper_method_inheritence(Symbol name, Symbol return_type, Formals formals, tree_node *t) {
  bool result = true;

  Symbol parent_class = classtable->inherits_from(current_class->get_name());
  Symbol parent_return_type = mte->get_return_type (parent_class, name);

  /* Return true if method is not inherited from superclass. */
  if (parent_return_type == NULL)
    return true;

  if (parent_return_type != return_type)
    result = false;
  int i;
  for(i = formals->first(); formals->more(i); i = formals->next(i)) {
    if (mte->get_nth_argument_type(i, parent_class, name) != formals->nth(i)->get_type())
      result = false;
  }
  /* Check for too few arguments in overriden method. */
  if (mte->get_nth_argument_type(i, parent_class, name) != NULL)
    result = false;

  if (result == false) {
    classtable->semant_error(current_class->get_filename(), t) << "Inherited method " << name << "in class "
          << current_class->get_name() << " does not match prototype defined in parent class "
          << parent_class << endl;
  }
  return result;
}


bool method_class::verify_type()
{
  cout << "Evaluating method " << name << endl;
  bool result = true;

  if (this->name == self) {
    classtable->semant_error(current_class->get_filename(), this) << "Method improperly named keyword self" << endl;
    result = false;
  }
  /* Verify that this method does not override incorrectly. */
  result = verify_proper_method_inheritence(this->name, this->return_type, this->formals, this);

  /* Add self and formal parameters to symbol table. */
  st->enterscope();
  st->addid(self, SELF_TYPE);

  for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
    if (formals->nth(i)->get_type() == SELF_TYPE) {
      classtable->semant_error(current_class->get_filename(), this) << "Method " << this->name << " has improper "
        << "formal of type SELF_TYPE" << endl;
      result = false;
    }
    if (formals->nth(i)->get_name() == self) {
      classtable->semant_error(current_class->get_filename(), this) << "Method " << this->name << " has improper "
        << "formal with name self" << endl;
      result = false;
    }
    if (result)
      st->addid(formals->nth(i)->get_name(), formals->nth(i)->get_type());
  }
    
  if (!expr->verify_type()) {
    cout << "Verifying expression type failed." << endl;
    result = false;
  }

  if (!result) {
    st->exitscope();
    return false;
  }

  if (!is_subclass(expr->get_type(), return_type)) {
    classtable->semant_error(current_class->get_filename(), this) << "Method " << name << "'s expression type of " 
      << expr->get_type() << " does not match method's return type of " << return_type << endl;
    st->exitscope();
    return false;
  }
  st->exitscope();
  return result;
}

bool attr_class::verify_type()
{
  cout << "Evaluating attribute " << name << endl;
  bool result = true;

  if (this->name == self) {
    classtable->semant_error(current_class->get_filename(), this) << "Attribute incorrectly named keyword self" << endl;
    result = false;
  }

  /* Ensure no attribute with this name in ancestor classes. */
  Symbol parent_class = classtable->inherits_from(current_class->get_name());
  if (mte->has_attribute(parent_class, this->name)) {
    result = false;
    classtable->semant_error(current_class->get_filename(), this) << "Attribute " << name << " in class "
      << current_class->get_name() << " is previously defined in a parent class." << endl;
  }

  if (!init->verify_type()) return false;
  if (init->get_type() != No_type && !is_subclass(init->get_type(), type_decl)) {
    classtable->semant_error(current_class->get_filename(), this) << "Attribute initialization of type "
      << init->get_type() << " does not match expected type " << type_decl << endl;
    result = false;
  }
  if (st->lookup(name) != NULL) {
    classtable->semant_error(current_class->get_filename(), this) << "Multiple attributes declared with name " << name
      << " in class " << current_class->get_name() << endl;
    result = false;
  }
  if (result)
    st->addid(name, type_decl);
  return result;
}

bool formal_class::verify_type()
{
  return true;
}

bool branch_class::verify_type()
{
  cout << "evaluating branch class" << endl;
  bool result = true;
  st->enterscope();
  st->addid(name, type_decl);
  if (!expr->verify_type()) {
    this->type = Object;
    st->exitscope();
    return false;
  }
  this->type = expr->get_type();
  st->exitscope();
  return true;
}

//
// assign_class::dump_with_types prints "assign" and then (indented)
// the variable being assigned, the expression, and finally the type
// of the result.  Note the call to dump_type (see above) at the
// end of the method.
//
bool assign_class::verify_type()
{
  cout << "evaluating assign statement for variable " << name << endl;

  /* Look for object ID in symbol table. */
  bool result = true;
  Symbol found_type = st->lookup(name);
  if (found_type == NULL) {
    classtable->semant_error(current_class->get_filename(), this) << "Object identifier " << name <<
      " in assign statement not found in symbol table." << endl;
    this->type = Object;
    result = false;
  }

  /* Verify that the assignment expression's type is valid and a subclass of return type. */
  if (!expr->verify_type()) {
    this->type = Object;
    return false;
  }
  if (!is_subclass(expr->get_type(), found_type)) {
    classtable->semant_error(current_class->get_filename(), this) << "Object identifier " << name <<
      " with type " << found_type <<
      " does not match type of its assignment " << expr->get_type() << endl;
    this->type = Object;
    result = false;
  }
  if (!result) return false;

  this->type = found_type;
  return true;
}

//
// static_dispatch_class::dump_with_types prints the expression,
// static dispatch class, function name, and actual arguments
// of any static dispatch.  
//
bool static_dispatch_class::verify_type()
{
  cout << "Evaluating static dispatch" << endl;
  bool result = true;
  if (!expr->verify_type()) {
    this->type = Object;
    result = false;
  }
  Symbol class_name = expr->get_type();
  Symbol expected_class_name = this->type_name;

  /* Extra check for expected type in static dispatch. */
  if (!is_subclass(class_name, expected_class_name)) {
    classtable->semant_error(current_class->get_filename(), this) << "Type of expression in "
      << "static dispatch statement does not match type " << expected_class_name << endl;
    this->type = Object;
    result = false;
  }

  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    if (!actual->nth(i)->verify_type()) {
      this->type = Object;
      result = false;
      continue;
    }
    Symbol expected_arg_type = mte->get_nth_argument_type(i, class_name, this->name);
    if (!is_subclass(actual->nth(i)->get_type(),expected_arg_type)) {
      classtable->semant_error(current_class->get_filename(), this) << "Call to function " << this->name
        << " in class " << class_name << " does not match prototype." << endl;
      this->type = Object;
      result = false;
    }
  }

  if (result == true)
    this->type = mte->get_return_type(class_name, this->name);

  return result;
}


bool dispatch_class::verify_type()
{
  cout << "Evaluating dispatch class" << endl;
  bool result = true;
  if (!expr->verify_type()) {
    this->type = Object;
    result = false;
  }

  /* If no object expression provided, use current cass. */
  Symbol class_name = expr->get_type();
  if (class_name == No_type)
    class_name = current_class->get_name();

  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    if (!actual->nth(i)->verify_type()) {
      this->type = Object;
      result = false;
      continue;
    }
    Symbol expected_arg_type = mte->get_nth_argument_type(i, class_name, this->name);
    if (!is_subclass(actual->nth(i)->get_type(),expected_arg_type)) {
      classtable->semant_error(current_class->get_filename(), this) << "Call to function " << this->name
        << " in class " << class_name << " does not match prototype." << endl;
      this->type = Object;
      result = false;
    }
  }

  if (result) {
    cout << "Looking in MTE for class " << class_name << " and method " << this->name << endl;
    Symbol return_type = mte->get_return_type(class_name, this->name);
    this->type = return_type;
    cout << "Set this->type to " << return_type << endl;
  }

  return result;
}

//
// cond_class::dump_with_types dumps each of the three expressions
// in the conditional and then the type of the entire expression.
//
bool cond_class::verify_type()
{
  cout << "Evaluating cond class" << endl;
  bool result = true;

  /* Ensure subexpressions are valid. */
  if (!pred->verify_type())
    result = false;
  if (!then_exp->verify_type())
    result = false;
  if (!else_exp->verify_type())
    result = false;

  if (!result) {
    this->type = Object;
    return false;
  }
  /* Ensure predicate is a boolean. */
  if (pred->get_type() != Bool) {
    classtable->semant_error(current_class->get_filename(), this) << "Predicate in if-statement not of type Bool" << endl;
    this->type = Object;
    return false;
  }
  this->type = lub(then_exp->get_type(), else_exp->get_type());
  return true;
}


bool loop_class::verify_type()
{
  /* Set type to Object no matter what for loops. */
  this->type = Object;
  if (!pred->verify_type() || !body->verify_type()) {
    return false;
  }
  /* Verify predicate is a boolean. */
  if (pred->get_type() != Bool) {
    classtable->semant_error(current_class->get_filename(), this) << "Predicate in loop not of type Bool" << endl;
    return false;
  }
  return true;
}


bool typcase_class::verify_type()
{
  bool result = true;
  cout << "Evaluating typecase class" << endl;
  if (!expr->verify_type()) {
    this->type = Object;
    return false;
  }

  Symbol return_type = Object;

  /* If no cases, throw semantic error. */
  if (cases->len() == 0) {
    classtable->semant_error(current_class->get_filename(), this) << "Case expression with no "
      << "branches found" << endl;
    result = false;
  }

  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    /* If improper branch, mark as failure and skip to next branch. */
    if (!cases->nth(i)->verify_type()) {
      result = false;
      continue;
    }
    /* Return type is lub(evaluted types of all n branches) */
    return_type = lub(return_type, cases->nth(i)->get_type());
  }
  this->type = return_type;
  return result;
}

//
//  The rest of the cases for Expression are very straightforward
//  and introduce nothing that isn't already in the code discussed
//  above.
//
bool block_class::verify_type()
{
  bool result = true;

  /* Set type of block equal to last expression in block. */
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    if (!body->nth(i)->verify_type()) 
      result = false;
    this->type = body->nth(i)->get_type();
  }
  if (result == false) {
    this->type = Object;
    return false;
  }
  return true;
}

bool let_class::verify_type()
{
  cout << "Evaluating let case" << endl;
  bool result = true;

  Symbol declared_type = type_decl;

  if (this->identifier == self) {
    classtable->semant_error(current_class->get_filename(), this) << "Object identifier cannot be self" << endl;
    result = false;
  }

  /* Evaluate type of init and ensure it matches type_decl. */  
  if (!init->verify_type())
    result = false;

  if (!result) {
    this->type = Object;
    return false;
  }
  Symbol init_type = init->get_type();
  if (!is_subclass(init_type, declared_type)) {
      classtable->semant_error(current_class->get_filename(), this) << "Object identifier " << identifier <<
      " in let statement does not match type of its assignment" << endl;
      this->type = Object;
      return false;
  }

  /* Evaluate body using new object. */
  st->enterscope();
  st->addid(identifier, type_decl);

  if (!body->verify_type()) {
    this->type = Object;
    st->exitscope();
    return false;
  }

  this->type = body->get_type();
  st->exitscope();
  return true;
}

bool plus_class::verify_type()
{
  bool result = true;
  if (!e1->verify_type()) result = false;
  if (!e2->verify_type()) result = false;
  if (result == false) {
    this->type = Object;
    return false;
  }
  if (e1->get_type() != Int || e2->get_type() != Int) {
    classtable->semant_error(current_class->get_filename(), this) << 
      "Both expressions in a plus operation are not of type Int" << endl;
    this->type = Object;
    return false;
  }
  this->type = Int;
  return true;
}

bool sub_class::verify_type()
{
  bool result = true;
  if (!e1->verify_type()) result = false;
  if (!e2->verify_type()) result = false;
  if (result == false) {
    this->type = Object;
    return false;
  }
  if (e1->get_type() != Int || e2->get_type() != Int) {
    classtable->semant_error(current_class->get_filename(), this) << 
      "Both expressions in a subtraction operation are not of type Int" << endl;
    this->type = Object;
    return false;
  }
  this->type = Int;
  return true;
}

bool mul_class::verify_type()
{
  bool result = true;
  if (!e1->verify_type()) result = false;
  if (!e2->verify_type()) result = false;
  if (result == false) {
    this->type = Object;
    return false;
  }
  if (e1->get_type() != Int || e2->get_type() != Int) {
    classtable->semant_error(current_class->get_filename(), this) << 
      "Both expressions in a multiply operation are not of type Int" << endl;
    this->type = Object;
    return false;
  }
  this->type = Int;
  return true;
}

bool divide_class::verify_type()
{
  bool result = true;
  if (!e1->verify_type()) result = false;
  if (!e2->verify_type()) result = false;
  if (result == false) {
    this->type = Object;
    return false;
  }
  if (e1->get_type() != Int || e2->get_type() != Int) {
    classtable->semant_error(current_class->get_filename(), this) << 
      "Both expressions in a divide operation are not of type Int" << endl;
    this->type = Object;
    return false;
  }
  this->type = Int;
  return true;
}

bool neg_class::verify_type()
{
  bool result = true;
  if (!e1->verify_type()) {
    this->type = Object;
    return false;
  }
  if (e1->get_type() != Int) {
    classtable->semant_error(current_class->get_filename(), this) << 
      "Attempting to negate something other than an Int" << endl;
    this->type = Object;
    return false;
  }
  this->type = Int;
  return result;
}

bool lt_class::verify_type()
{
  bool result = true;
  if (!e1->verify_type()) result = false;
  if (!e2->verify_type()) result = false;
  if (result == false) {
    this->type = Object;
    return false;
  }
  if (e1->get_type() != Int or e2->get_type() != Int) {
    this->type = Object;
    classtable->semant_error(current_class->get_filename(), this) << 
      "Attempting to compare something other than an Int" << endl;
    return false;
  }
  this->type = Bool;
  return true;
}


bool eq_class::verify_type()
{
  bool result = true;
  if (!e1->verify_type()) result = false;
  if (!e2->verify_type()) result = false;
  if (result == false) {
    this->type = Object;
    return false;
  }
  bool type_check_required = false;
  if (e1->get_type() == Int  ||
      e1->get_type() == Str  ||
      e1->get_type() == Bool ||
      e2->get_type() == Int  ||
      e2->get_type() == Str  ||
      e2->get_type() == Bool )
    type_check_required = true;

  if (type_check_required) {
    if (e1->get_type() != e2->get_type()) {
      this->type = Object;
      classtable->semant_error(current_class->get_filename(), this) << 
        "Attempting to compare using == mismatching basic types (Int, Bool, or String)" << endl;
      return false;
    }
  }
  
  this->type = Bool;
  return true;
}

bool leq_class::verify_type()
{
  bool result = true;
  if (!e1->verify_type()) result = false;
  if (!e2->verify_type()) result = false;
  if (result == false) {
    this->type = Object;
    return false;
  }
  if (e1->get_type() != Int or e2->get_type() != Int) {
    this->type = Object;
    classtable->semant_error(current_class->get_filename(), this) << 
      "Attempting to compare something other than an Int" << endl;
    return false;
  }
  this->type = Bool;
  return true;
}

bool comp_class::verify_type()
{
  bool result = true;
  if (!e1->verify_type()) {
    this->type = Object;
    return false;
  }
  if (e1->get_type() != Bool) {
    classtable->semant_error(current_class->get_filename(), this) << 
      "Attempting to NOT something other than a Bool" << endl;
    this->type = Object;
    return false;
  }
  this->type = Bool;
  return true;
}

bool int_const_class::verify_type()
{
  this->type = Int;
  return true;
}

bool bool_const_class::verify_type()
{
  this->type = Bool;
  return true;
}

bool string_const_class::verify_type()
{
  this->type = Str;
  return true;
}

bool new__class::verify_type()
{
  if (type_name == SELF_TYPE) {
    this->type = SELF_TYPE;
  } else {
    this->type = type_name;
  }
  return true;
}

bool isvoid_class::verify_type()
{
  bool result = true;
  if (!e1->verify_type()) {
    this->type = Object;
    return false;
  }
  this->type = Bool;
  return true;
}

bool no_expr_class::verify_type()
{
  this->type = No_type;
  return true;
}

bool object_class::verify_type()
{
  /* self is of type SELF_TYPE */
  if (this->name == self) {
    this->type = SELF_TYPE;
    return true;
  }
  /* Look up variable in symbol table. */
  Symbol found_type = st->lookup(this->name);
  if (found_type == NULL) {
    classtable->semant_error(current_class->get_filename(), this) << "Object identifier " << name <<
      " not found in symbol table." << endl;
    return false;
  }
  this->type = found_type;  
  return true;
}


void verify_types (Classes classes) {
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ current_class = classes->nth(i);
    current_class->verify_type();
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
  classtable = new ClassTable(classes);
  exit_gracefully_if_errors(classtable);
  
  if (!classtable->all_defined()) exit_gracefully_if_errors(classtable);
  if (classtable->has_cycles()) exit_gracefully_if_errors(classtable);
  
  /* some semantic analysis code may go here */
  printf ("WHERE AM I?\n");
  mte = new MethodTypeEnvironment (classes);
  //mte->dump_type_environment();
  cout << "AFTER DUMP" << endl;
  st = new SymbolTable<Symbol, Entry>();
  verify_types(classes);
  exit_gracefully_if_errors(classtable);
  if (classtable->errors()) {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
}
