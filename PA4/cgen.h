#include <assert.h>
#include <stdio.h>
#include <map>
#include <vector>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class ClassInfo {
public:
  std::vector<Symbol> attribute_types;
  std::vector<Symbol> attribute_names;
  std::vector<Symbol> method_names;
  std::vector<Symbol> method_definers;
  std::map<Symbol, int> attribute_offset_map;
  int class_tag;
};

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   int next_class_tag_to_give;
   int next_label_to_give;


// The following methods emit code for
// constants and global declarations.
   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   int first_pass(CgenNodeP node, ostream &s);
   void recursively_emit_prototype(CgenNodeP node, ostream &s, std::vector<Symbol>& prototype_types, std::vector<Symbol>& prototype_names);
   void recursively_emit_disptable(CgenNodeP node, ostream &s, std::vector<Symbol> disptable_names, std::vector<Symbol> disptable_definers);
   void emit_class_objTab();
   void emit_class_objTab_helper(CgenNodeP node);
   void emit_class_nameTab();
   void emit_class_nameTab_helper(CgenNodeP);
   void emit_object_inits(CgenNodeP, ostream&);
   void emit_class_methods(CgenNodeP, ostream&);
   void generate_method_code (CgenNodeP, method_class *, ostream&);
   CgenNodeP find_symbol(Symbol, CgenNodeP);
public:
   CgenClassTable(Classes, ostream& str);
   int attribute_offset(CgenNodeP class_node, Symbol attr_name);
   int give_class_tag();
   int give_label();
   Symbol get_parent(Symbol);
   Symbol get_class_name(int class_tag);
   void code();
   CgenNodeP root();
   std::map<Symbol, ClassInfo> class_info_map;
   std::map<Symbol, std::vector<Symbol> > class_to_method_map;
   std::map<Symbol, int> num_subclass_map;
   std::vector<Symbol> class_tags;


};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   SymbolTable <Symbol, int> store;           // Maps local variable names to offset from FP

};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};




