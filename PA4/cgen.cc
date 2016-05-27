
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <queue>
#include <stack>
#include <string>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };

/* Static variable used to track current class. */
static CgenNodeP cur_class;

/* Static CgenClassTable. */
static CgenClassTable *ct;


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  ct = new CgenClassTable(classes,os);
  
  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;
  emit_disptable_ref(Str, s);


 /***** Add dispatch information for class String ******/

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 
  emit_disptable_ref(Int, s);

 /***** Add dispatch information for class Int ******/

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;
  emit_disptable_ref(Bool, s);

 /***** Add dispatch information for class Bool ******/

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


#define STRCLASSTAG 4
#define BOOLCLASSTAG 3
#define INTCLASSTAG 2


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   stringclasstag = STRCLASSTAG;
   intclasstag =    INTCLASSTAG;
   boolclasstag =   BOOLCLASSTAG;

   next_class_tag_to_give = 0;
   next_label_to_give = 0;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
   ct = this;         // Set ct to this because we never leave constructor.
   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

int CgenClassTable::give_class_tag() {
  return next_class_tag_to_give++;
}

int CgenClassTable::give_label() {
  return next_label_to_give++;
}

CgenNodeP CgenClassTable::find_symbol(Symbol class_name, CgenNodeP node) {
  if (node->name == class_name)
    return node;
  for (List<CgenNode> *child = node->get_children(); child; child = child->tl()) {
    CgenNodeP found_node = find_symbol(class_name, child->hd());
    if (found_node != NULL) return found_node;
  }
  return NULL;
}

Symbol CgenClassTable::get_parent(Symbol class_name) {
  if (class_name == No_class) return No_class;
  CgenNodeP root_node = root();
  CgenNodeP node = find_symbol(class_name, root_node);
  CgenNodeP parent = node->get_parentnd();
  return parent->name;
}

void CgenClassTable::emit_class_nameTab_helper(CgenNodeP node) {
  StringEntry *entry = stringtable.lookup_string(node->get_name()->get_string());
  str << WORD;
  entry->code_ref(str);
  str << endl;
  for (List<CgenNode> *child = node->get_children(); child; child = child->tl())
    emit_class_nameTab_helper(child->hd());
}

void CgenClassTable::emit_class_objTab_helper(CgenNodeP node) {
  str << WORD;
  emit_protobj_ref(node->get_name(), str);
  str << endl << WORD;
  emit_init_ref(node->get_name(), str);
  str << endl;
  for (List<CgenNode> *child = node->get_children(); child; child = child->tl())
    emit_class_objTab_helper(child->hd());
}

void CgenClassTable::emit_class_nameTab() {
  str << CLASSNAMETAB << ":" << endl;
  emit_class_nameTab_helper(this->root());
}

void CgenClassTable::emit_class_objTab() {
  str << CLASSOBJTAB << ":" << endl;
  emit_class_objTab_helper(this->root());
}

void CgenClassTable::emit_object_inits(CgenNodeP node, ostream& s) {
  cur_class = node;
  emit_init_ref(node->get_name(), s);
  s << ":" << endl;

  /* Enter method. */
  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, 16, s);
  emit_move(SELF, ACC, s);

  /* Call superparent's init method. */
  if (node->get_parentnd()->get_name() != No_class) {
    s << JAL;
    emit_init_ref(node->get_parentnd()->get_name(), s);
    s << endl;
  }
  method_class *init_method = (method_class *)method(node->name, nil_Formals(), No_class, block(nil_Expressions()));
  /* Generate init code for attributes. */
  for (int i = node->features->first(); node->features->more(i); i = node->features->next(i)) {
    if (!node->features->nth(i)->is_method()) {
      if (node->features->nth(i)->get_init()->is_present()) {
        node->features->nth(i)->get_init()->code(init_method, s);
        int offset = attribute_offset(node, node->features->nth(i)->get_name());
        emit_store(ACC, offset, SELF, s);
        if(cgen_Memmgr != GC_NOGC) {
          emit_addiu(A1, SELF, offset, s);
          emit_jal("_GenGC_Assign", s);
        }
      }
      
    }
  }

  /* Leave method. */
  emit_move(ACC, SELF, s);
  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  emit_addiu(SP, SP, 12, s);
  emit_return(s);
  
  /* Recurse. */
  for (List<CgenNode> *child = node->get_children(); child; child = child->tl())
    emit_object_inits(child->hd(), s);
}


void CgenClassTable::generate_method_code (CgenNodeP node, method_class *method, ostream& s) {
  cur_class->store.enterscope();

  emit_method_ref(node->get_name(), method->get_name(), s);
  s << ":" << endl;

  /* Enter method. */
  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, 16, s);
  emit_move(SELF, ACC, s);

  /* Add all formals to the  symbol table */
  for (int i = method->formals->first(); method->formals->more(i); i = method->formals->next(i)) {
    int offset = i - method->formals->first(); // TODO: figure out if this should have a +1
    cur_class->store.addid(method->formals->nth(i)->get_name(), new int(offset));
    if (cgen_debug)
      cout << "Adding formal with name " << method->formals->nth(i)->get_name() << " at offset " << offset << " from FP " << endl;
  }

  /* Evaluate expression. */
  method->expr->code(method, s);

  /* Leave method. */
  cur_class->store.exitscope();
  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  int frame_size = 12 + method->formals->len()*WORD_SIZE;
  emit_addiu(SP, SP, frame_size, s);
  emit_return(s);

}

void CgenClassTable::emit_class_methods(CgenNodeP node, ostream& s) {
  cur_class = node;

  /* Generate init code for features. */
  for (int i = node->features->first(); node->features->more(i); i = node->features->next(i)) {
    if (node->features->nth(i)->is_method() && !node->basic()) {
      method_class *method = (method_class *)node->features->nth(i);
      generate_method_code(node, method, s);
    }
  }

  /* Recurse to other classes. */
  for (List<CgenNode> *child = node->get_children(); child; child = child->tl())
    emit_class_methods(child->hd(), s);
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "coding prototype objects" << endl;

  CgenNodeP root_node = root();
  first_pass(root_node, str);
  
  emit_class_nameTab();
  emit_class_objTab();

  std::vector<Symbol> disptable_names;
  std::vector<Symbol> disptable_definers;
  recursively_emit_disptable(root_node, str, disptable_names, disptable_definers);
  str << WORD << -1 << endl; /* End of disp tables. */
  recursively_emit_prototype(root_node, str, disptable_names);

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  emit_object_inits(root_node, str);
  emit_class_methods(root_node, str);
}


void CgenClassTable::recursively_emit_disptable(CgenNodeP node, ostream &s, std::vector<Symbol> disptable_names, 
                                                std::vector<Symbol> disptable_definers) {
  std::vector<Symbol> our_disptable_names = disptable_names;
  std::vector<Symbol> our_disptable_definers = disptable_definers;
  std::vector<Symbol> method_names = class_info_map[node->name].method_names;
  std::vector<Symbol> method_definers = class_info_map[node->name].method_definers;

  /* Create our information to emit. */
  for (size_t i = 0; i < method_names.size(); i++) {
    /* Deal with overriding functions. */
    bool is_overriding = false;
    for (size_t j = 0; j < disptable_names.size(); j++) {
      if (method_names[i] == disptable_names[j]) {
        our_disptable_definers[j] = method_definers[i];
        is_overriding = true;
        break;
      }
    }
    if (is_overriding) continue;

    /* Add method to our definers. */
    our_disptable_names.push_back(method_names[i]);
    our_disptable_definers.push_back(method_definers[i]);
  }

  class_to_method_map[node->name] = our_disptable_names;

  /* Perform the emit. */
  emit_disptable_ref(node->name, s);
  s << ":" << endl;

  for (size_t i = 0; i < our_disptable_names.size(); i++) {
    s << WORD << our_disptable_definers[i] << "." << our_disptable_names[i] << endl;
  }

  for (List<CgenNode> *child = node->get_children(); child; child = child->tl())
      recursively_emit_disptable(child->hd(), s, our_disptable_names, our_disptable_definers);

}

void CgenClassTable::recursively_emit_prototype(CgenNodeP node, ostream &s, std::vector<Symbol>& prototype_types) {
  size_t type_counter = 0;

  /* Fill up prototype_types with correct attributes. */
  std::vector<Symbol> types = class_info_map[node->name].attribute_types;
  for (size_t i = 0; i < types.size(); i++) {
    type_counter++;
    prototype_types.push_back(types[i]);
  }

  emit_protobj_ref(node->name, s);
  s << ":" << endl;

  s << WORD << class_info_map[node->name].class_tag << endl;
  s << WORD << prototype_types.size() + 3 << endl;
  s << WORD;
  emit_disptable_ref(node->name, s);
  s << endl;

  /* Print out all attributes. NOTE: WE PRINT OUT STR INSTEAD OF CORRECT CONST */
  for (size_t i = 0; i < prototype_types.size(); i++) {
    Symbol type = prototype_types[i];
    /* Set to default values. */
    if (type == Int) {
      IntEntry *entry = inttable.lookup_string("0");
      s << WORD;
      entry->code_ref(s);
      s << endl;
    } else if (type == Str) {
      StringEntry *entry = stringtable.lookup_string("");
      s << WORD;
      entry->code_ref(s);
      s << endl;
    } else if (type == Bool) {
      s << WORD;
      falsebool.code_ref(s);
      s << endl;
    } else {
      s << WORD << "0" << endl;
    }
  }

  s << WORD << -1 << endl; /* Garbage Collection */

  for (List<CgenNode> *child = node->get_children(); child; child = child->tl())
      recursively_emit_prototype(child->hd(), s, prototype_types);

  /* Remove all added attributes. */
  for (size_t i = 0; i < type_counter; i++) {
    prototype_types.erase(prototype_types.begin() + (prototype_types.size() - 1));
  }
}

/* Returns the offset, in bytes, of the given attribute in an object represented by
   class_node. */
int CgenClassTable::attribute_offset(CgenNodeP class_node, Symbol attr_name) {
  if (class_info_map.find(class_node->get_name()) == class_info_map.end()) {
    cout << "Improper class name provided to attribute_offset" << endl;
    return -1;
  }
  ClassInfo ci = class_info_map[class_node->get_name()];
  for (int i = 0 ; i < ci.attribute_names.size() ; i++) {
    if (ci.attribute_names[i] == attr_name)
      return i + 3; // + 3 to account for class tag, object size, and disp pointer.
  }
  cout << "No attribute with name " << attr_name << " in class " << class_node->get_name() << " found" << endl;
  return -1;


}

Symbol CgenClassTable::get_class_name(int class_tag) {
  return class_tags[class_tag];
}


int CgenClassTable::first_pass(CgenNodeP node, ostream &s)
{
  /* Form Map. */
  ClassInfo ci;
  ci.class_tag = give_class_tag();

  for (int i = node->features->first(); node->features->more(i);
       i = node->features->next(i)) {
    Feature f = node->features->nth(i);
    if (f->is_method()) {
      ci.method_names.push_back(f->get_name());
      ci.method_definers.push_back(node->get_name());
    } else {
      ci.attribute_names.push_back(f->get_name());
      ci.attribute_types.push_back(f->get_type());
    }
  }

  class_info_map[node->name] = ci;
  class_tags.push_back(node->name);

  // cout << "added " << class_info_map[node->name].method_names.size() << " methods and "
  //      << class_info_map[node->name].attribute_types.size() << " attributes to class named "
  //      << node->name << endl << endl;
  // cout << "definers is sized: " << class_info_map[node->name].method_names.size()
  //      << " and methods is sized: " << class_info_map[node->name].method_definers.size() << endl << endl;
  
  int result = 0;
  for (List<CgenNode> *child = node->get_children(); child; child = child->tl())
      result = result + 1 + first_pass(child->hd(), s);
  num_subclass_map[node->name] = result;
  return result;

}




CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(method_class *method, ostream& s) {
  /* Invariants for our recursive algorithm:
      1. Inside of a code call, any register values can change
      2. When you leave a code call, the result is in ACC
  */
  expr->code(method, s);
  int *offset = cur_class->store.lookup(name);

  /* Attribute */
  if (offset == NULL) {
    int attr_offset = ct->attribute_offset(cur_class, name);
    emit_store(ACC, attr_offset, SELF, s);
    if(cgen_Memmgr != GC_NOGC) {
          emit_addiu(A1, SELF, attr_offset, s);
          emit_jal("_GenGC_Assign", s);
    }
    return;
  }

  /* Local variable */
  emit_store(ACC, *offset, FP, s);
}

void static_dispatch_class::code(method_class *method, ostream& s) {

  std::stack<Expression> expression_stack;
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    expression_stack.push(actual->nth(i));
  }

  while (!expression_stack.empty()) {
    Expression arg = expression_stack.top();
    expression_stack.pop();
    arg->code(method, s);
    emit_push(ACC, s);
  }

  /* Calling object. */
  expr->code(method, s);

  /* Check for dispatch on void object. */
  int not_void_label = ct->give_label();
  emit_bne(ACC, ZERO, not_void_label, s);

  /* Runtime error: dispatch on void. */
  StringEntry* filename = static_cast<StringEntry*>(cur_class->get_filename());
  emit_load_string(ACC, filename, s);
  int line_num = expr->get_line_number();
  emit_load_imm(T1, line_num, s);
  emit_jal("_dispatch_abort", s);

  /* Get proper method to invoke. */
  Symbol type = type_name;
  if (type == SELF_TYPE)
    type = cur_class->get_name();

  std::vector<Symbol> method_names = ct->class_to_method_map[type];

  int method_offset = -1;
  for (int i = 0; i < method_names.size(); i++) {
    if (method_names[i] == name) {
      method_offset = i;
      break;
    }
  }
  if (method_offset == -1)
    cout << "No method with name " << name << " found in dispatch." << endl;

  if (cgen_debug) cout << "Method offset found is " << method_offset << endl;
  emit_label_def(not_void_label, s);
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);
  emit_load(T1, method_offset, T1, s);
  emit_jalr(T1, s);

  /* Pop all arguments off the stack */
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    emit_addiu(SP, SP, 4, s);
  }

}

void dispatch_class::code(method_class *method, ostream& s) {

  std::stack<Expression> expression_stack;
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    expression_stack.push(actual->nth(i));
  }

  while (!expression_stack.empty()) {
    Expression arg = expression_stack.top();
    expression_stack.pop();
    arg->code(method, s);
    emit_push(ACC, s);
  }

  /* Calling object. */
  expr->code(method, s);

  /* Check for dispatch on void object. */
  int not_void_label = ct->give_label();
  emit_bne(ACC, ZERO, not_void_label, s);

  /* Runtime error: dispatch on void. */
  StringEntry* filename = static_cast<StringEntry*>(cur_class->get_filename());
  emit_load_string(ACC, filename, s);
  int line_num = expr->get_line_number();
  emit_load_imm(T1, line_num, s);
  emit_jal("_dispatch_abort", s);

  /* Get proper method to invoke. */
  Symbol type = expr->get_type();
  if (type == SELF_TYPE)
    type = cur_class->get_name();
  /* TODO: error check type? */
  std::vector<Symbol> method_names = ct->class_to_method_map[type];

  int method_offset = -1;
  for (int i = 0; i < method_names.size(); i++) {
    if (method_names[i] == name) {
      method_offset = i;
      break;
    }
  }

  if (cgen_debug) cout << "Method offset found is " << method_offset << endl;
  emit_label_def(not_void_label, s);
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);
  emit_load(T1, method_offset, T1, s);
  emit_jalr(T1, s);

}

void cond_class::code(method_class *method, ostream& s) {
  /* TODO: Branching causes an unknown label. */
  pred->code(method, s);
  int false_label = ct->give_label();
  int end_label = ct->give_label();

  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_beqz(T1, false_label, s);
  then_exp->code(method, s);
  emit_branch(end_label, s);
  emit_label_def(false_label, s);
  else_exp->code(method, s);
  emit_label_def(end_label, s);
}

void loop_class::code(method_class *method, ostream& s) {
  int begin_label = ct->give_label();
  int done_label = ct->give_label();

  /* Label containing all comparisons. */
  emit_label_def(begin_label, s);
  pred->code(method, s);
  emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
  emit_beqz(ACC, done_label, s);

  /* Run the body. */
  body->code(method, s);
  emit_branch(begin_label, s);

  /* Jump here when done. */
  emit_label_def(done_label, s);
  emit_load_imm(ACC, 0, s);
}

void typcase_class::code(method_class *method, ostream& s) {
  expr->code(method, s);

  /* Check for case on void object. */
  int valid_statement_label = ct->give_label();
  int done_label = ct->give_label();
  emit_bne(ACC, ZERO, valid_statement_label, s);
  StringEntry* filename = static_cast<StringEntry*>(cur_class->get_filename());
  emit_load_string(ACC, filename, s);
  int line_num = expr->get_line_number();
  emit_load_imm(T1, line_num, s);
  emit_jal("_case_abort2", s);
  emit_branch(done_label, s);

  /* Valid Case Statement. */
  emit_label_def(valid_statement_label, s);

  emit_load(T1, TAG_OFFSET, ACC, s);
  emit_push(T1, s);

  int start_label = ct->give_label();
  emit_label_def(start_label, s);

  /* Create a label for each branch and store in label_vec */
  std::vector<int> label_vec;
  std::vector<branch_class *> branch_vec;
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    branch_class *branch = (branch_class *)cases->nth(i);
    label_vec.push_back(ct->give_label());
    branch_vec.push_back(branch);
  }

  /* Sort branch_vec from highest class_tag to lowest */
  for(int i = 0; i < cases->len(); i++) {
    for(int j = 1; j < cases->len(); j++) {
      int class_tag = ct->class_info_map[branch_vec[j]->type_decl].class_tag;
      int prev_class_tag = ct->class_info_map[branch_vec[j-1]->type_decl].class_tag;
      if(class_tag > prev_class_tag) {
        branch_class *temp = branch_vec[j];
        branch_vec[j] = branch_vec[j-1];
        branch_vec[j-1] = temp;
      }
    }
  }

  /* Emit branches. */
  for (int i = 0 ; i < branch_vec.size() ; i++) {
    branch_class *branch = branch_vec[i];
    Symbol type = branch->type_decl;
    int begin_class_tag = ct->class_info_map[branch_vec[i]->type_decl].class_tag;
    int end_class_tag = ct->class_info_map[branch_vec[i]->type_decl].class_tag + ct->num_subclass_map[type];
    emit_load(T1, 1, SP, s);
    emit_blti(T1, begin_class_tag, label_vec[i], s);
    emit_bgti(T1, end_class_tag, label_vec[i], s);

    /* Execute actual branch code. */
    int offset = method->get_new_offset();
    cur_class->store.enterscope();
    emit_push(T1, s);
    cur_class->store.addid(branch->name, new int(offset));
    branch->expr->code(method, s);
    emit_addiu(SP, SP, 4, s);
    method->restore_offset();
    cur_class->store.exitscope();
    emit_branch(done_label, s);
    emit_label_def(label_vec[i], s);
  }
  emit_jal("_case_abort", s);
  emit_label_def(done_label, s);
  emit_addiu(SP, SP, 4, s);
}

void block_class::code(method_class *method, ostream& s) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(method, s);
  }
}

void let_class::code(method_class *method, ostream& s) {

  /* Add new variable to store. */
  cur_class->store.enterscope();

  if (init->get_type() != No_class) {
    init->code(method, s);
  } else {
    /* Special default values. */
    if (type_decl == Bool || type_decl == Int || type_decl == Str) {
      s << LA << ACC << " ";
      emit_protobj_ref(type_decl, s);
      s << endl;
      emit_jal("Object.copy", s);
    } else {
      emit_load_imm(ACC, 0, s);
    }
  }

  /* Put value inside of ACC into new local variable */
  emit_push(ACC, s);
  int offset = method->get_new_offset();
  cur_class->store.addid(this->identifier, new int(offset));
  emit_store(ACC, offset, FP, s);

  /* Emit code for body */
  body->code(method, s);
  emit_addiu(SP, SP, 4, s);

  method->restore_offset();
  cur_class->store.exitscope();
}

void plus_class::code(method_class *method, ostream& s) {
  e1->code(method, s);
  emit_store(ACC, 0, SP, s);
  emit_addiu(SP, SP, -4, s);
  e2->code(method, s);
  emit_jal("Object.copy", s);
  emit_fetch_int(T2, ACC, s);
  emit_load(T1, 1, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_add(T1, T1, T2, s);
  emit_addiu(SP, SP, 4, s);
  emit_store_int(T1, ACC, s);

}

void sub_class::code(method_class *method, ostream& s) {
  e1->code(method, s);
  emit_store(ACC, 0, SP, s);
  emit_addiu(SP, SP, -4, s);
  e2->code(method, s);
  emit_jal("Object.copy", s);
  emit_fetch_int(T2, ACC, s);
  emit_load(T1, 1, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_sub(T1, T1, T2, s);
  emit_addiu(SP, SP, 4, s);
  emit_store_int(T1, ACC, s);
}

void mul_class::code(method_class *method, ostream& s) {
  e1->code(method, s);
  emit_store(ACC, 0, SP, s);
  emit_addiu(SP, SP, -4, s);
  e2->code(method, s);
  emit_jal("Object.copy", s);
  emit_fetch_int(T2, ACC, s);
  emit_load(T1, 1, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_mul(T1, T1, T2, s);
  emit_addiu(SP, SP, 4, s);
  emit_store_int(T1, ACC, s);
}

void divide_class::code(method_class *method, ostream& s) {
  e1->code(method, s);
  emit_store(ACC, 0, SP, s);
  emit_addiu(SP, SP, -4, s);
  e2->code(method, s);
  emit_jal("Object.copy", s);
  emit_fetch_int(T2, ACC, s);
  emit_load(T1, 1, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_div(T1, T1, T2, s);
  emit_addiu(SP, SP, 4, s);
  emit_store_int(T1, ACC, s);
}

void neg_class::code(method_class *method, ostream& s) {
  e1->code(method, s);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);

  /* Save value on stack while creating Object copy. */
  emit_push(T1, s);
  emit_jal("Object.copy", s);

  /* Retrieve value from stack and store in return object. */
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void lt_class::code(method_class *method, ostream& s) {
  e1->code(method, s);
  emit_push(ACC, s);
  e2->code(method, s);
  emit_load(T1, 1, SP, s);    //store result of e1 in T1
  emit_addiu(SP, SP, 4, s);

  /* Instead of Int objects, get actual int values. */
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  int finished_label = ct->give_label();
  emit_load_bool(ACC, truebool, s);
  emit_blt(T1, T2, finished_label, s);

  emit_load_bool(ACC, falsebool, s);

  emit_label_def(finished_label, s);
}

void eq_class::code(method_class *method, ostream& s) {
  e1->code(method, s);
  emit_push(ACC, s);
  e2->code(method, s);
  emit_load(T1, 1, SP, s);    //store result of e1 in T1
  emit_move(T2, ACC, s);    //store result of e2 in T2
  emit_addiu(SP, SP, 4, s);
  int finished_label = ct->give_label();
  emit_load_bool(ACC, truebool, s);

  emit_beq(T1, T2, finished_label, s);     // Jump to end if the pointers are the same.

  /* If not the same objects, compare using equality_test. */
  if (cgen_debug)
    cout << "Comparing objects for equality using equality_test" << endl;
  emit_load_bool(A1, falsebool, s);
  emit_jal("equality_test", s);

  emit_label_def(finished_label, s);
}


void leq_class::code(method_class *method, ostream& s) {
  e1->code(method, s);
  emit_push(ACC, s);
  e2->code(method, s);
  emit_load(T1, 1, SP, s);    //store result of e1 in T1
  emit_addiu(SP, SP, 4, s);

  /* Instead of Int objects, get actual int values. */
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  int finished_label = ct->give_label();
  emit_load_bool(ACC, truebool, s);
  emit_bleq(T1, T2, finished_label, s);

  emit_load_bool(ACC, falsebool, s);

  emit_label_def(finished_label, s);

}

/* Not operator */
void comp_class::code(method_class *method, ostream& s) {
  int begin_label = ct->give_label();
  int end_label = ct->give_label();

  e1->code(method, s);
  /* If the below does not work, try this method of evaluating the bool: 

  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_beqz(T1, begin_label, s);
  */
  emit_load_bool(T1, falsebool, s);
  emit_beq(ACC, T1, begin_label, s);

  /* If ACC contains true, then put false in ACC */
  if (cgen_debug)
    cout << "In a NOT operation, returning FALSE" << endl;
  emit_load_bool(ACC, falsebool, s);
  emit_branch(end_label, s);

  /* If ACC contains false, then put true in ACC */
  emit_label_def(begin_label, s);
  if (cgen_debug)
    cout << "In a NOT operation, returning TRUE" << endl;
  emit_load_bool(ACC, truebool, s);
  emit_label_def(end_label, s);
}

void int_const_class::code(method_class *method, ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(method_class *method, ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(method_class *method, ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(method_class *method, ostream& s) {
  /* Get correct Object type. */
  Symbol type = type_name;

  if (type == SELF_TYPE) {
    /* Get the correct offset in Object Table for prototype object. */
    emit_partial_load_address(T1, s);
    s << CLASSOBJTAB << endl;
    emit_load(T2, 0, SELF, s);
    emit_sll(T2, T2, 3, s);
    emit_addu(T1, T1, T2, s);
    emit_load(ACC, 0, T1, s);
    emit_jal("Object.copy", s);

    /* Get the correct offset in Object Table for initializer. */
    emit_partial_load_address(T1, s);
    s << CLASSOBJTAB << endl;
    emit_load(T2, 0, SELF, s);
    emit_sll(T2, T2, 3, s);
    emit_addu(T1, T1, T2, s);
    emit_load(T1, 4, T1, s);
    emit_jalr(T1, s);
    return;
  }

  /* Find Object initialization information in class_objTab. */
  emit_partial_load_address(ACC, s);
  emit_protobj_ref(type, s);
  s << endl;
  emit_jal("Object.copy", s);
  s << JAL;
  emit_init_ref(type, s);
  s << endl;
}

void isvoid_class::code(method_class *method, ostream& s) {
  e1->code(method, s);
  int isvoid_label = ct->give_label();
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_beqz(T1, isvoid_label, s);

  emit_load_bool(ACC, falsebool, s);
  emit_label_def(isvoid_label, s);
}

void no_expr_class::code(method_class *method, ostream& s) {
  emit_load_imm(ACC, 0, s);
}

void object_class::code(method_class *method, ostream& s) {
  /* Handle self case. */
  if (name == self) {
    emit_move(ACC, SELF, s);
    return;
  }

  int *offset = cur_class->store.lookup(name);
  
  /* Attribute. */
  if (offset == NULL) {
    int attr_offset = ct->attribute_offset(cur_class, name);
    emit_load(ACC, attr_offset, SELF, s);
    return;
  }
  /* Local */
  emit_load(ACC, *offset, FP, s);
}


