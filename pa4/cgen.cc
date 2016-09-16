
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

#include <functional>
#include <algorithm>
#include <initializer_list>
#include <utility>

#include "cgen.h"
#include "cgen_gc.h"

using namespace std::placeholders;

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
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

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
      << WORD << STRINGNAME DISPTAB_SUFFIX << endl;              // dispatch table
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
      << WORD << INTNAME DISPTAB_SUFFIX << endl;          // dispatch table
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
      << WORD << BOOLNAME DISPTAB_SUFFIX << endl;           // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

CgenNodeP CgenClassTable::get_node_by_name(Symbol name) const {
  CgenNodeMap::const_iterator i = node_map.find(name);
  if(i != node_map.end()) {
    return i->second;
  }

  return 0;
}

bool CgenClassTable::add_node(CgenNodeP node) {
  Symbol name = node->get_name();

  if (get_node_by_name(name)) {
    return false;
  }

  nodes.push_back(node);
  node_map[name] = node;

  return true;
}

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

static bool node_tag_compare(const CgenNodeP a, const CgenNodeP b) {
  return a->get_tag() < b->get_tag();
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nodes(NULL) , str(s)
{
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);

   build_inheritance_tree();
   root()->inherit(0);
   sort(nodes.begin(),nodes.end(),node_tag_compare);

   intclasstag =    get_node_by_name(Int)->get_tag();
   boolclasstag =   get_node_by_name(Bool)->get_tag();
   stringclasstag = get_node_by_name(Str)->get_tag();

   code();
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
/*  add_node(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  add_node(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  add_node(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));*/

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

void CgenClassTable::install_class(CgenNodeP node)
{
  add_node(node);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

void CgenClassTable::build_inheritance_tree()
{
  std::for_each(nodes.rbegin(),nodes.rend(),
                std::bind(&CgenClassTable::set_relations,this,_1));
}

void CgenClassTable::set_relations(CgenNodeP node)
{
  CgenNode *parent_node = get_node_by_name(node->get_parent());
  node->set_parent_node(parent_node);
  if(parent_node) {
    parent_node->add_child(node);
  }
}

void CgenClassTable::class_name_tab() {
  str << CLASSNAMETAB << LABEL;
  for(CgenNodeVector::iterator i=nodes.begin();
      i != nodes.end();++i) {
    CgenNodeP n = *i;
    str << WORD;
    n->get_string_name()->code_ref(str);
    str << endl;
  }
}

void CgenClassTable::class_obj_tab() {
  str << CLASSOBJTAB << LABEL;
  for(auto i=nodes.begin();i != nodes.end();++i) {
    CgenNodeP n = *i;
    str << WORD << n->get_name() << PROTOBJ_SUFFIX << endl;
    str << WORD << n->get_name() << CLASSINIT_SUFFIX << endl;
  }
}

void CgenClassTable::class_disp_tab(CgenNodeP node) {
  str << node->get_name() << DISPTAB_SUFFIX << LABEL;

  const MethodVector &methods = node->get_methods();
  for(auto i=methods.begin();i != methods.end();++i) {
    Method *method = *i;
    str << WORD;
    if(debug_mode) str << "[" << method->index << "]";
    str << method->class_name 
        << METHOD_SEP << method->name << endl;
  }

  const CgenNodeVector& children = node->get_children();
  for_each(children.rbegin(),children.rend(),
           bind(&CgenClassTable::class_disp_tab,this,_1));
}

void CgenClassTable::class_prot_obj(CgenNodeP node) {
  str << WORD << "-1" << endl;
  str << node->get_name() << PROTOBJ_SUFFIX << LABEL;
  str << WORD << node->get_tag() << endl;
  str << WORD << node->get_object_size() << endl;
  str << WORD << node->get_name() << DISPTAB_SUFFIX << endl;
  
  const AttrVector &attrs = node->get_attrs();
  for(auto i = attrs.begin();i != attrs.end();++i) {
    Attribute *attr = *i;

    str << WORD;
    if(debug_mode) str << "[" << attr->index << "]";
    if(Int == attr->type) {
      inttable.lookup_string("0")->code_ref(str);
    } else if(Str == attr->type) {
      stringtable.lookup_string("")->code_ref(str);
    } else if(Bool == attr->type) {
      falsebool.code_ref(str);
    } else {
      str << EMPTYSLOT;
    }

    str << endl;
  }

  const CgenNodeVector& children = node->get_children();
  for_each(children.rbegin(),children.rend(),
           bind(&CgenClassTable::class_prot_obj,this,_1));
}

void CgenClassTable::class_init(CgenNodeP node) {
  node->code_init(this,str);

  const CgenNodeVector& children = node->get_children();
  for_each(children.rbegin(),children.rend(),
           bind(&CgenClassTable::class_init,this,_1));
}

void CgenClassTable::class_functions(CgenNodeP node) {
  node->code_func(this,str);

  const CgenNodeVector& children = node->get_children();
  for_each(children.rbegin(),children.rend(),
           bind(&CgenClassTable::class_functions,this,_1));
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  class_name_tab();

  class_obj_tab();

  class_disp_tab(root());

  class_prot_obj(root());

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  class_init(root());

  class_functions(root());

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

}


CgenNodeP CgenClassTable::root()
{
   return get_node_by_name(Object);
}

VarScope* CgenClassTable::get_class_scope(Symbol class_name) const {
  CgenNodeP node = get_node_by_name(class_name);
  if(!node) {
    return 0;
  }

  auto *var_scope = new VarScope();
  var_scope->enterscope();

  const auto& attrs = node->get_attrs();
  for(auto i=attrs.begin();i!=attrs.end();++i) {
    Attribute *attr = *i;
    var_scope->addid(attr->name,attr);
  }

  var_scope->addid(self,new Self(self,node->get_name()));

  return var_scope;
}

const Attribute* CgenClassTable::lookup_attribute(Symbol class_name, Symbol attr_name) const {
  CgenNodeP node = get_node_by_name(class_name);
  if(!node){
    logger->report_error("Class[%s] not found\n",class_name->get_string());
    return 0;
  }

  return node->get_attr_by_name(attr_name);
}

const Method* CgenClassTable::lookup_method(Symbol class_name, Symbol method_name) const {
  CgenNodeP node = get_node_by_name(class_name);
  if(!node){
    logger->report_error("Class[%s] not found\n",class_name->get_string());
    return 0;
  }

  return node->get_method_by_name(method_name);
}

unsigned int CgenClassTable::get_class_tag(Symbol class_name) const {
  CgenNodeP node = get_node_by_name(class_name);
  if(!node){
    logger->report_error("Class[%s] not found\n",class_name->get_string());
    return 0;
  }

  return node->get_tag();
}

unsigned int CgenClassTable::get_class_max_child_tag(Symbol class_name) const {
  CgenNodeP node = get_node_by_name(class_name);
  if(!node){
    logger->report_error("Class[%s] not found\n",class_name->get_string());
    return 0;
  }

  return node->get_max_child_tag();
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

unsigned int CgenNode::class_tag_index = 0;

CgenNode::CgenNode(Class_ class_, Basicness bstatus, CgenClassTableP ct) :
   class__class(dynamic_cast<const class__class&>(*class_)),
   parent_node(NULL),
   basic_status(bstatus),
   tag(0)
{ 

}

bool CgenNode::add_attribute(Attribute *info) {
  info->class_name = name;
  info->index = attrs.size();
  attrs.push_back(info);
  attr_map[info->name] = info;
  return true;
}

bool CgenNode::add_method(Method *info) {
  info->class_name = name;

  auto base_method = method_map.find(info->name);
  if(base_method == method_map.end()) {
    info->index = methods.size();  
    methods.push_back(info);
  } else {
    info->index = base_method->second->index;
    methods[info->index] = info;
  } 
  method_map[info->name] = info; 
  
  return true;
}

void CgenNode::inherit(CgenNodeP parent) {
  string_name = stringtable.add_string(name->get_string());

  if(parent) {
    tag = ++class_tag_index;
    logger->debug("inherit[%s]<-[%s][%d]\n",
                parent->name->get_string(),
                name->get_string(),tag);

    
    attrs = parent->attrs;
    attr_map = parent->attr_map;
    methods = parent->methods;
    method_map = parent->method_map;
  }

  get_features_info(this);

  for_each(children.rbegin(),children.rend(),
           bind(&CgenNode::inherit,_1,this));

  max_child_tag = class_tag_index;

  if(debug_mode) {
    fprintf(stderr,"Attrs[%s]\n",name->get_string());
    for(auto i=attr_map.begin();i != attr_map.end();++i) {
      Symbol name = i->first;
      Attribute *attr = i->second;
      fprintf(stderr,"[%s]->[%d]\n",name->get_string(),attr->index);
    }

    fprintf(stderr,"Methods[%s]\n",name->get_string());
    for(auto i=method_map.begin();i != method_map.end();++i) {
      Symbol name = i->first;
      Method *method = i->second;
      fprintf(stderr,"[%s]->[%d][%s]\n",name->get_string(),method->index,method->class_name->get_string());
    }
  }
}

const Attribute* CgenNode::get_attr_by_name(Symbol name) const {
  auto i = attr_map.find(name);
  if(i == attr_map.end()) {
    logger->report_error("Attribute[%s] not found\n",name->get_string());
    return 0;
  }
  return i->second;
}

const Method* CgenNode::get_method_by_name(Symbol name) const {
  auto i = method_map.find(name);
  if(i == method_map.end()) {
    logger->report_error("Method[%s] not found\n",name->get_string());
    return 0;
  }
  return i->second;
}

void CgenNode::code_init(IStaticInfo* static_info,ostream& s) const {
  CurrentFilenameContext filename_context(logger,filename);

  s << name << CLASSINIT_SUFFIX << LABEL;

  unsigned int tmp_count = 0;
  for(auto i=attrs.begin();i!=attrs.end();++i) {
      Attribute *attr = *i;
      tmp_count = std::max(tmp_count,attr->init_expr->tmp_needed());
  }

  Frame frame(0,tmp_count,s);
  Scope scope(name,static_info);

  if(parent_node) {
    s << JAL << parent_node->name << CLASSINIT_SUFFIX << endl;
  } 

  for(auto i=attrs.begin();i!=attrs.end();++i) {
    Attribute *attr = *i;
    if(attr->class_name == name) {
      assign(attr->name,attr->init_expr)->code(&frame,&scope,s);
    }
  }

  emit_move(ACC,SELF,s);
}

void CgenNode::code_func(IStaticInfo* static_info,ostream& s) const {
  CurrentFilenameContext filename_context(logger,filename);

  if(basic_status == Basic) {
    return;
  }

  Scope scope(name,static_info);

  for(auto i=methods.begin();i!=methods.end();++i) {
    Method *method = *i;
    if(method->class_name == name) {
      method->code(&scope,s);
    }
  }
}

/******************************************************************/

bool class__class::get_features_info(IFeatureInfoStorage* info_storage) {
    CurrentFilenameContext filename_context(logger,filename);
    CurrentTreeNodeContext tree_node_context(logger,this);

    bool result = true;

    for(int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feature = features->nth(i);
        result = result && feature->get_feature_info(info_storage);
    }
    return result;
}

bool attr_class::get_feature_info(IFeatureInfoStorage* info_storage) {
    CurrentTreeNodeContext tree_node_context(logger,this);

    Attribute *attr_info = new Attribute(name,type_decl,init);
    if(!attr_info) {
        logger->report_error(this,"%s: Memory allocation failure[%d]\n",
                                           __func__,
                                           sizeof(*attr_info));
        return false;
    }

    logger->debug("%s\n",attr_info->to_string().c_str());

    if(!info_storage->add_attribute(attr_info)) {
        return false;
    }

    return true;
}

bool method_class::get_feature_info(IFeatureInfoStorage* info_storage) {
    CurrentTreeNodeContext tree_node_context(logger,this);

    Method *method_info = new Method(name,return_type,expr);
    if(!method_info) {
        logger->report_error(this,"%s: Memory allocation failure[%d]\n",
                                          __func__,
                                          sizeof(*method_info));
        return false;
    }

    unsigned int total = 0;
    for(int i = formals->first(); formals->more(i); i = formals->next(i),++total);

    for(int i = formals->first(),index=0; formals->more(i); i = formals->next(i),++index) {
        Formal formal = formals->nth(i);

        Argument arg_info(formal->get_name(),formal->get_type(),index,total);
        method_info->args.push_back(arg_info);
    }

    logger->debug("Method[%s]\n",method_info->to_string().c_str());

    if(!info_storage->add_method(method_info)) {
         return false;
    }

    return true;
}

/******************************************************************/

static unsigned int emit_stack_store(std::initializer_list<char*> regs,
                                     unsigned int tmp_count,
                                     ostream &s) {
  unsigned int size = regs.size()+tmp_count;
  emit_addiu(SP,SP,-WORD_SIZE*size,s);
  for(auto i=regs.begin();i!=regs.end();++i) {
    emit_store(*i,regs.begin()-i+regs.size(),SP,s);
  }
  return size;
}

static void emit_stack_load(std::initializer_list<char*> regs,
                            unsigned int arg_count,
                            unsigned int tmp_count,
                            ostream &s) {
  unsigned int size = regs.size()+arg_count+tmp_count;
  for(auto i=regs.begin();i!=regs.end();++i) {
    emit_load(*i,regs.begin()-i+regs.size(),SP,s);
  }
  emit_addiu(SP,SP,WORD_SIZE*size,s);
}

Frame::Frame(unsigned int _arg_count,unsigned int _tmp_count,ostream &_s)
:s(_s),arg_count(_arg_count),tmp_count(_tmp_count),tmp_used(0) {
  unsigned int size = emit_stack_store({FP,SELF,RA},tmp_count,s);
  emit_addiu(FP,SP,size*WORD_SIZE,s);
  emit_move(SELF,ACC,s);

  for(unsigned int i=0;i<tmp_count;++i) {
    tmps.push_back(StackTemporary(i));
  }
}

Frame::~Frame() {
  emit_stack_load({FP,SELF,RA},arg_count,tmp_count,s);
  emit_return(s);
}

const Variable* Frame::reserve_temporary() {
  if(tmp_used < tmp_count) {
    return &tmps[tmp_used++];
  } else {
    logger->report_error("Frame::reserve_temporary: no more tmps[%d]\n",
                         tmp_count);
    return 0;
  }
}

void Frame::release_temporary(const Variable* var) {
  if(var != &tmps[tmp_used-1]) {
    logger->report_error("Frame::release_temporary: improper release attempt[%s][%s]\n",
                          var->to_string().c_str(),tmps.rbegin()->to_string().c_str());
  } else {
    --tmp_used;
  }
}

/******************************************************************/

void Attribute::code_load(char* reg,ostream& s) const {
  emit_load(reg,DEFAULT_OBJFIELDS+index,SELF,s);
}

void Attribute::code_store(char* reg,ostream& s) const {
  emit_store(reg,DEFAULT_OBJFIELDS+index,SELF,s);
}

void Attribute::code_load(char* reg,char* src_reg,ostream& s) const {
  emit_load(reg,DEFAULT_OBJFIELDS+index,src_reg,s);
}

void Attribute::code_store(char* reg,char* src_reg,ostream& s) const {
  emit_store(reg,DEFAULT_OBJFIELDS+index,src_reg,s);
}

void Argument::code_load(char* reg, ostream& s) const {
  emit_load(reg,total-index,FP,s);
}

void Argument::code_store(char* reg, ostream& s) const {
  emit_store(reg,total-index,FP,s);
}

static Symbol get_tmp_name(unsigned int index) {
  char buffer[12];
  snprintf(buffer,sizeof(buffer),"!tmp%d",index);
  return idtable.add_string(buffer);
}

StackTemporary::StackTemporary(unsigned int _index)
:Variable(get_tmp_name(_index),No_type),index(_index) {

}

void StackTemporary::code_load(char* reg, ostream& s) const {
  emit_load(reg,-index,FP,s);
}

void StackTemporary::code_store(char* reg, ostream& s) const {
  emit_store(reg,-index,FP,s);
}

void Self::code_load(char* reg, ostream& s) const {
  emit_move(reg,SELF,s);
}

void Self::code_store(char* reg, ostream& s) const {
  emit_move(SELF,reg,s);
}

void Method::code(Scope *scope, ostream &s) const {
  ScopeGuard<Scope> scope_guard(scope);

  for(auto i=args.begin();i!=args.end();++i) {
    const Argument *arg = &(*i);
    scope->addid(arg->name,arg);
  }

  s << class_name << METHOD_SEP << name << LABEL;

  unsigned int tmp_count = expr->tmp_needed();
  logger->debug("Method[%s]:tmp[%d]\n",to_string().c_str(),tmp_count);
  Frame frame(args.size(),tmp_count,s);

  expr->code(&frame,scope,s);
}

/******************************************************************/

Scope::Scope(Symbol class_name, IStaticInfo *_static_info)
:current_class_name(class_name),
 static_info(_static_info),
 var_scope(static_info->get_class_scope(class_name)),
 int_val_attr(*static_info->lookup_attribute(Int,val)),
 bool_val_attr(*static_info->lookup_attribute(Bool,val)) {

}

const Method* Scope::lookup_method(Symbol class_name,Symbol method_name) const {
  if(class_name == SELF_TYPE) {
    class_name = current_class_name;
  }
  return static_info->lookup_method(class_name,method_name);
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

bool assign_class::code(Frame *frame,Scope *scope,ostream &s) {
  CurrentTreeNodeContext tree_node_context(logger,this);

  const Variable *var = scope->lookup(name);
  if(!var) {
    logger->report_error("Variable[%s] not found in current scope\n",name->get_string());
    return false;
  }

  if(expr->code(frame,scope,s)) {
    var->code_store(ACC,s);
  }

  return true;
}

static unsigned int label_index = 0;

template<class Self,class Operation,class Value>
static bool unary_operation(Self &self,Operation operation,Value *value,
                            Frame *frame, Scope *scope,ostream &s) {
  self.e1->code(frame,scope,s);
  emit_jal("Object.copy",s);
  
  value->code_load(T1,ACC,s);  
  operation(T1,T1,s);
  value->code_store(T1,ACC,s);

  return true;
}

template<class Self,class Operation,class Value>
static bool binary_comparison(Self &self,Operation operation,Value *value,
                              Frame *frame, Scope *scope,ostream &s) {
  self.e1->code(frame,scope,s);
  value->code_load(T1,ACC,s);

  const Variable *tmp1 = frame->reserve_temporary();
  if(!tmp1) return false;
  tmp1->code_store(T1,s);

  self.e2->code(frame,scope,s);
  
  value->code_load(T2,ACC,s);
  tmp1->code_load(T1,s);
  
  unsigned int label_id = label_index++;

  emit_load_bool(ACC,truebool,s);  
  operation(T1,T2,label_id,s);
  emit_load_bool(ACC,falsebool,s);
  emit_label_def(label_id,s);

  frame->release_temporary(tmp1);

  return true;
}

template<class Self,class Operation,class Value>
static bool binary_operation(Self &self,Operation operation,Value *value,
                             Frame *frame, Scope *scope,ostream &s) {
  
  self.e1->code(frame,scope,s);
  value->code_load(T1,ACC,s);  

  const Variable *tmp1 = frame->reserve_temporary();
  if(!tmp1) return false;
  tmp1->code_store(T1,s);

  self.e2->code(frame,scope,s);
  emit_jal("Object.copy",s);

  value->code_load(T2,ACC,s);
  tmp1->code_load(T1,s);
  operation(T1,T1,T2,s);

  value->code_store(T1,ACC,s);

  frame->release_temporary(tmp1);

  return true;
}

bool static_dispatch_class::code(Frame *frame,Scope *scope,ostream &s) {
  unsigned int label_id = label_index++;

  for(int i = actual->first(),index=0; actual->more(i); i = actual->next(i),++index) {
      Expression arg = actual->nth(i);

      arg->code(frame,scope,s);

      emit_push(ACC,s);
  }

  expr->code(frame,scope,s);

  emit_bne(ACC,ZERO,label_id,s);

  emit_load_string(ACC,stringtable.lookup_string(logger->get_current_filename()->get_string()),s);
  emit_load_imm(T1,get_line_number(),s);
  emit_jal("_dispatch_abort",s);

  emit_label_def(label_id,s);

  const Method* method = scope->lookup_method(type_name,name);
  if(!method) {
    logger->report_error("Static dispatch: Class[%s] method[%s] not found\n",
                         type_name->get_string(),
                         name->get_string());
    return false;
  }

  s << LA << T1 << " " << type_name << DISPTAB_SUFFIX << endl;
  emit_load(T1,method->index,T1,s);
  emit_jalr(T1,s);

  return true;
}

bool dispatch_class::code(Frame *frame,Scope *scope,ostream &s) {
  unsigned int label_id = label_index++;

  for(int i = actual->first(),index=0; actual->more(i); i = actual->next(i),++index) {
      Expression arg = actual->nth(i);

      arg->code(frame,scope,s);

      emit_push(ACC,s);
  }

  expr->code(frame,scope,s);

  emit_bne(ACC,ZERO,label_id,s);

  emit_load_string(ACC,stringtable.lookup_string(logger->get_current_filename()->get_string()),s);
  emit_load_imm(T1,get_line_number(),s);
  emit_jal("_dispatch_abort",s);

  emit_label_def(label_id,s);

  Symbol expr_type = expr->get_type();
  const Method* method = scope->lookup_method(expr_type,name);
  if(!method) {
    logger->report_error("Dynamic dispatch: Class[%s] method[%s] not found\n",
                         expr_type->get_string(),
                         name->get_string());
    return false;
  }

  emit_load(T1,DISPTABLE_OFFSET,ACC,s);
  emit_load(T1,method->index,T1,s);
  emit_jalr(T1,s);

  return true;
}

bool cond_class::code(Frame *frame,Scope *scope,ostream &s) {
  unsigned int label_else = label_index++;
  unsigned int label_end = label_index++;

  auto value = scope->get_bool_val_attr();

  pred->code(frame,scope,s);

  value->code_load(T1,ACC,s);
  emit_beqz(T1,label_else,s);

  then_exp->code(frame,scope,s);

  emit_branch(label_end,s);
  emit_label_def(label_else,s);

  else_exp->code(frame,scope,s);

  emit_label_def(label_end,s);

  return true;
}

bool loop_class::code(Frame *frame,Scope *scope,ostream &s) {
  unsigned int label_begin = label_index++;
  unsigned int label_end = label_index++;

  auto value = scope->get_bool_val_attr();

  emit_label_def(label_begin,s);

  pred->code(frame,scope,s);
  
  value->code_load(T1,ACC,s);
  emit_beq(T1,ZERO,label_end,s);
  
  body->code(frame,scope,s);
  
  emit_branch(label_begin,s);
  emit_label_def(label_end,s);
  emit_move(ACC,ZERO,s);

  return true;
}

bool typcase_class::code(Frame *frame,Scope *scope,ostream &s) {
  expr->code(frame,scope,s);

  unsigned int label_end = label_index++;
  unsigned int label_ok = label_index++;  

  emit_bne(ACC,ZERO,label_ok,s);
  
  emit_load_string(ACC,stringtable.lookup_string(logger->get_current_filename()->get_string()),s);
  emit_load_imm(T1,get_line_number(),s);
  emit_jal("_case_abort2",s);
  
  emit_label_def(label_ok,s);

  emit_load(T2,TAG_OFFSET,ACC,s);

  std::vector<Case> case_vector;

  for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
      Case case_ = cases->nth(i);
      case_->update_type_tag_info(scope);

      case_vector.push_back(case_);

      for(auto k=case_vector.rbegin();
          k!=case_vector.rend()-1 
          && (*k)->class_tag > (*(k+1))->class_tag;
          ++k) {
        std::iter_swap(k,k+1);
      }
  }

  for(auto i=case_vector.begin();i!=case_vector.end();++i) {
    (*i)->code(label_end,frame,scope,s);
  }

  emit_jal("_case_abort",s);
  emit_label_def(label_end,s);

  return true;
}

void branch_class::update_type_tag_info(Scope* scope) {
  class_tag = scope->get_class_tag(type_decl);
  class_max_child_tag = scope->get_class_max_child_tag(type_decl);
}

bool branch_class::code(unsigned int label_end,Frame *frame,Scope * scope,ostream& s) {
  unsigned int label_id = label_index++;

  emit_blti(T2,class_tag,label_id,s);
  emit_bgti(T2,class_max_child_tag,label_id,s);

  ScopeGuard<Scope> scope_guard(scope);

  auto tmp = frame->reserve_temporary();
  scope->addid(name,tmp);

  tmp->code_store(ACC,s);
  expr->code(frame,scope,s);
  emit_branch(label_end,s);
  emit_label_def(label_id,s);

  frame->release_temporary(tmp);

  return true;
}

bool block_class::code(Frame *frame,Scope *scope,ostream &s) {
  for(int i = body->first(); body->more(i); i = body->next(i)) {
    Expression expr = body->nth(i);
    expr->code(frame,scope,s);
  }
  return true;
}

bool let_class::code(Frame *frame,Scope *scope,ostream &s) {
  bool has_init = init->code(frame,scope,s);
  if(!has_init) {
    if(Int == type_decl) {
      emit_load_int(ACC,inttable.lookup_string("0"),s);
    } else if(Str == type_decl) {
      emit_load_string(ACC,stringtable.lookup_string(""),s);
    } else if(Bool == type_decl) {
      emit_load_bool(ACC,falsebool,s);
    } else {
      emit_load_imm(ACC,EMPTYSLOT,s);
    }
  }

  ScopeGuard<Scope> scope_guard(scope);

  auto tmp = frame->reserve_temporary();
  scope->addid(identifier,tmp);  

  tmp->code_store(ACC,s);

  body->code(frame,scope,s);

  frame->release_temporary(tmp);

  return true;
}

bool plus_class::code(Frame *frame,Scope *scope,ostream &s) {
  return binary_operation(*this,emit_add,
                          scope->get_int_val_attr(),
                          frame,scope,s);
}

bool sub_class::code(Frame *frame,Scope *scope,ostream &s) {
  return binary_operation(*this,emit_sub,
                          scope->get_int_val_attr(),
                          frame,scope,s);
}

bool mul_class::code(Frame *frame,Scope *scope,ostream &s) {
  return binary_operation(*this,emit_mul,
                          scope->get_int_val_attr(),
                          frame,scope,s);
}

bool divide_class::code(Frame *frame,Scope *scope,ostream &s) {
  return binary_operation(*this,emit_div,
                          scope->get_int_val_attr(),
                          frame,scope,s);
}

bool neg_class::code(Frame *frame,Scope *scope,ostream &s) {
  return unary_operation(*this,emit_neg,
                         scope->get_int_val_attr(),
                         frame,scope,s);
}

bool lt_class::code(Frame *frame,Scope *scope,ostream &s) {
  return binary_comparison(*this,emit_blt,
                           scope->get_int_val_attr(),
                           frame,scope,s);
}

bool eq_class::code(Frame *frame,Scope *scope,ostream &s) {
  e1->code(frame,scope,s);

  const Variable *tmp1 = frame->reserve_temporary();
  if(!tmp1) return false;
  tmp1->code_store(ACC,s);

  e2->code(frame,scope,s);
  emit_move(T2,ACC,s);
  tmp1->code_load(T1,s);
  
  unsigned int label_id = label_index++;

  emit_load_bool(ACC,truebool,s);  
  emit_beq(T1,T2,label_id,s);
  emit_load_bool(A1,falsebool,s);
  emit_jal("equality_test",s);
  emit_label_def(label_id,s);

  frame->release_temporary(tmp1);

  return true;
}

bool leq_class::code(Frame *frame,Scope *scope,ostream &s) {
  return binary_comparison(*this,emit_bleq,
                           scope->get_int_val_attr(),
                           frame,scope,s);
}

bool comp_class::code(Frame *frame,Scope *scope,ostream &s) {
  auto value = scope->get_bool_val_attr();

  e1->code(frame,scope,s);
  value->code_load(T1,ACC,s);  
  
  unsigned int label_id = label_index++;

  emit_load_bool(ACC,truebool,s);  
  emit_beqz(T1,label_id,s);
  emit_load_bool(ACC,falsebool,s);
  emit_label_def(label_id,s);

  return true;
}

bool int_const_class::code(Frame *frame,Scope *scope,ostream &s)  
{
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
  return true;
}

bool string_const_class::code(Frame *frame,Scope *scope,ostream &s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
  return true;
}

bool bool_const_class::code(Frame *frame,Scope *scope,ostream &s)
{
  emit_load_bool(ACC, BoolConst(val), s);
  return true;
}

bool new__class::code(Frame *frame,Scope *scope,ostream &s) {
  if(type_name == SELF_TYPE) {
    emit_load_address(T1,"class_objTab",s);
    emit_load(T2,TAG_OFFSET,SELF,s);
    emit_sll(T2,T2,3,s); //*2*4
    emit_addu(T1,T1,T2,s);
    emit_move(S1,T1,s);
    emit_load(ACC,0,T1,s); //load class protObj pointer
    emit_jal("Object.copy",s);
    emit_load(T1,1,S1,s); //load class init pointer
    emit_jalr(T1,s);
  } else {
    s << LA << ACC << " " << type_name << PROTOBJ_SUFFIX << endl;
    s << JAL << "Object.copy" << endl;
    s << JAL << type_name << CLASSINIT_SUFFIX << endl;
  }
  return true;
}

bool isvoid_class::code(Frame *frame,Scope *scope,ostream &s) {
  e1->code(frame,scope,s);
  emit_move(T1,ACC,s);
  
  unsigned int label_id = label_index++;

  emit_load_bool(ACC,truebool,s);  
  emit_beqz(T1,label_id,s);
  emit_load_bool(ACC,falsebool,s);
  emit_label_def(label_id,s);

  return true;
}

bool no_expr_class::code(Frame *frame,Scope *scope,ostream &s) {
  return false;
}

bool object_class::code(Frame *frame,Scope *scope,ostream &s) {
  CurrentTreeNodeContext tree_node_context(logger,this);

  const Variable *var = scope->lookup(name);
  if(!var) {
    logger->report_error("Variable[%s] not found in current scope\n",name->get_string());
    return false;
  }

  var->code_load(ACC,s);

  return true;
}

/**************************************************************/

unsigned int assign_class::tmp_needed() {
  return expr->tmp_needed();
}

unsigned int static_dispatch_class::tmp_needed() {
  unsigned int tmp_count = expr->tmp_needed();

  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
      Expression arg = actual->nth(i);

      tmp_count = std::max(tmp_count,arg->tmp_needed());
  }

  return tmp_count;
}

unsigned int dispatch_class::tmp_needed() {
  unsigned int tmp_count = expr->tmp_needed();

  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
      Expression arg = actual->nth(i);

      tmp_count = std::max(tmp_count,arg->tmp_needed());
  }

  return tmp_count;
}

unsigned int cond_class::tmp_needed() {
  return std::max({pred->tmp_needed(),
                   then_exp->tmp_needed(),
                   else_exp->tmp_needed()});
}

unsigned int loop_class::tmp_needed() {
  return std::max({pred->tmp_needed(),
                   body->tmp_needed()});
}

unsigned int typcase_class::tmp_needed() {
  unsigned int tmp_count = expr->tmp_needed();

  for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
      Case case_ = cases->nth(i);

      tmp_count = std::max(tmp_count,case_->tmp_needed());
  }

  return tmp_count;
}

unsigned int branch_class::tmp_needed() {
  return 1+expr->tmp_needed();
}

unsigned int block_class::tmp_needed() {
  unsigned int tmp_count = 0;

  for(int i = body->first(); body->more(i); i = body->next(i)) {
      Expression expr = body->nth(i);

      tmp_count = std::max(tmp_count,expr->tmp_needed());
  }

  return tmp_count;
}

unsigned int let_class::tmp_needed() {
  return 1+std::max(init->tmp_needed(),body->tmp_needed());
}

unsigned int plus_class::tmp_needed() {
  return std::max(e1->tmp_needed(),1+e2->tmp_needed());
}

unsigned int sub_class::tmp_needed() {
  return std::max(e1->tmp_needed(),1+e2->tmp_needed());
}

unsigned int mul_class::tmp_needed() {
  return std::max(e1->tmp_needed(),1+e2->tmp_needed());
}

unsigned int divide_class::tmp_needed() {
  return std::max(e1->tmp_needed(),1+e2->tmp_needed());
}

unsigned int neg_class::tmp_needed() {
  return e1->tmp_needed();
}

unsigned int lt_class::tmp_needed() {
  return std::max(e1->tmp_needed(),1+e2->tmp_needed());
}

unsigned int eq_class::tmp_needed() {
  return std::max(e1->tmp_needed(),1+e2->tmp_needed());
}

unsigned int leq_class::tmp_needed() {
  return std::max(e1->tmp_needed(),1+e2->tmp_needed());
}

unsigned int comp_class::tmp_needed() {
  return e1->tmp_needed();
}

unsigned int bool_const_class::tmp_needed() {
  return 0;
}

unsigned int string_const_class::tmp_needed() {
  return 0;
}

unsigned int int_const_class::tmp_needed() {
  return 0;
}

unsigned int new__class::tmp_needed() {
  return 0;
}

unsigned int isvoid_class::tmp_needed() {
  return e1->tmp_needed();
}

unsigned int no_expr_class::tmp_needed() {
  return 0;
}

unsigned int object_class::tmp_needed() {
  return 0;
}
