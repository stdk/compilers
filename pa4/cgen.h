#include <vector>
#include <map>

#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

typedef std::vector<CgenNodeP> CgenNodeVector;
typedef std::map<Symbol,CgenNodeP> CgenNodeMap;
typedef std::map<unsigned int,CgenNodeP> CgenNodeTagMap;

class CgenClassTable : public IStaticInfo {
private:
   CgenNodeVector nodes;
   CgenNodeMap node_map;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;

   CgenNodeP get_node_by_name(Symbol name) const;
   bool add_node(CgenNodeP node);

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   void class_name_tab();
   void class_obj_tab();
   void class_disp_tab(CgenNodeP);
   void class_prot_obj(CgenNodeP);
   void class_init(CgenNodeP);
   void class_functions(CgenNodeP);

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP node);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP node);   
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();

   VarScope* get_class_scope(Symbol class_name) const;
   const Attribute* lookup_attribute(Symbol class_name, Symbol attr_name) const;
   const Method* lookup_method(Symbol class_name, Symbol method_name) const;
   unsigned int get_class_tag(Symbol class_name) const;
   unsigned int get_class_max_child_tag(Symbol class_name) const;
};


class CgenNode : public class__class, public IFeatureInfoStorage {
private: 
  static unsigned int class_tag_index;

  CgenNodeP parent_node;                   
  Basicness basic_status;                   

  CgenNodeVector children;

  AttrVector attrs;
  MethodVector methods;

  AttrMap attr_map;
  MethodMap method_map;
                                            
  StringEntryP string_name;
  unsigned int tag;
  unsigned int max_child_tag;

  bool add_attribute(Attribute *info);
  bool add_method(Method *info);
public:
  CgenNode(Class_ class_,
          Basicness bstatus,
          CgenClassTableP class_table);

  int is_basic() const { return (basic_status == Basic); }

  StringEntryP get_string_name() const { return string_name; }
  unsigned int get_tag() const { return tag; }
  unsigned int get_max_child_tag() { return max_child_tag; }
  unsigned int get_object_size() const { return DEFAULT_OBJFIELDS + attrs.size(); }

  void add_child(CgenNodeP child) { children.push_back(child); }
  const CgenNodeVector& get_children() { return children; }
  
  void set_parent_node(CgenNodeP p) { parent_node = p; }
  CgenNodeP get_parent_node() { return parent_node; }

  void inherit(CgenNodeP parent);

  const AttrVector& get_attrs() const { return attrs; }
  const MethodVector& get_methods() const { return methods; };

  const Attribute* get_attr_by_name(Symbol name) const;
  const Method* get_method_by_name(Symbol name) const;

  void code_init(IStaticInfo*,ostream&) const;
  void code_func(IStaticInfo*,ostream&) const;
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