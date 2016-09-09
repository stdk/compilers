#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
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

typedef SymbolTable<Symbol, AttrInfo> AttrMap;
typedef SymbolTable<Symbol, MethodInfo> MethodMap;

struct ClassInfo : public IFeatureInfoReporter {
    Symbol name;
    Symbol parent;
    AttrMap attr_map;
    MethodMap method_map;

    ClassInfo(Symbol _name,Symbol _parent):name(_name),parent(_parent) {
        attr_map.enterscope();
        method_map.enterscope();
    }

    bool add_attribute(AttrInfo *info) {
        if(!attr_map.probe(info->name)) {
            attr_map.addid(info->name, info);
            return true;
        } else {
            return false;
        }
    }

    bool add_method(MethodInfo *info) {
        if(!method_map.probe(info->name)) {
            method_map.addid(info->name, info);
            return true;
        } else {
            return false;
        }
    }

    void dump() {
        fprintf(stderr,"Class[%s]:[%s]\n",name->get_string(),parent->get_string());        
    }
};

typedef SymbolTable<Symbol,ClassInfo> ClassMap;

class ClassTable : public IErrorReporter {
private:
  int semant_errors;

  ClassMap class_map;

  Classes classes;

  bool check_method_inheritance(Symbol class_name, Symbol method_name);
  bool check_attribute_inheritance(Symbol class_name, Symbol attr_name);

  bool first_pass(Classes classes);
  bool second_pass(Classes classes);

  Classes install_basic_classes(Classes);

  Symbol current_filename;
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }

  Classes get_classes() const {
    return classes;
  }

  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  /* IErrorReporter */
  void report_error(tree_node *t, const char* format, ...) __attribute__ ((format (printf, 3, 4)));
  void report(const char* format, ...) __attribute__ ((format (printf, 2, 3)));
};


#endif

