#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include <map>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

//typedef SymbolTable<Symbol, MethodInfo> MethodMap;
typedef std::map<Symbol,VarInfo*> VarMap;
typedef std::map<Symbol,MethodInfo*> MethodMap;

struct ClassInfo : public IFeatureInfoReporter {
    Symbol name;
    Symbol parent;
    VarMap attr_map;
    MethodMap method_map;

    ClassInfo(Symbol _name,Symbol _parent):name(_name),parent(_parent) {

    }

    VarInfo* get_attribute(Symbol name);
    MethodInfo* get_method(Symbol name);

    bool add_attribute(AttrInfo *info,ILogger *logger);
    bool add_method(MethodInfo *info,ILogger *logger);

    std::string to_string() const;
};

typedef SymbolTable<Symbol,ClassInfo> ClassMap;

class Logger : public ILogger {
    unsigned int padding;
    unsigned int error_count;
    ostream& error_stream;    
    Symbol current_filename;
    tree_node *current_tree_node;

public:
    Logger():padding(0),error_count(0),error_stream(cerr),
        current_filename(static_cast<Symbol>(NULL)),
        current_tree_node(static_cast<tree_node*>(NULL)) { }



    virtual unsigned int errors() const { 
        return error_count;
    }   

    virtual Symbol get_current_filename() const {
        return current_filename;
    }
    virtual tree_node* get_current_tree_node() const {
        return current_tree_node;
    }
    virtual void set_current_filename(Symbol filename) {
        current_filename = filename;
    }
    virtual void set_current_tree_node(tree_node *node) {
        current_tree_node = node;
    }

    virtual void increase_debug_padding() {
        padding += 2;
    }

    virtual void decrease_debug_padding() {
        if(padding >= 2) {
            padding -=2;
        } else {
            padding = 0;
        }
    }

    virtual void report_error(const char* format, ...) __attribute__ ((format (printf, 2, 3)));    
    virtual void report_error(tree_node *t, const char* format, ...) __attribute__ ((format (printf, 3, 4)));
    virtual void debug(const char* format, ...) __attribute__ ((format (printf, 2, 3)));

};

class ClassTable : public ISemanticChecker {
private:
  Logger logger;

  ClassMap class_map;

  Classes classes;

  bool first_pass(Classes classes);
  bool second_pass(Classes classes);
  bool third_pass(Classes classes);

  Classes install_basic_classes(Classes);
public:
  ClassTable(Classes);
  
  unsigned int errors() const {
    return logger.errors();
  }

  Classes get_classes() const {
    return classes;
  }

    /* ISemanticChecker */
  virtual bool check_class_inheritance(Symbol class_name,ILogger *logger);
  virtual bool check_method_inheritance(Symbol class_name, Symbol method_name,ILogger *logger);
  virtual bool check_attribute_inheritance(Symbol class_name, Symbol attr_name,ILogger *logger);
  virtual bool class_name_exists(Symbol class_name);
  virtual VarScope* get_class_attr_scope(Symbol class_name);
  virtual MethodScope* get_class_method_scope(Symbol class_name);
  virtual bool is_descendant(Symbol child_name, Symbol parent_name);
  virtual Symbol common_ancestor(Symbol class_name1, Symbol class_name2);
  
};

#endif

