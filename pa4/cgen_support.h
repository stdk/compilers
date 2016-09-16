#ifndef CGEN_SUPPORT_H
#define CGEN_SUPPORT_H

#include <string>
#include <vector>
#include <map>
#include "cool.h"
#include "tree.h"
#include "symtab.h"

extern bool debug_mode;

struct ILogger {
    virtual ~ILogger() = 0;

    virtual Symbol get_current_filename() const = 0;
    virtual tree_node* get_current_tree_node() const = 0;
    virtual void set_current_filename(Symbol) = 0;
    virtual void set_current_tree_node(tree_node *) = 0;    

    virtual unsigned int errors() const = 0;
    virtual void report_error(tree_node *t, const char* format, ...) __attribute__ ((format (printf, 3, 4))) = 0;
    virtual void report_error(const char* format, ...) __attribute__ ((format (printf, 2, 3))) = 0;
         
    virtual void increase_debug_padding() = 0;
    virtual void decrease_debug_padding() = 0;
    virtual void debug(const char* format, ...) __attribute__ ((format (printf, 2, 3))) = 0;
};

extern ILogger *logger;

struct Variable {
    Symbol name;
    Symbol type;

    Variable(Symbol _name, Symbol _type)
    :name(_name),type(_type) {

    }

    virtual void code_load(char* reg,ostream& s) const = 0;
    virtual void code_store(char* reg,ostream& s) const = 0;

    virtual std::string to_string() const = 0;
};

typedef SymbolTable<Symbol,const Variable> VarScope;

class Expression_class;
typedef Expression_class *Expression;

struct Attribute : public Variable {
    Expression init_expr;

    Symbol class_name;
    unsigned int index;    

    Attribute(Symbol name, Symbol type,Expression expr)
    :Variable(name,type),init_expr(expr),class_name(0),index(0) {
    
    }

    virtual void code_load(char* reg,ostream& s) const;
    virtual void code_store(char* reg,ostream& s) const;

    void code_load(char* reg,char *src_reg,ostream& s) const;
    void code_store(char* reg,char *src_reg,ostream& s) const;

    virtual std::string to_string() const;
};

struct Argument : public Variable {
    unsigned int index;
    unsigned int total;

    Argument(Symbol name, Symbol type, unsigned int _index, unsigned int _total)
    :Variable(name,type),index(_index),total(_total) {

    }

    virtual void code_load(char* reg, ostream& s) const;
    virtual void code_store(char* reg,ostream& s) const;

    virtual std::string to_string() const;
};

struct StackTemporary : public Variable {
    unsigned int index;

    StackTemporary(unsigned int _index);

    virtual void code_load(char* reg, ostream& s) const;
    virtual void code_store(char* reg,ostream& s) const;

    virtual std::string to_string() const;
};

struct Self : public Variable {
    Self(Symbol name, Symbol type)
    :Variable(name,type) {

    }

    virtual void code_load(char *reg, ostream &s) const;
    virtual void code_store(char* reg,ostream& s) const;

    virtual std::string to_string() const;
};

class Scope;

struct Method {
    Symbol name;
    Symbol return_type;
    std::vector<Argument> args;
    Expression expr;

    Symbol class_name;
    int index;

    Method(Symbol _name, Symbol _return_type,Expression _expr)
    :name(_name),return_type(_return_type),expr(_expr),
     class_name(NULL),index(-1) {

    }

    std::string to_string() const {
        char args_buffer[500] = {0};
        char *begin = args_buffer;
        char *end = args_buffer + sizeof(args_buffer)/sizeof(*args_buffer);

        for(size_t i=0; begin < end && i<args.size();++i) {
            begin += snprintf(begin,end-begin,"%s%s",args[i].name->get_string(),
                                                     (i==args.size()-1?"":","));
        }

        char buffer[1000] = {0};
        snprintf(buffer,sizeof(buffer),"%s(%s):%s",
                                       name->get_string(),
                                       args_buffer,
                                       return_type->get_string());

        return std::string(buffer);
    }

    void code(Scope*,ostream&) const;
};

struct IFeatureInfoStorage {
    virtual bool add_attribute(Attribute*) = 0;
    virtual bool add_method(Method*) = 0;
};

class PaddingContext {
protected:
    ILogger *logger;
public:
    PaddingContext(ILogger *_logger):logger(_logger) {
        logger->increase_debug_padding();
    }

    ~PaddingContext() {
        logger->decrease_debug_padding();
    }
};

class CurrentFilenameContext {
    ILogger *logger;
    Symbol prev_filename;
public:
    CurrentFilenameContext(ILogger *_logger,Symbol filename)
    :logger(_logger) {
        prev_filename = logger->get_current_filename();
        logger->set_current_filename(filename);
    }

    ~CurrentFilenameContext() {
        logger->set_current_filename(prev_filename);
    }    
};

class CurrentTreeNodeContext : public PaddingContext{
    tree_node *prev_tree_node;
public:
    CurrentTreeNodeContext(ILogger *_logger,tree_node *node)
    :PaddingContext(_logger) {
        prev_tree_node = logger->get_current_tree_node();
        logger->set_current_tree_node(node);
    }

    ~CurrentTreeNodeContext() {
        logger->set_current_tree_node(prev_tree_node);
    }    
};

class Frame {
  ostream &s;
  unsigned int arg_count;
  unsigned int tmp_count;
  unsigned int tmp_used;
  std::vector<StackTemporary> tmps;
public:
  Frame(unsigned int _arg_count,unsigned int _tmp_count,ostream &);
  ~Frame();

  const Variable* reserve_temporary();
  void release_temporary(const Variable*);
};

struct IStaticInfo {
    virtual VarScope* get_class_scope(Symbol class_name) const = 0;
    virtual const Attribute* lookup_attribute(Symbol class_name, Symbol attr_name) const = 0;
    virtual const Method* lookup_method(Symbol class_name, Symbol method_name) const = 0;
    virtual unsigned int get_class_tag(Symbol class_name) const = 0;
    virtual unsigned int get_class_max_child_tag(Symbol class_name) const = 0;
};

class Scope {
    Symbol current_class_name;
    IStaticInfo *static_info;
    VarScope *var_scope;
    Attribute int_val_attr;
    Attribute bool_val_attr;
public:
    Scope(Symbol class_name, IStaticInfo *_static_info);

    const Attribute* get_int_val_attr() const {
        return &int_val_attr;
    }

    const Attribute* get_bool_val_attr() const {
        return &bool_val_attr;
    }

    unsigned int get_class_tag(Symbol class_name) const {
        return static_info->get_class_tag(class_name);
    }

    unsigned int get_class_max_child_tag(Symbol class_name) const {
        return static_info->get_class_max_child_tag(class_name);
    }

    void enterscope() {
        var_scope->enterscope();
    }

    void exitscope() {
        var_scope->exitscope();
    }

    void addid(Symbol name, const Variable* var) {
        var_scope->addid(name,var);
    }

    const Variable* lookup(Symbol name) {
        return var_scope->lookup(name);
    }

    const Attribute* lookup_attribute(Symbol class_name,Symbol attr_name) const {
        return static_info->lookup_attribute(class_name,attr_name);
    }

    const Method* lookup_method(Symbol class_name,Symbol method_name) const;
};

template<class T>
class ScopeGuard {
    T *scope;
public:
    ScopeGuard(T *_scope):scope(_scope) {
        scope->enterscope();
    }

    ~ScopeGuard() {
        scope->exitscope();
    }
};

typedef std::map<Symbol,Attribute*> AttrMap;
typedef std::map<Symbol,Method*> MethodMap;

typedef std::vector<Attribute*> AttrVector;
typedef std::vector<Method*> MethodVector;

#endif