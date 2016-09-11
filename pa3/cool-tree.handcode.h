//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include <vector>
#include <string>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#include "symtab.h"
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

struct ILogger {
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

struct VarInfo {
    Symbol name;
    Symbol type;

    VarInfo(Symbol _name, Symbol _type)
    :name(_name),type(_type) {

    }

    virtual std::string to_string() const {
        char buffer[500];

        snprintf(buffer,sizeof(buffer),"Var[%s:%s]",
                                       name->get_string(),
                                       type->get_string());

        return std::string(buffer);
    }

    bool operator==(const VarInfo &other) const {
        return name==other.name && type == other.type;
    }

    bool operator==(Symbol var_name) const {
        return name==var_name;
    }
};

typedef SymbolTable<Symbol,VarInfo> VarScope;

struct AttrInfo : public VarInfo {
    AttrInfo(Symbol name, Symbol type):VarInfo(name,type) {
    
    }

    virtual std::string to_string() const {
        char buffer[500];

        snprintf(buffer,sizeof(buffer),"Attr[%s:%s]",
                                       name->get_string(),
                                       type->get_string());

        return std::string(buffer);
    }
};

struct MethodInfo {
    Symbol name;
    Symbol return_type;
    std::vector<VarInfo> args; 

    MethodInfo(Symbol _name, Symbol _return_type)
    :name(_name),return_type(_return_type) {

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

    bool same_signature_as(const MethodInfo *other)  const{
        if(return_type != other->return_type) {
            return false;
        }

        if(args.size() != other->args.size()) {
            return false;
        }

        for(unsigned int i=0;i<args.size();++i) {
            if(args[i].type != other->args[i].type) {
                return false;
            }
        }

        return true;
    }
};

typedef SymbolTable<Symbol,MethodInfo> MethodScope;

struct IFeatureInfoReporter {
    virtual bool add_attribute(AttrInfo *,ILogger *) = 0;
    virtual bool add_method(MethodInfo *,ILogger *) = 0;
};

class PaddingContext {
protected:
    ILogger *error_reporter;
public:
    PaddingContext(ILogger *_error_reporter):error_reporter(_error_reporter) {
        error_reporter->increase_debug_padding();
    }

    ~PaddingContext() {
        error_reporter->decrease_debug_padding();
    }
};

class CurrentFilenameContext {
    ILogger *error_reporter;
    Symbol prev_filename;
public:
    CurrentFilenameContext(ILogger *_error_reporter,Symbol filename)
    :error_reporter(_error_reporter) {
        //error_reporter->report("CurrentFilenameContext[%s]\n",filename->get_string());
        prev_filename = error_reporter->get_current_filename();
        error_reporter->set_current_filename(filename);
    }

    ~CurrentFilenameContext() {
        //error_reporter->report("~CurrentFilenameContext[%s]\n",error_reporter->get_current_filename()->get_string());
        error_reporter->set_current_filename(prev_filename);
    }    
};

class CurrentTreeNodeContext : public PaddingContext{
    tree_node *prev_tree_node;
public:
    CurrentTreeNodeContext(ILogger *_error_reporter,tree_node *node)
    :PaddingContext(_error_reporter) {
        //error_reporter->report("CurrentTreeNodeContext[%p]\n",node);
        prev_tree_node = error_reporter->get_current_tree_node();
        error_reporter->set_current_tree_node(node);
    }

    ~CurrentTreeNodeContext() {
        //error_reporter->report("~CurrentTreeNodeContext[%p]\n",error_reporter->get_current_tree_node());
        error_reporter->set_current_tree_node(prev_tree_node);
    }    
};

struct ISemanticChecker {
    virtual bool check_class_inheritance(Symbol class_name,ILogger *_error_reporter) = 0;
    virtual bool check_method_inheritance(Symbol class_name, Symbol method_name,ILogger *_error_reporter) = 0;
    virtual bool check_attribute_inheritance(Symbol class_name, Symbol attr_name,ILogger *_error_reporter) = 0;
    virtual bool class_name_exists(Symbol class_name) = 0;
    virtual VarScope* get_class_attr_scope(Symbol class_name) = 0;
    virtual MethodScope* get_class_method_scope(Symbol class_name) = 0;
    virtual bool is_descendant(Symbol child_name, Symbol parent_name) = 0;
    virtual Symbol common_ancestor(Symbol class_name1, Symbol class_name2) = 0;
};

class Scope {
    Symbol class_name;
    ISemanticChecker *checker;
    VarScope *var_scope;
    MethodScope *method_scope;
public:
    Scope(Symbol _class_name, ISemanticChecker *_checker)
    :class_name(_class_name),
     checker(_checker),
     var_scope(checker->get_class_attr_scope(class_name)),
     method_scope(checker->get_class_method_scope(class_name)) {

    }

    Symbol self_type() const {
        return class_name;
    }

    bool class_name_exists(Symbol class_name);

    bool check_method_inheritance(Symbol method_name,ILogger *error_reporter) {
        return checker->check_method_inheritance(class_name,method_name,error_reporter);
    }

    bool check_attribute_inheritance(Symbol attribute_name,ILogger *error_reporter) {
        return checker->check_attribute_inheritance(class_name,attribute_name,error_reporter);
    }

    bool is_descendant(Symbol child_name, Symbol parent_name);
    Symbol common_ancestor(Symbol class_name1, Symbol class_name2);

    MethodInfo* get_method(Symbol method_name);
    MethodInfo* get_method(Symbol class_name, Symbol method_name);
    
    void enterscope() {
        var_scope->enterscope();
    }

    void exitscope() {
        var_scope->exitscope();
    }

    void addid(Symbol name, Symbol type) {
        VarInfo *var_info = new VarInfo(name,type);
        if(!var_info) {
            fprintf(stderr,"Scope::addid: Allocation of VarInfo failed\n");
            return;
        }
        var_scope->addid(name,var_info);
    }

    Symbol lookup(Symbol name) {
        VarInfo *var_info = var_scope->lookup(name);
        if(!var_info) {
            return 0;
        }

        return var_info->type;
    }

    MethodInfo* lookup_method(Symbol name) {
        return method_scope->lookup(name);
    }
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

#define Program_EXTRAS              \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&, int) = 0; 



#define program_EXTRAS          \
void semant();     				\
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                            \
virtual Symbol get_filename() = 0;               \
virtual Symbol get_name() = 0;                   \
virtual Symbol get_parent() = 0;                 \
virtual Features get_features() = 0;             \
virtual bool get_features_info(IFeatureInfoReporter*,ILogger*) = 0; \
virtual bool semant(ISemanticChecker*,bool,ILogger*) = 0; \
virtual void dump_with_types(ostream&,int) = 0; 


#define class__EXTRAS                         \
Symbol get_filename() { return filename; }        \
Symbol get_name() { return name; }            \
Symbol get_parent() { return parent; }        \
Features get_features() { return features; }  \
bool get_features_info(IFeatureInfoReporter*,ILogger*); \
bool semant(ISemanticChecker*,bool,ILogger*); \
void dump_with_types(ostream&,int);                    


#define Feature_EXTRAS                          \
virtual bool semant(Scope*,bool,ILogger*) = 0; \
virtual bool get_feature_info(IFeatureInfoReporter*,ILogger*) = 0; \
virtual void dump_with_types(ostream&,int) = 0; 


#define Feature_SHARED_EXTRAS        \
void dump_with_types(ostream&,int);    

#define method_EXTRAS \
bool semant(Scope*,bool,ILogger*); \
bool get_feature_info(IFeatureInfoReporter*,ILogger*); 

#define attr_EXTRAS \
bool semant(Scope*,bool,ILogger*); \
bool get_feature_info(IFeatureInfoReporter*,ILogger*);



#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;    \
virtual Symbol get_name() = 0;                     \
virtual Symbol get_type() = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int);             \
Symbol get_name() { return name; }              \
Symbol get_type() { return type_decl; }


#define Case_EXTRAS                               \
virtual Symbol get_expr_type(ILogger*) = 0;       \
virtual Symbol get_branch_type() = 0;     \
virtual bool semant(Scope*,ILogger*) = 0;         \
virtual void dump_with_types(ostream& ,int) = 0;


#define branch_EXTRAS                   \
void dump_with_types(ostream& ,int);    \
Symbol get_expr_type(ILogger*);         \
Symbol get_branch_type();       \
bool semant(Scope*,ILogger*);


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type(ILogger*);                           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class(); \
virtual bool semant(Scope*,ILogger*);


#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int); 

#define assign_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define static_dispatch_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define dispatch_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define cond_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define loop_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define typcase_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define block_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define let_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define plus_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define sub_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define mul_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define divide_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define neg_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define lt_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define eq_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define leq_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define comp_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define int_const_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define bool_const_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define string_const_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define new__EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define isvoid_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define no_expr_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#define object_EXTRAS \
virtual bool semant(Scope*,ILogger*);

#endif
