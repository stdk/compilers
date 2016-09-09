//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include <vector>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
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

struct AttrInfo {
    Symbol name;
    Symbol type_;

    AttrInfo(Symbol _name, Symbol _type)
    :name(_name),type_(_type) {

    }

    void dump() const {
        fprintf(stderr,"Attr[%s]:[%s]\n",name->get_string(),type_->get_string());
    }
};

struct MethodInfo {
    Symbol name;
    Symbol return_type;
    std::vector<Symbol> args; 

    MethodInfo(Symbol _name, Symbol _return_type)
    :name(_name),return_type(_return_type) {

    }

    void dump() const {
        char buffer[500] = {0};
        char *begin = buffer;
        char *end = buffer + sizeof(buffer)/sizeof(*buffer);

        for(size_t i=0;i<args.size();++i) {
            begin += snprintf(begin,end-begin,"%s%s",args[i]->get_string(),
                                                     (i==args.size()-1?"":","));
        }

        fprintf(stderr,"Method[%s](%s):[%s]\n",name->get_string(),
                                               buffer,
                                               return_type->get_string());
    }

    bool same_signature_as(const MethodInfo *other)  const{
        return return_type == other->return_type
               && args == other->args;
    }
};

struct IFeatureInfoReporter {
    virtual bool add_attribute(AttrInfo *info) = 0;
    virtual bool add_method(MethodInfo *info) = 0;
};

struct IErrorReporter {
    virtual void report_error(tree_node *t, const char* format, ...) __attribute__ ((format (printf, 3, 4))) = 0;
    virtual void report(const char* format, ...) __attribute__ ((format (printf, 2, 3))) = 0;
};

#define Program_EXTRAS                          \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&, int) = 0; 



#define program_EXTRAS                          \
void semant();     				\
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                            \
virtual Symbol get_filename() = 0;               \
virtual Symbol get_name() = 0;                   \
virtual Symbol get_parent() = 0;                 \
virtual Features get_features() = 0;             \
virtual bool get_features_info(IFeatureInfoReporter*,IErrorReporter*) = 0; \
virtual void dump_with_types(ostream&,int) = 0; 


#define class__EXTRAS                         \
Symbol get_filename() { return filename; }        \
Symbol get_name() { return name; }            \
Symbol get_parent() { return parent; }        \
Features get_features() { return features; }  \
bool get_features_info(IFeatureInfoReporter*,IErrorReporter*); \
void dump_with_types(ostream&,int);                    


#define Feature_EXTRAS                          \
virtual void semant() = 0;                      \
virtual bool get_feature_info(IFeatureInfoReporter*,IErrorReporter*) = 0; \
virtual void dump_with_types(ostream&,int) = 0; 


#define Feature_SHARED_EXTRAS        \
void dump_with_types(ostream&,int);    

#define method_EXTRAS \
void semant(); \
bool get_feature_info(IFeatureInfoReporter*,IErrorReporter*); 

#define attr_EXTRAS \
void semant(); \
bool get_feature_info(IFeatureInfoReporter*,IErrorReporter*);



#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;    \
virtual Symbol get_name() = 0;                     \
virtual Symbol get_type() = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int);             \
Symbol get_name() { return name; }              \
Symbol get_type() { return type_decl; }


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0;


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int);


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; }

#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int); 

#endif
