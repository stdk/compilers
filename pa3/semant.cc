

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
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

bool ClassTable::check_method_inheritance(Symbol class_name, Symbol method_name) {
    ClassInfo *class_info = class_map.probe(class_name);
    if(!class_info) {
        fprintf(stderr,"%s: class[%s] not found\n",
                       __func__,
                       class_name->get_string());
        return false;
    }

    MethodInfo *method_info = class_info->method_map.probe(method_name);
    if(!method_info) {
        fprintf(stderr,"%s: method[%s] not found in class[%s]\n",
                       __func__,
                       method_name->get_string(),
                       class_name->get_string());
        return false;
    }

    for(class_info = class_map.probe(class_info->parent);
        class_info && class_info->name != No_class;
        class_info = class_map.probe(class_info->parent)) {

        MethodInfo* ancestor_method_info = class_info->method_map.probe(method_name);
        if(ancestor_method_info && !method_info->same_signature_as(ancestor_method_info)) {
            fprintf(stderr,"Attempt to incorrectly inherit method[%s] of class[%s] in child class[%s]\n",
                           method_name->get_string(),
                           class_info->name->get_string(),
                           class_name->get_string());
            semant_error();
            return false;
        }
    }

    return class_info->name == No_class;
}

bool ClassTable::check_attribute_inheritance(Symbol class_name, Symbol attr_name) {
    ClassInfo *class_info = class_map.probe(class_name);
    if(!class_info) {
        fprintf(stderr,"%s: class[%s] not found\n",
                       __func__,
                       class_name->get_string());
        return false;
    }

    AttrInfo *attr_info = class_info->attr_map.probe(attr_name);
    if(!attr_info) {
        fprintf(stderr,"%s: attr[%s] not found in class[%s]\n",
                       __func__,
                       attr_name->get_string(),
                       class_name->get_string());
        return false;
    }

    for(class_info = class_map.probe(class_info->parent);
        class_info && class_info->name != No_class;
        class_info = class_map.probe(class_info->parent)) {
        if(class_info->attr_map.probe(attr_name)) {
            fprintf(stderr,"Attempt to redefine attribute[%s] of class[%s] in child class[%s]\n",
                           attr_name->get_string(),
                           class_info->name->get_string(),
                           class_name->get_string());
            semant_error();
            return false;
        }
    }
    
    return class_info->name == No_class;
}

bool ClassTable::first_pass(Classes classes) {
    class_map.enterscope();

    fprintf(stderr,"First pass\n");

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ class_ = classes->nth(i);
        current_filename = class_->get_filename();

        Symbol class_name = class_->get_name();

        ClassInfo* class_info = new ClassInfo(class_name,class_->get_parent());
        if(!class_info) {
            report("%s: ClassInfo allocation failed[%d]\n",__func__,sizeof(*class_info));
            return false;
        }

        if(!class_map.probe(class_name)) {
            class_map.addid(class_name, class_info);
        } else {
            report_error(class_,"Class name redefinition[%s]\n",class_name->get_string());
            return false;
        }

        class_info->dump();

        class_->get_features_info(class_info,this);        
    }

    return true;
}

bool ClassTable::second_pass(Classes classes) {
    fprintf(stderr,"Second pass\n");

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ class_ = classes->nth(i);
        current_filename = class_->get_filename();

        Symbol class_name = class_->get_name();
        fprintf(stderr,"[%s]:[%s]\n",class_name->get_string(),class_->get_parent()->get_string());

        Features features = class_->get_features();
        for(int i = features->first(); features->more(i); i = features->next(i)) {
            Feature feature = features->nth(i);
            
        }
    }

    return true;
}

ClassTable::ClassTable(Classes program_classes) : semant_errors(0) , error_stream(cerr) {
    Classes classes = install_basic_classes(program_classes);

    first_pass(classes);

    second_pass(program_classes);
}

Classes ClassTable::install_basic_classes(Classes classes) {

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

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
    Classes basic_classes = single_Classes(Object_class);
    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
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
	       filename);
    basic_classes = append_Classes(basic_classes,single_Classes(IO_class));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
    basic_classes = append_Classes(basic_classes,single_Classes(Int_class));
    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    basic_classes = append_Classes(basic_classes,single_Classes(Bool_class));
    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
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
	       filename);
    basic_classes = append_Classes(basic_classes,single_Classes(Str_class));

    return append_Classes(classes,basic_classes);
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

void ClassTable::report_error(tree_node *t, const char* format, ...) {
    char buffer[1000];
    buffer[0] = '\0';

    va_list args;
    va_start(args,format);
    vsnprintf(buffer,sizeof(buffer),format,args);
    va_end(args);

    error_stream << current_filename << ":" << t->get_line_number() << ": " << buffer;
    semant_errors++;
}

void ClassTable::report(const char* format, ...) {
    char buffer[1000];
    buffer[0] = '\0';

    va_list args;
    va_start(args,format);
    vsnprintf(buffer,sizeof(buffer),format,args);
    va_end(args);

    error_stream << current_filename << ":" << buffer;
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
    ClassTable *classtable = new ClassTable(classes);

    //classes = classtable->get_classes();

    if (classtable->errors()) {
	   cerr << "Compilation halted due to static semantic errors." << endl;
	   exit(1);
    }
}

/* extra methods implementation */

bool class__class::get_features_info(IFeatureInfoReporter* info_reporter,
                                     IErrorReporter* error_reporter) {
    for(int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feature = features->nth(i);
        feature->get_feature_info(info_reporter,error_reporter);
    }
    return true;
}


void attr_class::semant() { 
    fprintf(stderr,"attr[%s]\n",name->get_string());
}

bool attr_class::get_feature_info(IFeatureInfoReporter* info_reporter,
                                  IErrorReporter* error_reporter) {
    AttrInfo *attr_info = new AttrInfo(name,type_decl);
    if(!attr_info) {
        error_reporter->report_error(this,"%s: Memory allocation failure[%d]\n",
                                           __func__,
                                           sizeof(*attr_info));
        return false;
    }

    attr_info->dump();

    if(!info_reporter->add_attribute(attr_info)) {
        error_reporter->report_error(this,"Attribute name redefinition[%s]\n",
                                    attr_info->name->get_string());
        return false;
    }

    return true;
}

void method_class::semant() { 
    fprintf(stderr,"method[%s]\n",name->get_string());
}

bool method_class::get_feature_info(IFeatureInfoReporter* info_reporter,
                                    IErrorReporter* error_reporter) {
    MethodInfo *method_info = new MethodInfo(name,return_type);
    if(!method_info) {
        error_reporter->report_error(this,"%s: Memory allocation failure[%d]\n",
                                          __func__,
                                          sizeof(*method_info));
        return false;
    }

    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal formal = formals->nth(i);
        method_info->args.push_back(formal->get_type());
    }

    method_info->dump();

    if(!info_reporter->add_method(method_info)) {
        error_reporter->report_error(this,"Method name redefinition[%s]\n",
                                          method_info->name->get_string());
        return false;
    }

    return true;
}


