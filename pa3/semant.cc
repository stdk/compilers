

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <set>
#include <algorithm>

extern int semant_debug;
extern char *curr_filename;

static bool debug_mode = getenv("DEBUG");

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

VarInfo* ClassInfo::get_attribute(Symbol name) {
    VarMap::iterator i = attr_map.find(name);
    if(i != attr_map.end()) {
        return i->second;
    }

    return 0;
}

MethodInfo* ClassInfo::get_method(Symbol name) {
    MethodMap::iterator i = method_map.find(name);
    if(i != method_map.end()) {
        return i->second;
    }

    return 0;
}

bool ClassInfo::add_attribute(AttrInfo *info,ILogger *logger) {
    if(info->name == self) {
        logger->report_error("Unallowed attribute name[%s]\n",info->name->get_string());
        return false;
    }

    if(attr_map.find(info->name) == attr_map.end()) {
        attr_map.insert(std::pair<Symbol,VarInfo*>(info->name,info));
        return true;
    } else {
        logger->report_error("Attribute name redefinition[%s]\n",
                                     info->name->get_string());
        return false;
    }
}

bool ClassInfo::add_method(MethodInfo *info,ILogger *logger) {
    std::set<Symbol> names;

    for(unsigned int i=0;i<info->args.size();++i) {
        VarInfo &arg = info->args[i];
        if(self == arg.name) {
            logger->report_error("Formal parameter cannot have type name[self]\n");
            return false;
        }

        if(SELF_TYPE == arg.type) {
            logger->report_error("Formal parameter[%s] cannot have type SELF_TYPE\n",
                                 arg.name->get_string());
            return false;
        }

        if(names.find(arg.name) == names.end()) {
            names.insert(arg.name);
        } else {
            logger->report_error("Duplicate name[%s] is formal parameters list\n",
                                 arg.name->get_string());
            return false;
        }
    }

    if(method_map.find(info->name) == method_map.end()) {
        method_map.insert(std::pair<Symbol,MethodInfo*>(info->name,info));
        return true;
    } else {
        logger->report_error("Method name redefinition[%s]\n",
                                    info->name->get_string());
        return false;
    }
}

std::string ClassInfo::to_string() const {
    char buffer[500];

    snprintf(buffer,sizeof(buffer),"%s:%s",name->get_string(),parent->get_string());

    return std::string(buffer);
}

bool ClassTable::check_class_inheritance(Symbol class_name,
                                         ILogger *logger) {
    ClassInfo *class_info = class_map.probe(class_name);
    if(!class_info) {
        logger->report_error("%s: No such class[%s]\n",
                                     __func__,
                                     class_name->get_string());
        return false;
    }

    std::set<Symbol> visited;
    visited.insert(class_info->name);

    while(class_info && class_info->parent != No_class) {
        if(visited.find(class_info->parent) != visited.end()) {
            char buffer[500] = {0};
            char *begin = buffer;
            char *end = buffer + sizeof(buffer);

            for(std::set<Symbol>::iterator i = visited.begin();
                begin < end && i != visited.end(); ++i) {
                begin += snprintf(begin,end-begin,"%s%s",(*i)->get_string(),",");
            }
            logger->report_error("%s: class tree forms a cycle containing [%s]\n",
                                 __func__,
                                 buffer);

            return false;
        } else {
            visited.insert(class_info->parent);
            ClassInfo *next_info = class_map.probe(class_info->parent);
            if(next_info) {
                class_info = next_info;
            } else {
                logger->report_error("%s: class[%s] parent[%s] does not exist\n",
                                     __func__,
                                     class_info->name->get_string(),
                                     class_info->parent->get_string());
                return false;
            }
        }
    }

    return true;
}

bool ClassTable::check_method_inheritance(Symbol class_name, 
                                          Symbol method_name,
                                          ILogger *logger) {
    ClassInfo *class_info = class_map.probe(class_name);
    if(!class_info) {
        logger->report_error("%s: class[%s] not found\n",
                                    __func__,
                                    class_name->get_string());
        return false;
    }

    MethodInfo *method_info = class_info->get_method(method_name);
    if(!method_info) {
        logger->report_error("%s: method[%s] not found in class[%s]\n",
                                    __func__,
                                    method_name->get_string(),
                                    class_name->get_string());
        return false;
    }

    ClassInfo *parent_info = class_map.probe(class_info->parent);
    if(!class_info) {
        logger->report_error("%s: class[%s] parent[%s] does not exist\n", 
                                     __func__,
                                     class_name->get_string(),
                                     class_info->parent->get_string());
        return false;
    }

    for(class_info = parent_info
        ;class_info && class_info->name != No_class
        ;class_info = parent_info) {
        MethodInfo* ancestor_method_info = class_info->get_method(method_name);
        if(ancestor_method_info && !method_info->same_signature_as(ancestor_method_info)) {
            logger->report_error("Attempt to incorrectly inherit method[%s] of class[%s] by method[%s] in child class[%s]\n",
                                        ancestor_method_info->to_string().c_str(),
                                        class_info->name->get_string(),
                                        method_info->to_string().c_str(),
                                        class_name->get_string());
            return false;
        }

        parent_info = class_map.probe(class_info->parent);
        if(!parent_info) {
            logger->debug("%s: class[%s] parent[%s] does not exist\n",
                                   __func__,
                                   class_info->name->get_string(),
                                   class_info->parent->get_string());
            return false;
        }
    }

    return true;
}

bool ClassTable::check_attribute_inheritance(Symbol class_name,
                                             Symbol attr_name,
                                             ILogger *logger) {
    ClassInfo *class_info = class_map.probe(class_name);
    if(!class_info) {
        logger->report_error("%s: class[%s] not found\n",
                                    __func__,
                                    class_name->get_string());
        return false;
    }

    VarInfo *attr_info = class_info->get_attribute(attr_name);
    if(!attr_info) {
        logger->report_error("%s: attr[%s] not found in class[%s]\n",
                                    __func__,
                                    attr_name->get_string(),
                                    class_name->get_string());
        return false;
    }

    ClassInfo *parent_info = class_map.probe(class_info->parent);
    if(!class_info) {
        logger->report_error("%s: class[%s] parent[%s] does not exist\n", 
                                     __func__,
                                     class_name->get_string(),
                                     class_info->parent->get_string());
        return false;
    }

    

    for(class_info = parent_info
        ;class_info && class_info->name != No_class
        ;class_info = parent_info) {
        if(class_info->get_attribute(attr_name)) {
            logger->report_error("Attempt to redefine attribute[%s] of class[%s] in child class[%s]\n",
                                        attr_name->get_string(),
                                        class_info->name->get_string(),
                                        class_name->get_string());
            return false;
        }

        parent_info = class_map.probe(class_info->parent);
        if(!parent_info) {
            logger->debug("%s: class[%s] parent[%s] does not exist\n",
                                   __func__,
                                   class_info->name->get_string(),
                                   class_info->parent->get_string());
            return false;
        }
    }
    
    return true;
}

bool ClassTable::class_name_exists(Symbol class_name) {
    if(No_class == class_name) {
        return false;
    }

    if(SELF_TYPE == class_name) {
        return true;
    }

    return class_map.probe(class_name);
}

VarScope* ClassTable::get_class_attr_scope(Symbol class_name) {
    ClassInfo *class_info = class_map.probe(class_name);
    if(!class_info) {
        return 0;
    }

    VarScope* scope = new VarScope();
    if(!scope) {
        logger.debug("%s: VarScope allocation failed.\n",__func__);
        return 0;
    }

    scope->enterscope();

    while(class_info && class_info->name != No_class) {
        
        for(VarMap::iterator i = class_info->attr_map.begin()
            ;i != class_info->attr_map.end();++i) {
            if(!scope->probe(i->first)) {
                scope->addid(i->first,i->second);
            }
        }            

        class_info = class_map.probe(class_info->parent);
    }

    return scope;
}

MethodScope* ClassTable::get_class_method_scope(Symbol class_name) {
    ClassInfo *class_info = class_map.probe(class_name);
    if(!class_info) {
        return 0;
    }

    MethodScope* scope = new MethodScope();
    if(!scope) {
        logger.debug("%s: MethodScope allocation failed.\n",__func__);
        return 0;
    }

    scope->enterscope();

    while(class_info && class_info->name != No_class) {

        for(MethodMap::iterator i = class_info->method_map.begin()
            ;i != class_info->method_map.end();++i) {
            if(!scope->probe(i->first)) {
                scope->addid(i->first,i->second);
            }
        }

        class_info = class_map.probe(class_info->parent);
    }

    return scope;
}

bool ClassTable::is_descendant(Symbol child_name, Symbol parent_name) {
    if(!child_name 
       || !parent_name 
       || child_name == No_type 
       || parent_name == No_type) {
        return false;
    }

    ClassInfo *class_info = class_map.probe(child_name);
    if(!class_info) {
        return false;
    }

    if(child_name == parent_name) {
        return true;
    }

    for(;class_info 
         && class_info->parent != parent_name 
         && class_info->parent!=No_class
        ;class_info = class_map.probe(class_info->parent));

    if(class_info->parent == parent_name) {
        return true;
    }

    return false;
}

Symbol ClassTable::common_ancestor(Symbol class_name1, Symbol class_name2) {

    if(!class_name1 
       || !class_name2 
       || class_name1 == No_type 
       || class_name2 == No_type) {
        return No_type;
    }    

    ClassInfo *class_info = class_map.probe(class_name1);
    if(!class_info) {
        return No_type;
    }

    std::set<Symbol> visited;

    while(class_info && class_info->name != No_class) {
        visited.insert(class_info->name);
        class_info = class_map.probe(class_info->parent);
    }

    /*for(std::set<Symbol>::iterator i=visited.begin()
        ;i != visited.end();++i) {
        logger.debug("visited[%s]\n",(*i)->get_string());
    }*/

    class_info = class_map.probe(class_name2);
    if(!class_info) {
        return No_type;
    }

    while(class_info && class_info->name != No_class) {
        if(visited.find(class_info->name) != visited.end()) {
            return class_info->name;
        }
        class_info = class_map.probe(class_info->parent);
    }    

    return No_type;
}

bool Scope::class_name_exists(Symbol class_name) {
    if(SELF_TYPE == class_name) {
        return true;
    }

    return checker->class_name_exists(class_name);
}

bool Scope::is_descendant(Symbol child_name, Symbol parent_name) {
    if(child_name == parent_name) {
        return true;
    }

    if(SELF_TYPE == parent_name) {
        return false;
    }

    if(SELF_TYPE == child_name) {
        child_name = class_name;
    }

    return checker->is_descendant(child_name,parent_name);
}

Symbol Scope::common_ancestor(Symbol class_name1, Symbol class_name2) {
    if(class_name1 == class_name2) {
        return class_name1;
    }

    if(SELF_TYPE == class_name1) {
        class_name1 = class_name;
    }    

    if(SELF_TYPE == class_name2) {
        class_name2 = class_name;
    }   

    return checker->common_ancestor(class_name1,class_name2);
}

MethodInfo* Scope::get_method(Symbol method_name) {    
    return method_scope->lookup(method_name);
}

MethodInfo* Scope::get_method(Symbol class_name, Symbol method_name) {
    if(SELF_TYPE == class_name) {
        return get_method(method_name);
    }

    MethodScope* class_method_scope = checker->get_class_method_scope(class_name);
    if(!method_scope) {
        return 0;
    }

    return class_method_scope->lookup(method_name);
}

bool ClassTable::first_pass(Classes classes) {
    class_map.enterscope();

    logger.debug("First pass\n");

    ClassInfo* class_info = new ClassInfo(No_class,No_class);
    if(!class_info) {
        logger.debug("%s: %s info allocation failed[%d]\n",
                               __func__,
                               No_class->get_string(),
                               sizeof(*class_info));
        return false;
    }

    class_map.addid(No_class,class_info);

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ class_ = classes->nth(i);
        CurrentFilenameContext filename_context(&logger,class_->get_filename());
        CurrentTreeNodeContext tree_node_context(&logger,class_);

        Symbol class_name = class_->get_name();
        Symbol parent_name = class_->get_parent();

        if(SELF_TYPE == class_name) {
            logger.report_error("SELF_TYPE cannot be redefined as user class\n");
            return false;
        }

        if(SELF_TYPE == parent_name 
           || Int == parent_name
           || Str == parent_name
           || Bool == parent_name) {
            logger.report_error("Class[%s] attempts to inherit from[%s]\n",
                                class_name->get_string(),
                                parent_name->get_string());
            return false;
        }

        ClassInfo* class_info = new ClassInfo(class_name,class_->get_parent());
        if(!class_info) {
            logger.report_error("%s: ClassInfo allocation failed[%d]\n",__func__,sizeof(*class_info));
            return false;
        }

        if(!class_map.probe(class_name)) {
            class_map.addid(class_name, class_info);
        } else {
            logger.report_error("Class name redefinition[%s]\n",class_name->get_string());
            delete class_info;
            return false;
        }

        logger.debug("Class[%s]\n",class_info->to_string().c_str());

        if(!class_->get_features_info(class_info,&logger)) {
            return false;
        }
    }

    return true;
}

bool ClassTable::second_pass(Classes classes) {
    logger.debug("Second pass\n");

    const bool self_check = true;
    bool result = true;

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ class_ = classes->nth(i);

        result = result && class_->semant(this,self_check,&logger);
    }

    if(result) {
        ClassInfo *class_info = class_map.probe(Main);
        if(!class_info) {
            logger.report_error("Class Main is not defined.\n");
            return false;
        }
    }

    return result;
}

bool ClassTable::third_pass(Classes classes) {
    logger.debug("Third pass\n");

    const bool self_check = false;
    bool result = true;

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ class_ = classes->nth(i);

        result = result && class_->semant(this,self_check,&logger);
    }

    return result;
}

ClassTable::ClassTable(Classes program_classes) {
    Classes classes = install_basic_classes(program_classes);

    if(!first_pass(classes)) {
        return;
    }

    if(!second_pass(program_classes)) {
        return;
    }

    if(!third_pass(program_classes)) {
        return;
    }
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

    return append_Classes(basic_classes,classes);
}

void Logger::report_error(tree_node *t, const char* format, ...) {
    char buffer[1000];

    buffer[0] = '\0';

    va_list args;
    va_start(args,format);
    vsnprintf(buffer,sizeof(buffer),format,args);
    va_end(args);

    if(current_filename) {
        error_stream << current_filename << ":";
    }

    if(t) {
        error_stream << t->get_line_number() << ": ";
    }

    error_stream << buffer;

    ++error_count;
}

void Logger::report_error(const char* format, ...) {
    char buffer[1000];
    
    buffer[0] = '\0';

    va_list args;
    va_start(args,format);
    vsnprintf(buffer,sizeof(buffer),format,args);
    va_end(args);

    if(current_filename) {
        error_stream << current_filename << ":";
    }

    if(current_tree_node) {
        error_stream << current_tree_node->get_line_number() << ": ";
    }

    error_stream << buffer;
                 
    ++error_count;
}

void Logger::debug(const char* format, ...) {
    char buffer[1000] = {0};

    if(debug_mode) {
        memset(buffer,' ',padding);
        buffer[padding] = '\0';
        error_stream << buffer;
    }
    
    buffer[0] = '\0';

    va_list args;
    va_start(args,format);
    vsnprintf(buffer,sizeof(buffer),format,args);
    va_end(args);

    if(debug_mode) {
        error_stream << buffer;
    }
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
	   if(!debug_mode) exit(1);
    }
}

/* extra methods implementation */

bool class__class::get_features_info(IFeatureInfoReporter* info_reporter,
                                     ILogger* logger) {
    CurrentFilenameContext filename_context(logger,filename);
    CurrentTreeNodeContext tree_node_context(logger,this);

    bool result = true;

    for(int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feature = features->nth(i);
        result = result && feature->get_feature_info(info_reporter,logger);
    }
    return result;
}

bool attr_class::get_feature_info(IFeatureInfoReporter* info_reporter,
                                  ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);

    AttrInfo *attr_info = new AttrInfo(name,type_decl);
    if(!attr_info) {
        logger->report_error(this,"%s: Memory allocation failure[%d]\n",
                                           __func__,
                                           sizeof(*attr_info));
        return false;
    }

    logger->debug("%s\n",attr_info->to_string().c_str());

    if(!info_reporter->add_attribute(attr_info,logger)) {
        return false;
    }

    return true;
}

bool method_class::get_feature_info(IFeatureInfoReporter* info_reporter,
                                    ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);

    MethodInfo *method_info = new MethodInfo(name,return_type);
    if(!method_info) {
        logger->report_error(this,"%s: Memory allocation failure[%d]\n",
                                          __func__,
                                          sizeof(*method_info));
        return false;
    }

    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal formal = formals->nth(i);

        VarInfo var_info(formal->get_name(),formal->get_type());
        method_info->args.push_back(var_info);
    }

    logger->debug("Method[%s]\n",method_info->to_string().c_str());

    if(!info_reporter->add_method(method_info,logger)) {
         return false;
    }

    return true;
}

bool class__class::semant(ISemanticChecker* checker,
                          bool self_check,
                          ILogger* logger) {
    CurrentFilenameContext filename_context(logger,filename);
    CurrentTreeNodeContext tree_node_context(logger,this);

    logger->debug("Semant[%s]\n",name->get_string());

    if(self_check && !checker->check_class_inheritance(name,logger)) {
        return false;
    }

    Scope scope(name,checker);
    scope.addid(self,SELF_TYPE);

    for(int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feature = features->nth(i);
        feature->semant(&scope,self_check,logger);
    }

    return true;   
}

bool attr_class::semant(Scope* scope,
                        bool self_check,
                        ILogger* logger) { 
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("Attribute[%s]\n",name->get_string());

    if(self_check && !scope->check_attribute_inheritance(name,logger)) {
        return false;
    }

    if(self_check) {
        return true;
    }

    if(!init->semant(scope,logger)) {
        return false;
    }

    Symbol init_type = init->get_type(logger);

    if(!init_type) {
        logger->report_error("Type of init expression in undefined\n");
        return false;
    }

    if(init_type!=No_type && !scope->is_descendant(init_type,type_decl)) {
        logger->report_error("Attribute[%s] type[%s] does not match init expression type[%s]\n",
                                     name->get_string(),
                                     type_decl->get_string(),
                                     init_type->get_string());
        return false;
    }

    return true;
}

bool method_class::semant(Scope* scope,
                          bool self_check,
                          ILogger* logger) { 
    CurrentTreeNodeContext tree_node_context(logger,this);

    MethodInfo *method = scope->get_method(name);
    if(!method) {
        logger->report_error("Method[%s] not found in class[%s] scope\n",
                             name->get_string(),
                             scope->self_type()->get_string());
        return false;
    }

    logger->debug("Method[%s]\n",method->to_string().c_str());

    if(self_check && !scope->check_method_inheritance(name,logger)) {
        return false;
    }

    if(self_check) {
        return true;
    }

    ScopeGuard<Scope> scope_guard(scope);

    for(std::vector<VarInfo>::iterator i=method->args.begin()
        ;i != method->args.end();++i) {
        scope->addid(i->name,i->type);
    }

    if(!expr->semant(scope,logger)) {
        return false;
    }

    Symbol expr_type = expr->get_type(logger);

    if(expr_type!=No_type && !scope->is_descendant(expr_type,return_type)) {
        logger->report_error("Method[%s] return type[%s] does not match its expression type[%s]\n",
                                     name->get_string(),
                                     return_type->get_string(),
                                     expr_type->get_string());
        return false;
    }

    return true;
}

Symbol branch_class::get_expr_type(ILogger *logger) {
    Symbol expr_type = expr->get_type(logger);

    if(!expr_type) {
        expr_type = No_type;
        logger->report_error(this,"Type of branch expression in undefined\n");
    }
    return expr_type;
}

Symbol branch_class::get_branch_type() {
    return type_decl;
}

Expression_class::Expression_class() { 
    type = No_type;
}

Symbol Expression_class::get_type(ILogger *logger) {
    if(!type) {
        set_type(No_type);
        logger->report_error(this,"Type of expression in undefined\n");
    }

    if(No_type == type) {
        logger->debug("Expression type inferred as _no_type\n");
    }

    return type;
}  

bool Expression_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("Generic expression::semant\n");

    return true;
}

bool assign_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("assign_class::semant\n");

    if(self == name) {
        logger->report_error("Assignment to self of prohibited\n");
        return false;
    }

    Symbol type = scope->lookup(name);
    if(!type) {
        logger->report_error("No variable[%s] in current scope\n",name->get_string());
        type = No_type;
        return false;
    }

    if(!expr->semant(scope,logger)) {
        return false;
    }
    Symbol expr_type = expr->get_type(logger);
    logger->debug("expr_type[%s]\n",expr_type->get_string());

    if(expr_type!=No_type && !scope->is_descendant(expr_type,type)) {
        logger->report_error("Variable[%s] type[%s] does not match its assigned expression type[%s]\n",
                                     name->get_string(),
                                     type->get_string(),
                                     expr_type->get_string());
        return false;
    }

    set_type(expr_type);

    return true;
}

bool static_dispatch_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("static_dispatch_class::semant\n");

    if(SELF_TYPE == type_name) {
        logger->report_error("SELF_TYPE cannot be used as a type name in static dispatch\n");
        return false;
    }

    if(!scope->class_name_exists(type_name)) {
        logger->report_error("Non-existent class[%s] in static dispatch\n",type_name->get_string());
        return false;
    }

    if(!expr->semant(scope,logger)) {
        return false;
    }

    Symbol expr_type = expr->get_type(logger);

    if(!scope->is_descendant(expr_type,type_name)) {
        logger->report_error("Expression type[%s] in static dispatch is not a descendant of a given type[%s]\n",
                              expr_type->get_string(),
                              type_name->get_string());
    }

    std::vector<Symbol> arg_types;
    for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Expression arg = actual->nth(i);
        if(!arg->semant(scope,logger)) {
            return false;
        }

        arg_types.push_back(arg->get_type(logger));        
    }

    MethodInfo* method = scope->get_method(type_name,name);
    if(!method) {
        logger->report_error("Method[%s] of class[%s] not found.\n",
                             name->get_string(),
                             expr_type->get_string());
        return false;
    }

    if(arg_types.size() != method->args.size()) {
        logger->report_error("Method[%s] of class[%s] called with[%d] arguments instead of [%d]\n",
                             method->to_string().c_str(),
                             expr_type->get_string(),
                             arg_types.size(),
                             method->args.size());
        return false;
    }

    for(unsigned int j = 0;j<arg_types.size();++j) {
        if(!scope->is_descendant(arg_types[j],method->args[j].type)) {
            logger->report_error("Argument #[%d] of method[%s] type[%s] does not descend from [%s]\n",
                                 j,
                                 method->to_string().c_str(),
                                 arg_types[j]->get_string(),
                                 method->args[j].type->get_string());
            return false;
        }
    }

    if(SELF_TYPE == method->return_type) {
        set_type(expr_type);
    } else {
        set_type(method->return_type);    
    }    

    return true;
}

bool dispatch_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("dispatch_class::semant\n");

    if(!expr->semant(scope,logger)) {
        return false;
    }

    Symbol expr_type = expr->get_type(logger);

    std::vector<Symbol> arg_types;
    for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Expression arg = actual->nth(i);
        if(!arg->semant(scope,logger)) {
            return false;
        }

        arg_types.push_back(arg->get_type(logger));        
    }

    MethodInfo* method = scope->get_method(expr_type,name);
    if(!method) {
        logger->report_error("Method[%s] of class[%s] not found.\n",
                             name->get_string(),
                             expr_type->get_string());
        return false;
    }

    if(arg_types.size() != method->args.size()) {
        logger->report_error("Method[%s] of class[%s] called with[%d] arguments instead of [%d]\n",
                             method->to_string().c_str(),
                             expr_type->get_string(),
                             arg_types.size(),
                             method->args.size());
        return false;
    }

    for(unsigned int j = 0;j<arg_types.size();++j) {
        if(!scope->is_descendant(arg_types[j],method->args[j].type)) {
            logger->report_error("Argument #[%d] of method[%s] type[%s] does not descend from [%s]\n",
                                 j,
                                 method->to_string().c_str(),
                                 arg_types[j]->get_string(),
                                 method->args[j].type->get_string());
            return false;
        }
    }

    if(SELF_TYPE == method->return_type) {
        set_type(expr_type);
    } else {
        set_type(method->return_type);
    } 

    return true;
}

bool cond_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("cond_class::semant\n");

    pred->semant(scope,logger);

    Symbol pred_type = pred->get_type(logger);

    if(pred_type != Bool) {
        logger->report_error("Conditional predicate expression has type[%s] instead of Bool\n",
                                     pred_type->get_string());
        return false;
    }

    then_exp->semant(scope,logger);

    Symbol then_type = then_exp->get_type(logger);

    else_exp->semant(scope,logger);

    Symbol else_type = else_exp->get_type(logger);

    Symbol common_type = scope->common_ancestor(then_type,else_type);
    if(No_type == common_type) {
        logger->report_error("Common ancestor of then branch[%s] and else branch[%s] is _no_type\n",
                             then_type->get_string(),
                             else_type->get_string());
        return false;
    }

    set_type(common_type);

    return true;
}

bool loop_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("loop_class::semant\n");

    pred->semant(scope,logger);

    Symbol pred_type = pred->get_type(logger);

    if(pred_type != Bool) {
        logger->report_error("Loop predicate expression has type[%s] instead of Bool\n",
                                     pred_type->get_string());
    }

    body->semant(scope,logger);

    Symbol body_type = body->get_type(logger);

    set_type(Object);

    return true;
}

bool branch_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("branch_class::semant\n");

    if(name == self) {
        logger->report_error("Unallowed branch variable name[%s]\n",name->get_string());
        return false;
    }

    if(!scope->class_name_exists(type_decl)) {        
        logger->report_error("Non-existent type[%s] of variable[%s]\n",
                                     type_decl->get_string(),
                                     name->get_string());
        return false;
    }

    ScopeGuard<Scope> scope_guard(scope);
    scope->addid(name,type_decl);

    if(!expr->semant(scope,logger)) {
        return false;
    }

    return true;
}

bool typcase_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("typcase_class::semant\n");

    if(!expr->semant(scope,logger)) {
        return false;
    }
    Symbol expr_type = expr->get_type(logger);

    int i = cases->first();
    Case case_ = cases->nth(i);

    if(!case_->semant(scope,logger)) {
        return false;
    }

    Symbol common_type = case_->get_expr_type(logger);

    std::set<Symbol> branch_types;
    branch_types.insert(case_->get_branch_type());

    for(i = cases->next(i); cases->more(i); i = cases->next(i)) {
        case_ = cases->nth(i);
        if(!case_->semant(scope,logger)) {
            return false;
        }

        Symbol branch_type = case_->get_branch_type();
        if(branch_types.find(branch_type) != branch_types.end()) {
            logger->report_error(case_,"Duplicate branch[%s] in case statement\n",branch_type->get_string());
            return false;
        }

        Symbol current_type = case_->get_expr_type(logger);

        common_type = scope->common_ancestor(common_type,current_type);
    }

    set_type(common_type);

    return true;
}

bool block_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("block_class::semant\n");

    for(int i = body->first(); body->more(i); i = body->next(i)) {
        Expression expr = body->nth(i);
        expr->semant(scope,logger);

        set_type(expr->get_type(logger));
    }

    return true;
}

bool let_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("let_class::semant\n");

    if(identifier == self) {
        logger->report_error("Unallowed let binding variable name[%s]\n",identifier->get_string());
    }

    if(!scope->class_name_exists(type_decl)) {        
        logger->report_error("Non-existent type[%s] of variable[%s]\n",
                                     type_decl->get_string(),
                                     identifier->get_string());
    } 

    init->semant(scope,logger);

    Symbol init_type = init->get_type(logger);

    if(init_type != No_type && !scope->is_descendant(init_type,type_decl)) {
        logger->report_error("Variable[%s] type[%s] does not match init expression type[%s]\n",
                                     identifier->get_string(),
                                     type_decl->get_string(),
                                     init_type->get_string());
    }

    ScopeGuard<Scope> scope_guard(scope);
    scope->addid(identifier,type_decl);

    body->semant(scope,logger);

    Symbol body_type = body->get_type(logger);

    set_type(body_type);

    return true;
}

bool plus_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("plus_class::semant\n");

    e1->semant(scope,logger);
    Symbol e1_type = e1->get_type(logger);

    e2->semant(scope,logger);
    Symbol e2_type = e2->get_type(logger);

    if(Int == e1_type && Int == e2_type) {
        set_type(Int);
    } else {
        logger->report_error("Type of plus expression with e1[%s] and e2[%s] inferred as _no_type\n",
                                     e1_type->get_string(),
                                     e1_type->get_string());
        set_type(No_type);
    }

    return true;
}

bool sub_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("sub_class::semant\n");

    e1->semant(scope,logger);
    Symbol e1_type = e1->get_type(logger);

    e2->semant(scope,logger);
    Symbol e2_type = e2->get_type(logger);


    if(Int == e1_type && Int == e2_type) {
        set_type(Int);
    } else {
        logger->report_error("Type of sub expression with e1[%s] and e2[%s] inferred as _no_type\n",
                                     e1_type->get_string(),
                                     e1_type->get_string());
        set_type(No_type);
    }

    return true;
}

bool mul_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("mul_class::semant\n");

    e1->semant(scope,logger);
    Symbol e1_type = e1->get_type(logger);

    e2->semant(scope,logger);
    Symbol e2_type = e2->get_type(logger);

    if(Int == e1_type && Int == e2_type) {
        set_type(Int);
    } else {
        logger->report_error("Type of mul expression with e1[%s] and e2[%s] inferred as _no_type\n",
                                     e1_type->get_string(),
                                     e1_type->get_string());
        set_type(No_type);
    }

    return true;
}

bool divide_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("divide_class::semant\n");

    e1->semant(scope,logger);
    Symbol e1_type = e1->get_type(logger);

    e2->semant(scope,logger);
    Symbol e2_type = e2->get_type(logger);

    if(Int == e1_type && Int == e2_type) {
        set_type(Int);
    } else {
        logger->report_error("Type of divide expression with e1[%s] and e2[%s] inferred as _no_type\n",
                                     e1_type->get_string(),
                                     e1_type->get_string());
        set_type(No_type);
    }

    return true;
}

bool neg_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("neg_class::semant\n");

    e1->semant(scope,logger);

    Symbol e1_type = e1->get_type(logger);

    if(Int == e1_type) {
        set_type(Int);
    } else {
        logger->report_error("Type of neg expression with e1[%s] inferred as _no_type\n",
                                     e1_type->get_string());
        set_type(No_type);
    }

    return true;
}

bool lt_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("lt_class::semant\n");

    e1->semant(scope,logger);
    Symbol e1_type = e1->get_type(logger);

    e2->semant(scope,logger);
    Symbol e2_type = e2->get_type(logger);

    if(Int == e1_type && Int == e2_type) {
        set_type(Bool);
    } else {
        logger->report_error("< operator accepts only Int on both sides(e1[%s] e2[%s])\n",
                             e1_type->get_string(),
                             e1_type->get_string());
        set_type(No_type);
    }

    return true;
}

bool eq_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("eq_class::semant\n");

    e1->semant(scope,logger);
    Symbol e1_type = e1->get_type(logger);

    e2->semant(scope,logger);
    Symbol e2_type = e2->get_type(logger);

    std::set<Symbol> primitive_types;
    primitive_types.insert(Int);
    primitive_types.insert(Str);
    primitive_types.insert(Bool);

    if(primitive_types.find(e1_type) != primitive_types.end() 
        || primitive_types.find(e2_type) != primitive_types.end()) {
        if(e1_type != e2_type) {
            logger->report_error("Primitive types in equality operator must be the same(e1[%s] e2[%s])\n",
                                 e1_type->get_string(),
                                 e1_type->get_string());
            return false;
        }
    }

    set_type(Bool);

    return true;
}

bool leq_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("leq_class::semant\n");

    e1->semant(scope,logger);
    Symbol e1_type = e1->get_type(logger);

    e2->semant(scope,logger);
    Symbol e2_type = e2->get_type(logger);

    if(Int == e1_type && Int == e2_type) {
        set_type(Bool);
    } else {
        logger->report_error("<= operator accepts only Int on both sides(e1[%s] e2[%s])\n",
                             e1_type->get_string(),
                             e1_type->get_string());
        set_type(No_type);
    }

    return true;
}

bool comp_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("comp_class::semant\n");

    e1->semant(scope,logger);
    Symbol e1_type = e1->get_type(logger);

    if(Bool == e1_type) {
        set_type(Bool);
    } else {
        logger->report_error("not operator accepts only Bool(e1[%s])\n",
                             e1_type->get_string());
        set_type(No_type);
    }

    return true;
}

bool int_const_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("int_const_class::semant\n");

    set_type(Int);

    return true;
}

bool bool_const_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("bool_const_class::semant\n");

    set_type(Bool);

    return true;
}

bool string_const_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("string_const_class::semant\n");

    set_type(Str);

    return true;
}

bool new__class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("new__class::semant\n");

    if(scope->class_name_exists(type_name)) {
        set_type(type_name);        
    } else {
        logger->report_error("Non-existent class[%s] in new expression\n",type_name->get_string());
        set_type(No_type);
    }    

    return true;
}

bool isvoid_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("isvoid_class::semant\n");

    e1->semant(scope,logger);
    Symbol e1_type = e1->get_type(logger);

    if(e1_type != No_type) {
        set_type(Bool);
    } else {
        set_type(No_type);
    }

    set_type(Bool);

    return true;
}

bool no_expr_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("no_expr_class::semant\n");

    set_type(No_type);

    return true;
}

bool object_class::semant(Scope* scope,ILogger* logger) {
    CurrentTreeNodeContext tree_node_context(logger,this);
    logger->debug("object_class::semant\n");

    Symbol type = scope->lookup(name);
    if(!type) {
        logger->report_error("Undefined variable[%s]\n",name->get_string());
        set_type(No_type);
        return false;
    }

    set_type(type);

    return true;
}