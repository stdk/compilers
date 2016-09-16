#include <assert.h>
#include <stdio.h>
#include <stdarg.h>

#include "cgen_support.h"
#include "stringtab.h"

static bool is_debug_mode() {
  char *debug_env = getenv("DEBUG");
  return debug_env && !strcmp(debug_env,"1");
}
bool debug_mode = is_debug_mode();

static int ascii = 0;

void ascii_mode(ostream& str)
{
  if (!ascii) 
    {
      str << "\t.ascii\t\"";
      ascii = 1;
    } 
}

void byte_mode(ostream& str)
{
  if (ascii) 
    {
      str << "\"\n";
      ascii = 0;
    }
}

void emit_string_constant(ostream& str, char* s)
{
  ascii = 0;

  while (*s) {
    switch (*s) {
    case '\n':
      ascii_mode(str);
      str << "\\n";
      break;
    case '\t':
      ascii_mode(str);
      str << "\\t";
      break;
    case '\\':
      byte_mode(str);
      str << "\t.byte\t" << (int) ((unsigned char) '\\') << endl;
      break;
    case '"' :
      ascii_mode(str);
      str << "\\\"";
      break;
    default:
      if (*s >= ' ' && ((unsigned char) *s) < 128) 
	{
	  ascii_mode(str);
	  str << *s;
	}
      else 
	{
	  byte_mode(str);
	  str << "\t.byte\t" << (int) ((unsigned char) *s) << endl;
	}
      break;
    }
    s++;
  }
  byte_mode(str);
  str << "\t.byte\t0\t" << endl;
}

std::string Attribute::to_string() const {
  char buffer[500];

  snprintf(buffer,sizeof(buffer),"Attribute[%s:%s#%d]",
                                 name->get_string(),
                                 type->get_string(),
                                 index);

  return std::string(buffer);
}

std::string Argument::to_string() const {
  char buffer[500];

  snprintf(buffer,sizeof(buffer),"Argument[%s:%s#%d/%d]",
                                 name->get_string(),
                                 type->get_string(),
                                 index,
                                 total);

  return std::string(buffer);
}

std::string StackTemporary::to_string() const {
  char buffer[500];

  snprintf(buffer,sizeof(buffer),"StackTemporary[%s#%d]",
                                 name->get_string(),
                                 index);

  return std::string(buffer);
}

std::string Self::to_string() const {
  char buffer[500];

  snprintf(buffer,sizeof(buffer),"Self[%s:%s]",
                                 name->get_string(),
                                 type->get_string());

  return std::string(buffer);
}

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

    virtual ~Logger() {

    }

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

ILogger::~ILogger() {

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

ILogger *logger = new Logger();
