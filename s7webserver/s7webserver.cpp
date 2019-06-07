/*
  Written by Kjetil Matheussen: k.s.matheussen@notam02.no

  See s7webserver.h for instructions on how to compile the server.
*/


#include <unistd.h>

#include "s7.h"

#define COMPILING_S7WEBSERVER 1
#include "s7webserver.h"
#include "moc_s7webserver.cpp"

#include <QCoreApplication>

#include <qhttpserver.h>
#include <qhttprequest.h>
#include <qhttpresponse.h>


S7WebServer::S7WebServer(s7_scheme *s7, int portnumber)
  : s7(s7)
  , portnumber(portnumber)
  , verbose(false)
  , very_verbose(false)
{
  QHttpServer *server = new QHttpServer(this);
  connect(server, SIGNAL(newRequest(QHttpRequest*, QHttpResponse*)),
          this, SLOT(handleRequest(QHttpRequest*, QHttpResponse*)));
  
  has_started = server->listen(QHostAddress::Any, portnumber);
}


void S7WebServer::handleRequest(QHttpRequest *req, QHttpResponse *resp)
{
  new S7WebServerResponder(this, req, resp);
}


// function written by Rick Taube for common music (https://ccrma.stanford.edu/software/snd/snd/s7.html#repl)
static bool is_balanced(std::string str)
{
  int parens = 0;
  int quotes = 0;
  unsigned i = 0;
  while (i < str.size())
    {
      if (str[i] == ';')
	{
	  for (i = i + 1; i < str.size(); i++)
	    {
	      if (str[i] == '\n')
		break;
	    }
	}
      else if (str[i] == '"')
	{
	  if (i == 0 || str[i - 1] != '\\')
	    {
	      quotes = 1;
	      for (i = i + 1; i < str.size(); i++)
		{
		  if (str[i] == '"' && str[i - 1] != '\\')
		    {
		      quotes = 0;
		      break;
		    }
		}
	      if (quotes)
		return false;
	    }
	}
      else if (str[i] == '(')
	parens++;
      else if (str[i] == ')')
	parens--;
      i++;
    }
  return (parens == 0) && (quotes == 0);
}


// function written by Rick Taube for common music (https://ccrma.stanford.edu/software/snd/snd/s7.html#repl)
static bool is_white(std::string str)
{
  for (unsigned i = 0; (i < str.size() && str[i] != ';'); i++)
    if (str[i] != ' ' && str[i] != '\n' && str[i] != '\t')
      return false;
  return true;
}


S7WebServerResponder::S7WebServerResponder(S7WebServer *s7webserver, QHttpRequest *request, QHttpResponse *response)
  : s7webserver(s7webserver)
  , request(request)
  , response(response)
{
  if (s7webserver->very_verbose)
    printf("responder got something\n");
  
  response->setHeader("Content-Type", "text/plain");
  response->setHeader("Access-Control-Allow-Origin", "*");

  response->writeHead(200);
  
  connect(request, SIGNAL(data(const QByteArray&)), this, SLOT(accumulate(const QByteArray&)));
  connect(request, SIGNAL(end()), this, SLOT(reply()));
  connect(response, SIGNAL(done()), this, SLOT(deleteLater()));
}


void S7WebServerResponder::accumulate(const QByteArray &data)
{
  s7webserver->input_code_so_far += data.data();
  if (s7webserver->very_verbose)
    printf("code so far: %s\n",s7webserver->input_code_so_far.c_str());
}


static void my_print(s7_scheme *sc, unsigned char c, s7_pointer port)
{
  
  S7WebServerResponder *current_responder = static_cast<S7WebServerResponder*>(s7_c_pointer(s7_name_to_value(sc, "s7webserver-current-responder")));

  if (current_responder==NULL || current_responder->s7webserver->verbose)
    putchar(c);

  if (current_responder != NULL) {
    current_responder->response->write(QByteArray().append(c));
    current_responder->response->flush();
    current_responder->response->waitForBytesWritten();
  }
}


static void set_s7webserver_current_responder(s7_scheme *sc, S7WebServerResponder *responder){
  s7_pointer current_responder = s7_make_c_pointer(sc, responder);
  s7_symbol_set_value(sc, s7_make_symbol(sc, "s7webserver-current-responder"), current_responder);
}

void S7WebServerResponder::reply()
{
  if (s7webserver->very_verbose)
    printf("Code so far: -%s-\n",s7webserver->input_code_so_far.c_str());

  if (!is_balanced(s7webserver->input_code_so_far)) {
    response->end(QByteArray("")); //-unbalanced, waiting for more input-"));
    s7webserver->input_code_so_far += "\n";
    return;
  }

  if(is_white(s7webserver->input_code_so_far)) {
    response->end(QByteArray(""));
    s7webserver->input_code_so_far = "";
    return;
  }

  // Code in here mostly copied from   https://ccrma.stanford.edu/software/snd/snd/s7.html#Cerrors


  // evaluate with error handling
  {
    s7_scheme *s7 = s7webserver->s7;
    
    int gc_loc = -1;

    /* trap error messages */
    s7_pointer old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
    if (old_port != s7_nil(s7))
      gc_loc = s7_gc_protect(s7, old_port);
        

    {
      // call eval
      set_s7webserver_current_responder(s7, this);

      s7_pointer result = s7_eval_c_string(s7, s7webserver->input_code_so_far.c_str());

      set_s7webserver_current_responder(s7, NULL);

      {
        const char *result_as_string = s7_object_to_c_string(s7, result);
        if (s7webserver->verbose)
          printf("result: %s\n",result_as_string);
        
        {
          QByteArray array("result: ");
          array.append(QByteArray(result_as_string));

          {
            /* look for error messages */
            const char *errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
            
            /* if we got something, wrap it in "[]" */
            if ((errmsg) && (*errmsg)) {
              if (s7webserver->verbose)
                fprintf(stdout, "error message: [%s]\n", errmsg);
              array.append(QByteArray(errmsg));
            }
          }
          
          response->end(array);
        }
      }
    }

    s7_close_output_port(s7, s7_current_error_port(s7));
    s7_set_current_error_port(s7, old_port);

    if (gc_loc != -1)
      s7_gc_unprotect_at(s7, gc_loc);
  }

  s7webserver->input_code_so_far = "";
}


S7WebServer *s7webserver_create(s7_scheme *s7, int portnum, bool find_first_free_portnum){
  
  // Start QCoreApplication if it hasn't already been started.
  if (QCoreApplication::instance()==NULL){
    static int argc = 1;
    static const char *argv[1] = {"snd"};
    new QCoreApplication(argc, (char**)argv);
  }

  s7_define_variable(s7, "s7webserver-current-responder", s7_make_c_pointer(s7, NULL));
  
  s7_set_current_output_port(s7, s7_open_output_function(s7, my_print));

 try_again:
  
  S7WebServer *s7webserver = new S7WebServer(s7, portnum); 

  if (s7webserver->has_started==false) {
    delete s7webserver;
    s7webserver = NULL;
    
    if (find_first_free_portnum) {
      printf("s7webserver: Failed to open port %d, trying port %d instead.\n",portnum,portnum+1);
      portnum++;
      goto try_again;
    }
  }

  return s7webserver;
}

void s7webserver_call_very_often(void){
  QCoreApplication::processEvents();
}

void s7webserver_set_verbose(S7WebServer *s7webserver, bool verbose) {
  s7webserver->verbose = verbose;
}


void s7webserver_set_very_verbose(S7WebServer *s7webserver, bool very_verbose) {
  s7webserver->very_verbose = very_verbose;
}

int s7webserver_get_portnumber(S7WebServer *s7webserver){
  return s7webserver->portnumber;
}

void s7webserver_delete(S7WebServer *s7webserver){
  delete s7webserver;
}


#ifdef WITH_MAIN

#define OPTARGS_CHECK_GET(wrong,right) (lokke==argc-1?(fprintf(stderr,"Must supply argument for '%s'\n",argv[lokke]),exit(-4),wrong):right)

#define OPTARGS_BEGIN(das_usage) {int lokke;const char *usage=das_usage;for(lokke=1;lokke<argc;lokke++){char *a=argv[lokke];if(!strcmp("--help",a)||!strcmp("-h",a)){fprintf(stderr,"%s",usage);exit(0);
#define OPTARG(name,name2) }}else if(!strcmp(name,a)||!strcmp(name2,a)){{
#define OPTARG_GETINT() OPTARGS_CHECK_GET(0,atoll(argv[++lokke]))
//int optargs_inttemp;
//#define OPTARG_GETINT() OPTARGS_CHECK_GET(0,(optargs_inttemp=strtol(argv[++lokke],(char**)NULL,10),errno!=0?(perror("strtol"),0):optargs_inttemp))
#define OPTARG_GETFLOAT() OPTARGS_CHECK_GET(0.0f,atof(argv[++lokke]))
#define OPTARG_GETSTRING() OPTARGS_CHECK_GET("",argv[++lokke])
#define OPTARG_GETBOOL() ({const char *response = OPTARG_GETSTRING(); !strcasecmp(response,"false") ? false : !strcasecmp(response,"true") ? true : (fprintf(stderr,"Argument for '%s' must be \"false\" or \"true\"\n",argv[lokke-1]), exit(-5) , false);})
#define OPTARG_LAST() }}else if(lokke==argc-1 && argv[lokke][0]!='-'){lokke--;{
#define OPTARGS_ELSE() }else if(1){
#define OPTARGS_END }else{fprintf(stderr,"%s",usage);exit(-1);}}}

static const char *g_usage_string = ""
  "Usage: s7webserver [--verbose] [--very-verbose] [--search-for-first-free-portnum false-or-true] [portnumber]\n" 
  "\n"                                                                  
  "Default values:\n"                                                  
  "  verbose: false\n"                                                 
  "  very-verbose: false\n"                                            
  "  search-for-first-free-portnum: true\n"
  "  portnumber: 6080\n"                                               
  "\n"                                                                 
  "If \'search-for-first-free-portnum\' is set, s7webserver will search for the next free port number starting at \'portnumber\'.\n" 
  "\n";
                                                                        
int main(int argc, char **argv){

  int portnumber = 6080;
  bool verbose = false;
  bool very_verbose = false;
  bool find_first_free_portnum = true;

  OPTARGS_BEGIN(g_usage_string)
  {
    OPTARG("--verbose","-v") verbose = true;
    OPTARG("--very-verbose","-vv") very_verbose = true;
    OPTARG("--search-for-first-free-portnum", "-s") find_first_free_portnum=OPTARG_GETBOOL();
    OPTARG_LAST() portnumber=OPTARG_GETINT();
  }OPTARGS_END;

  QCoreApplication app(argc, argv);

  s7_scheme *s7 = s7_init();
  if (s7==NULL) {
    fprintf(stderr, "Can't start s7 scheme");
    return -2;
  }
  
  s7webserver_t *s7webserver = s7webserver_create(s7, portnumber, find_first_free_portnum);
  if (s7webserver==NULL){
    fprintf(stderr, "Unable to start server. Port may be in use\n");
    return -3;
  }
  
  s7webserver_set_verbose(s7webserver, verbose);
  s7webserver_set_very_verbose(s7webserver, very_verbose);
  
  printf("S7 server started on port %d. (verbose=%s) (very_verbose=%s) (--search-for-first-free-portnum=%s)\n", s7webserver->portnumber, s7webserver->verbose==true?"true":"false", s7webserver->very_verbose==true?"true":"false", find_first_free_portnum==true?"true":"false");

  app.exec();
}
#endif
