;;; makegl.scm creates the GL/GLU bindings using gldata.scm, writes gl.c

(define gl-file (open-output-file "gl.c"))

(define-expansion (hey . args)
  (cons 'format (cons 'gl-file args)))

(define (heyc arg)
  (display arg gl-file))

(define names ())
(define types ())
(define ints ())
(define funcs ())

(define x-types ())
(define x-funcs ())
(define x-ints ())
;(define g-types ())
;(define g-funcs ())
;(define g-ints ())
;(define g5-funcs ())
;(define g5-ints ())

(define in-glu #f)

(define (check-glu name)
  (if in-glu
      (if (not (string=? "glu" (substring name 0 3)))
	  (begin
	    (set! in-glu #f)
	    (hey "#endif~%")))
      (if (string=? "glu" (substring name 0 3))
	  (begin
	    (set! in-glu #t)
	    (hey "#if HAVE_GLU~%")))))

(define (uncheck-glu)
  (if in-glu
      (begin
	(hey "#endif~%")
	(set! in-glu #f))))

(define (cadr-str data)
  (let* ((sp1 (char-position #\space data))
	 (sp2 (char-position #\space data (+ sp1 1))))
    (substring data (if sp2 (values (+ sp1 1) sp2) sp1))))

(define (caddr-str data)
  (let* ((sp2 (char-position #\space data (+ (char-position #\space data) 1)))
	 (sp3 (char-position #\space data (+ sp2 1))))
    (substring data (if sp3 (+ sp2 1) sp2))))

(define (car-str data)
  (let ((sp (char-position #\space data)))
    (if sp
	(substring data 0 sp)
	data)))

(define (ref-arg? arg)
  (and (= (length arg) 3)
       (string? (caddr arg))))

(define (null-arg? arg)
  (and (= (length arg) 3)
       (eq? (caddr arg) 'null)))

(define (opt-arg? arg)
  (and (= (length arg) 3)
       (eq? (caddr arg) 'opt)))

(define (ref-args args)
  (let ((ctr 0))
    (for-each
     (lambda (arg)
       (if (ref-arg? arg)
	   (set! ctr (+ 1 ctr))))
     args)
    ctr))

(define (opt-args args)
  (let ((ctr 0))
    (for-each
     (lambda (arg)
       (if (opt-arg? arg)
	   (set! ctr (+ 1 ctr))))
     args)
    ctr))

(define (deref-type arg)
  (let ((type (car arg)))
    (substring type 0 (- (length type) 1))))

(define (deref-name arg)
  (string-append "ref_" (cadr arg)))

(define (has-stars type)
  (let ((len (length type)))
    (call-with-exit
     (lambda (return)
       (do ((i (- len 1) (- i 1)))
	   ((= i 0))
	 (if (char=? (type i) #\*)
	     (return #t)))
       #f))))

(define (no-stars type)
  (if (string=? type "Display*")
      (copy "Display")
      (if (string=? type "XVisualInfo*")
	  (copy "XVisualInfo")
	  (let ((len (length type))
		(val (copy type)))
	    (do ((i 0 (+ i 1)))
		((= i len) val)
	      (if (char=? (val i) #\*)
		  (set! (val i) #\_)))))))

(define* (parse-args args x)
  (if (string=? args "void")
      ()
      (do ((data ())
	   (sp -1)
	   (type #f)
	   (len (length args))
	   (i 0 (+ i 1)))
	  ((= i len) 
	   (reverse data))
	(let ((ch (args i)))
	  (when (or (char=? ch #\space)
		    (= i (- len 1)))
	    (if type
		(begin
		  (let ((given-name (substring args (+ 1 sp) (if (= i (- len 1)) (+ i 1) i))))
		    (case (given-name 0)
		      ((#\@) (set! data (cons (list type (substring given-name 1) 'null) data)))
		      ((#\#) (set! data (cons (list type (substring given-name 1) 'opt) data)))
		      ((#\[)
		       (set! data (cons (list type (substring given-name 1 (- (length given-name) 1)) given-name) data))
		       (set! type (deref-type (list type))))
		      (else (set! data (cons (list type given-name) data)))))
		  (if (eq? x 'x)
		      (if (not (member type x-types))
			  (set! x-types (cons type x-types)))
		      (if (not (or (eq? x 'g)
				   (member type types)))
			  (set! types (cons type types))))
		  (set! type #f))
		(if (> i (+ 1 sp))
		    (set! type (substring args (+ 1 sp) i))))
	    (set! sp i))))))

(define (helpify name type args)
  (let ((initial (format #f "  #define H_~A \"~A ~A(" name type name)))
    (let ((line-len (length initial))
	  (len (length args))
	  (typed #f)
	  (help-max 100))
      (hey initial)
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(let ((ch (args i)))
	  (if (char=? ch #\space)
	      (if typed
		  (begin
		    (heyc ", ")
		    (set! line-len (+ line-len 2))
		    (if (> line-len help-max)
			(begin
			  (hey "\\~%")
			  (set! line-len 0)))
		    (set! typed #f))
		  (begin
		    (set! line-len (+ 1 line-len))
		    (heyc " ")
		    (set! typed #t)))
	      (if (not (memv ch '(#\@ #\#)))
		  (begin
		    (set! line-len (+ 1 line-len))
		    (heyc ch))))))
      (hey ")\"~%"))))

(define direct-types 
  (list (cons "void" #f)
	(cons "GLvoid" #f)
	(cons "int" "INT")
	(cons "GLint" "INT")
	(cons "GLsizei" "INT")
	(cons "GLenum" "INT")
	(cons "GLfloat" "DOUBLE")
	(cons "GLclampf" "DOUBLE")
	(cons "GLdouble" "DOUBLE")
	(cons "GLclampd" "DOUBLE")
	(cons "double" "DOUBLE")
	(cons "char" "CHAR")
	(cons "char*" "STRING")
	(cons "GLbyte" "INT")
	(cons "GLshort" "INT")
	(cons "GLbitfield" "ULONG")
	(cons "GLboolean" "BOOLEAN")
	(cons "GLushort" "INT")
	(cons "GLuint" "ULONG")
	(cons "GLubyte" "INT")
	(cons "unsigned_long" "ULONG")
	(cons "Bool" "BOOLEAN")
	(cons "xen" #t)
	(cons "constchar*" "STRING")

	(cons "guint" "INT")
	(cons "gint" "INT")
	(cons "gboolean" "BOOLEAN")

	))

(define glu-1-2 '("GLUtesselator*" "gluBeginPolygon" "gluDeleteTess" "gluEndPolygon" "gluNextContour" "gluTessVertex"
		  "gluGetTessProperty" "gluTessBeginContour" "gluTessBeginPolygon" "gluTessEndContour" "gluTessEndPolygon"
		  "gluTessNormal" "gluTessProperty" "gluNewTess"))

(define (c-to-xen-macro-name typ str)
  (cond ((assoc str '(("INT"     . "C_int_to_Xen_integer")
		      ("DOUBLE"  . "C_double_to_Xen_real") 
		      ("BOOLEAN" . "C_bool_to_Xen_boolean")
		      ("ULONG"   . "C_ulong_to_Xen_ulong"))
		string=?) => cdr)
	((not (string-ci=? str "String")) (format #f "~A unknown" str))
	((string=? (car typ) "guchar*")   (copy "C_to_Xen_String"))
	(else                             (copy "C_string_to_Xen_string"))))

(define (xen-to-c-macro-name str)
  (cond ((assoc str '(("INT"     . "Xen_integer_to_C_int")
		      ("DOUBLE"  . "Xen_real_to_C_double") 
		      ("BOOLEAN" . "Xen_boolean_to_C_bool")
		      ("ULONG"   . "Xen_ulong_to_C_ulong"))
		string=?) => cdr)
	((string-ci=? str "String") (copy "Xen_string_to_C_string"))
	(else (format #f "~A unknown" str))))

(define (type-it type)
  (cond ((assoc type direct-types) => 
	 (lambda (typ)
	   (when (cdr typ)
	     (if (string? (cdr typ))
		 (begin
		   (if (not (member type
				    '("Display*" "XVisualInfo*" "int*" "Pixmap" "Font" "GLubyte*"
				      "GLdouble*" "GLfloat*" "GLvoid*" "GLuint*"
				      "GLboolean*" "void*" "GLint*" "GLshort*"
				      "GLsizei" "GLclampd" "GLclampf" "GLbitfield" "GLshort" "GLbyte"
				      "unsigned_long"
				      "void**")))
		       (if (string=? type "constchar*")
			   (hey "#define C_to_Xen_~A(Arg) C_string_to_Xen_string((char *)(Arg))~%" (no-stars type))
			   (hey "#define C_to_Xen_~A(Arg) ~A(Arg)~%" (no-stars type) (c-to-xen-macro-name typ (cdr typ)))))
		   
		   (unless (string=? type "constchar*")
		     (hey "#define Xen_to_C_~A(Arg) (~A)(~A(Arg))~%" (no-stars type) type (xen-to-c-macro-name (cdr typ)))
		     (hey "#define Xen_is_~A(Arg) Xen_is_~A(Arg)~%" 
			  (no-stars type)
			  (cond ((assoc (cdr typ) '(("INT"    . "integer") 
						    ("ULONG"  . "ulong") 
						    ("DOUBLE" . "number")) string=?)
				 => cdr)
				(else (string-downcase (cdr typ)))))))
		 (begin
		   (hey "#define Xen_is_~A(Arg) 1~%" (no-stars type))
		   (hey "#define Xen_to_C_~A(Arg) ((gpointer)Arg)~%" (no-stars type)))))))
	
	((not (member type '("Display*" "XVisualInfo*" "GLXContext") string=?))
	 ;; Snd g_snd_gl_context (snd-motif.c) calls GLXContext a pointer
	 (if (member type glu-1-2) 
	     (hey "#ifdef GLU_VERSION_1_2~%")
	     (if (member type '("GLUnurbs*" "GLUtesselator*" "GLUquadric*" "_GLUfuncptr"))
		 (hey "#if HAVE_GLU~%")))
	 (hey "XL_TYPE~A~A(~A, ~A)~%" 
	      (if (has-stars type) "_PTR" "")
	      (if (member type '("int*" "Pixmap" "Font" "GLubyte*" 
				 "GLdouble*" "GLfloat*" "GLvoid*" 
				 "GLuint*" "GLboolean*" "GLint*" "GLshort*"
				 "PangoFontDescription*" "GtkWidget*" "GdkGLConfigMode"))
		  "_1" 
		  (if (member type '("GdkVisual*" "PangoFont*" "GdkColormap*"))
		      "_2" 
		      ""))
	      (no-stars type)
	      type)
	 (if (or (member type glu-1-2) 
		 (member type '("GLUnurbs*" "GLUtesselator*" "GLUquadric*" "_GLUfuncptr")))
	     (hey "#endif~%")))
	
	((string=? type "Display*")
	 (hey "XL_TYPE_PTR(Display, Display*)~%"))
	
	((string=? type "XVisualInfo*")
	 (hey "XL_TYPE_PTR(XVisualInfo, XVisualInfo*)~%"))
	
	(else (hey "XL_TYPE_PTR(GLXContext, GLXContext)~%"))))

(define* (CFNC data spec spec-name)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(format () "~A CFNC~%" name)
	(let ((type (car-str data)))
	  (if (not (member type types))
	      (set! types (cons type types)))
	  (let ((strs (parse-args args)))
	    (set! funcs (cons (list name type strs (if spec (values args spec spec-name) args)) funcs))
	    (set! names (cons (cons name 'fnc) names)))))))

(define* (CINT name type)
  (if (assoc name names)
      (format () "~A CINT~%" name)
      (begin
	(set! ints (cons name ints))
	(set! names (cons (cons name 'int) names)))))

(define* (CFNC-X data spec spec-name)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(format () "~A CFNC-X~%" name)
	(let ((type (car-str data)))
	  (if (not (member type x-types))
	      (set! x-types (cons type x-types)))
	  (let ((strs (parse-args args 'x)))
	    (set! x-funcs (cons (list name type strs (if spec (values args spec spec-name) args)) x-funcs))
	    (set! names (cons (cons name 'fnc) names)))))))

(define* (CINT-X name type)
  (if (assoc name names)
      (format () "~A CINT-X~%" name)
      (begin
	(set! x-ints (cons name x-ints))
	(set! names (cons (cons name 'int) names)))))

;;; ---------------------------------------- read data ---------------------------------------- 

(load "gldata.scm")

;;; ---------------------------------------- write output file ----------------------------------------
(hey "/* gl.c: s7, Ruby, and Forth bindings for GL, GLU~%")
(hey " *   generated automatically from makegl.scm and gldata.scm~%")
(hey " *   needs xen.h~%")
(hey " *~%")
(hey " * reference args are ignored if passed, resultant values are returned in a list.~%")
(hey " * the various \"v\" forms are omitted for now -- are they needed in this context?~%")
(hey " * 'gl is added to *features*~%")
(hey " *~%")
(hey " * HISTORY:~%")
(hey " *~%")
(hey " *     16-Apr-14: changed max-args to 8.~%")
(hey " *     --------~%")
(hey " *     16-Dec-09: removed Guile support.~%")
(hey " *     --------~%")
(hey " *     17-Oct-08: removed gtkglext bindings.~%")
(hey " *     --------~%")
(hey " *     30-Mar-06: check for glu.h, omit GLU_* if necessary.  Add Forth support.~%")
(hey " *     --------~%")
(hey " *     13-Jun-05: merged gl-ruby.c into gl.c.~%")
(hey " *     --------~%")
(hey " *     10-Mar:    Gl_Version.~%")
(hey " *     1-Feb-03:  glGet* funcs now try to handle multiple return values correctly.~%")
(hey " *     --------~%")
(hey " *     18-Nov:    added more GtkGlext bindings.~%")
(hey " *     1-Aug:     removed all 'EXT' junk.~%")
(hey " *     24-July:   changed Guile prefix (R5RS reserves vertical-bar).~%")
(hey " *     18-June:   GL 1.1 stubs.~%")
(hey " *     4-June:    GtkGLext support.~%")
(hey " *     20-May-02: initial version.~%")
(hey " */~%~%")

(hey "#include \"mus-config.h\"~%~%")

(hey "#if HAVE_EXTENSION_LANGUAGE~%")

(hey "#include <GL/gl.h>~%")
(hey "#if HAVE_GLU~%")
(hey "  #include <GL/glu.h>~%")
(hey "#endif~%")
(hey "#if USE_MOTIF~%")
(hey "  #include <GL/glx.h>~%")
(hey "#endif~%")
(hey "#include <string.h>~%~%")

(hey "#if USE_SND~%")
(hey "  /* USE_SND causes xm to use Snd's error handlers which are much smarter than xen's fallback versions */~%")
(hey "  #include \"snd.h\"~%")
(hey "#else~%")
(hey "  #include \"xen.h\"~%")
(hey "#endif~%")

(hey "#ifndef unsigned_long~%")
(hey "  /* for FreeBSD (thanks to Michael Scholz) (can't use ulong here due to collisions elsewhere) */~%")
(hey "  typedef unsigned long unsigned_long;~%")
(hey "#endif~%~%")

(hey "/* prefix for all names */~%")
(hey "#if HAVE_SCHEME~%")
(hey "  #define XL_PRE \"\"~%")
(hey "  #define XL_POST \"\"~%")
(hey "#endif~%")
(hey "#if HAVE_RUBY~%")
(hey "/* for Ruby, XG PRE needs to be uppercase */~%")
(hey "  #define XL_PRE \"R\"~%")
(hey "  #define XL_POST \"\"~%")
(hey "#endif~%")
(hey "#if HAVE_FORTH~%")
(hey "  #define XL_PRE \"F\"~%")
(hey "  #define XL_POST \"\"~%")
(hey "#endif~%")
(hey "~%")

(hey "#define wrap_for_Xen(Name, Value) Xen_list_2(C_string_to_Xen_symbol(Name), Xen_wrap_C_pointer(Value))~%")
(hey "#define is_wrapped(Name, Value) (Xen_is_list(Value) && \\~%")
(hey "                            (Xen_list_length(Value) >= 2) && \\~%")
(hey "                            (Xen_is_symbol(Xen_car(Value))) && \\~%")
(hey "                            (strcmp(Name, Xen_symbol_to_C_string(Xen_car(Value))) == 0))~%")
(hey "~%")

;;; these have to match the choices in xm.c 
(hey "#define XL_TYPE(Name, XType) \\~%")
(hey "  static Xen C_to_Xen_ ## Name (XType val) {return(Xen_list_2(C_string_to_Xen_symbol(#Name), C_ulong_to_Xen_ulong(val)));} \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {return((XType)Xen_ulong_to_C_ulong(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(#Name, val));}~%")
(hey "#define XL_TYPE_1(Name, XType) \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {return((XType)Xen_ulong_to_C_ulong(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(#Name, val));}~%")
(hey "~%")
(hey "#define XL_TYPE_PTR(Name, XType) \\~%")
(hey "  static Xen C_to_Xen_ ## Name (XType val) {if (val) return(wrap_for_Xen(#Name, val)); return(Xen_false);} \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {if (Xen_is_false(val)) return(NULL); return((XType)Xen_unwrap_C_pointer(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(#Name, val));} /* if NULL ok, should be explicit */~%")
(hey "#define XL_TYPE_PTR_1(Name, XType) \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {if (Xen_is_false(val)) return(NULL); return((XType)Xen_unwrap_C_pointer(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(#Name, val));} /* if NULL ok, should be explicit */~%")

;XL_TYPE_PTR_2 was for "GdkVisual*" "PangoFont*" "GdkColormap*"
;(hey "#define XL_TYPE_PTR_2(Name, XType) \\~%")
;(hey "  static Xen C_to_Xen_ ## Name (XType val) {if (val) return(wrap_for_Xen(#Name, val)); return(Xen_false);}~%")


(hey "~%~%/* ---------------------------------------- types ---------------------------------------- */~%~%")

(hey "#if USE_MOTIF~%")
(for-each type-it (reverse x-types))
(hey "#endif~%")

;(hey "#if USE_GTK~%")
;(for-each type-it (reverse g-types))
;(hey "#endif~%")

(for-each type-it (reverse types))

(hey "~%~%/* ---------------------------------------- state readback confusion ---------------------------------------- */~%~%")

(hey "static int how_many_vals(GLenum gl)~%")
(hey "{~%")
(hey "  switch (gl)~%")
(hey "    {~%")
(hey "    case GL_CURRENT_COLOR:~%")
(hey "    case GL_CURRENT_TEXTURE_COORDS:~%")
(hey "    case GL_CURRENT_RASTER_POSITION:~%")
(hey "    case GL_CURRENT_RASTER_COLOR:~%")
(hey "    case GL_CURRENT_RASTER_TEXTURE_COORDS:~%")
(hey "    case GL_VIEWPORT:~%")
(hey "    case GL_FOG_COLOR:~%")
(hey "    case GL_AMBIENT:~%")
(hey "    case GL_DIFFUSE:~%")
(hey "    case GL_SPECULAR:~%")
(hey "    case GL_EMISSION:~%")
(hey "    case GL_LIGHT_MODEL_AMBIENT:~%")
(hey "    case GL_SCISSOR_BOX:~%")
(hey "    case GL_COLOR_WRITEMASK:~%")
(hey "    case GL_COLOR_CLEAR_VALUE:~%")
(hey "      return(4);~%")
(hey "      break;~%")
(hey "    case GL_MODELVIEW_MATRIX:~%")
(hey "    case GL_PROJECTION_MATRIX:~%")
(hey "    case GL_TEXTURE_MATRIX:~%")
(hey "      return(16);~%")
(hey "      break;~%")
(hey "    case GL_CURRENT_NORMAL:~%")
(hey "    case GL_SPOT_DIRECTION:~%")
(hey "      return(3);~%")
(hey "      break;~%")
(hey "    case GL_DEPTH_RANGE:~%")
(hey "    case GL_LINE_WIDTH_RANGE:~%")
(hey "      return(2);~%")
(hey "      break;~%")
(hey "    default: return(1); break; /* try to squelch c++ babbling */~%")
(hey "    }~%")
(hey "  return(1);~%")
(hey "}~%")

(define need-vals-check (list "glGetIntegerv" "glGetFloatv" "glGetMaterialfv" "glGetLightfv" "glGetBooleanv"))

(define max-args 8)

(hey "~%~%/* ---------------------------------------- functions ---------------------------------------- */~%~%")

(define (handle-func data)
  (let* ((args (caddr data))
	 (argslen (length args))
	 (refargs (ref-args args)))
    (let ((name (car data))
	  (return-type (cadr data))
	  (xgargs (- argslen refargs))
	  (argstr (cadddr data))
	  (arg-start 0)
	  (line-len 0))
      
      (define (hey-on . args)
	;; no cr -- just append
	(let ((line (apply format #f args)))
	  (set! line-len (+ line-len (length line)))
	  (heyc line)))
      
      (check-glu name)
      (if (member name glu-1-2) (hey "#ifdef GLU_VERSION_1_2~%"))
      
      (if (and (> (length data) 4)
	       (eq? (data 4) 'if))
	  (hey "#if HAVE_~A~%" (string-upcase (symbol->string (data 5)))))
      (hey "static Xen gxg_~A(" name)
      (if (null? args)
	  (heyc "void")
	  (if (>= argslen max-args)
	      (heyc "Xen arglist")
	      (let ((previous-arg #f))
		(for-each 
		 (lambda (arg)
		   (let ((argname (cadr arg))
					;(argtype (car arg))
			 )
		     (if previous-arg (heyc ", "))
		     (set! previous-arg #t)
		     (hey "Xen ~A" argname)))
		 args))))
      (hey ")~%{~%")
      (helpify name return-type argstr)
      (if (> refargs 0)
	  (for-each
	   (lambda (arg)
	     (if (ref-arg? arg)
		 (if (member name need-vals-check)
		     (hey "  ~A ~A[16];~%" (deref-type arg) (deref-name arg))
		     (hey "  ~A ~A[1];~%" (deref-type arg) (deref-name arg)))))
	   args))
      (when (and (>= argslen max-args)
		 (> xgargs 0))
	(heyc "  Xen ")
	(for-each
	 (let ((previous-arg #f))
	   (lambda (arg)
	     (if (not (ref-arg? arg)) ;(< (length arg) 3)
		 (begin
		   (if previous-arg (heyc ", "))
		   (set! previous-arg #t)
		   (hey "~A" (cadr arg))))))
	 args)
	(hey ";~%")
	(for-each
	 (let ((ctr 0)) ; list-ref counts from 0
	   (lambda (arg)
	     (if (not (ref-arg? arg))
		 (hey "  ~A = Xen_list_ref(arglist, ~D);~%" (cadr arg) ctr))
	     (set! ctr (+ 1 ctr))))
	 args))
      (if (> argslen 0)
	  (let ((ctr 1))
	    (for-each
	     (lambda (arg)
	       (let ((argname (cadr arg))
		     (argtype (car arg)))
		 (cond ((ref-arg? arg))

		       ((null-arg? arg)
			(hey "  Xen_check_type(Xen_is_~A(~A) || Xen_is_false(~A), ~A, ~D, ~S, ~S);~%" 
			     (no-stars argtype) argname argname argname ctr name argtype))

		       ((opt-arg? arg)
			(hey "  if (!Xen_is_bound(~A)) ~A = Xen_false; ~%" argname argname)
			(hey "  else Xen_check_type(Xen_is_~A(~A), ~A, ~D, ~S, ~S);~%" 
			     (no-stars argtype) argname argname ctr name argtype))

		       (else
			(hey "  Xen_check_type(Xen_is_~A(~A), ~A, ~D, ~S, ~S);~%"
			     (no-stars argtype) argname argname ctr name argtype)))
		 (set! ctr (+ 1 ctr))))
	     args)))
      (let ((using-result (and (> refargs 0)
			       (not (string=? return-type "void")))))
	(if using-result
	    (begin
	      (hey "  {~%")
	      (hey "    Xen result;~%")))
	(set! line-len 0)
	(if (string=? return-type "void")
	    (hey-on "  ")
	    (hey-on (if (= refargs 0)
			"  return(C_to_Xen_~A("
			"    result = C_to_Xen_~A(")
		    (no-stars return-type)))
	
	(hey-on "~A(" name)
	(set! arg-start line-len)
	(if (> argslen 0)
	    (let ((previous-arg #f))
	      (for-each
	       (lambda (arg)
		 (let ((argname (cadr arg))
		       (argtype (car arg)))
		   (if previous-arg 
		       (let ((arg ", "))
			 ;; cr ok after arg
			 (set! line-len (+ line-len (length arg)))
			 (heyc arg)
			 (if (> line-len 120)
			     (begin
			       (format gl-file "~%~NC" arg-start #\space)
			       (set! line-len arg-start)))))
		   (if (and (not previous-arg)
			    (> (length data) 4)
			    (eq? (data 4) 'const))
		       (hey "(const ~A)" argtype))
		   (set! previous-arg #t)
		   (if (ref-arg? arg)
		       (hey-on "~A" (deref-name arg))
		       (hey-on "Xen_to_C_~A(~A)" (no-stars argtype) argname))))
	       args)))
	
	(if (> refargs 0)
	    (let ((previous-arg using-result))
	      (if (not (string=? return-type "void")) 
		  (heyc ")"))
	      (hey ");~%")
	      (if using-result (heyc "  "))
	      (if (member name need-vals-check)
		  (begin
		    (hey "  {~%")
		    (if (not using-result)
			(hey "    Xen result;~%"))
		    (hey "    int i, vals;~%    ~
                             vals = how_many_vals(Xen_to_C_GLenum(pname));~%    ~
		             result = Xen_empty_list;~%    ~
                             for (i = 0; i < vals; i++)~%")
		    (hey "      result = Xen_cons(C_to_Xen_~A(~A[i]), result);~%" 
			 (no-stars (deref-type (args (- argslen 1))))
			 (deref-name (args (- argslen 1))))
		    (hey "    return(result);~%")
		    (hey "  }~%"))
		  (begin
		    (hey "  return(Xen_list_~D(" (if using-result (+ refargs 1) refargs))
		    (if using-result (heyc "result"))
		    (for-each 
		     (lambda (arg)
		       (if (ref-arg? arg)
			   (begin
			     (if previous-arg (heyc ", "))
			     (hey "C_to_Xen_~A(~A[0])" (no-stars (deref-type arg)) (deref-name arg))
			     (set! previous-arg #t))))
		     args)
		    (hey "));~%")))
	      (if using-result (hey "   }~%")))
	    (if (string=? return-type "void")
		(begin
		  (hey ");~%")
		  (hey "  return(Xen_false);~%"))
		(hey ")));~%"))))
      
      (hey "}~%")
      (if (member name glu-1-2) (hey "#endif~%"))
      (hey "~%"))))

(hey "#if USE_MOTIF~%")
(for-each handle-func (reverse x-funcs))
(hey "#endif~%")

(for-each handle-func (reverse funcs))
(uncheck-glu)


;;; ---------------- argify linkages

(define (argify-func func)
  (let ((cargs (length (caddr func)))
	(refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	;(args (- cargs refargs))
	(if-fnc (and (> (length func) 4)
		     (eq? (func 4) 'if))))
    (let ((name (car func)))
      (check-glu name)
      (if (member name glu-1-2) (hey "#ifdef GLU_VERSION_1_2~%")))
    (if if-fnc
	(hey "#if HAVE_~A~%" (string-upcase (symbol->string (func 5)))))
    (hey "Xen_wrap_~A(gxg_~A_w, gxg_~A)~%" 
	 (if (>= cargs max-args) 
	     "any_args"
	     (format #f (values (if (> refargs 0)
				    (values "~D_optional_arg~A" cargs)
				    (values "~A_arg~A" (if (zero? cargs) "no" (number->string cargs))))
				(if (= cargs 1) "" "s"))))
	 (car func) (car func))
    (if if-fnc
	(hey "#endif~%"))
    (if (member (car func) glu-1-2) (hey "#endif~%"))))
	 
(hey "#if USE_MOTIF~%")
(for-each argify-func (reverse x-funcs))
(hey "#endif~%")

(for-each argify-func (reverse funcs))
(uncheck-glu)



;;; ---------------- procedure linkages

(define (gtk-type->s7-type gtk)
  (let ((dt (assoc gtk direct-types)))
    (or (not (and (pair? dt)
		  (string? (cdr dt))))
	(let ((direct (cdr dt)))
	  (cond ((member direct '("INT" "ULONG") string=?) 'integer?)
		((assoc direct '(("BOOLEAN" . boolean?) 
				 ("DOUBLE"  . real?) 
				 ("CHAR"    . char?) 
				 ("String"  . string?))
			string=?) => cdr)
		(#t #t))))))
	       
(define make-signature 
  (let ((compress (lambda (sig)
		    (do ((sig sig (cdr sig)))
			((not (and (pair? sig)
				   (pair? (cdr sig))
				   (not (and (eq? (car sig) 'pair?) (null? (cddr sig))))
				   (eq? (car sig) (cadr sig))))
			 sig)))))
    (lambda (fnc)
      (let ((sig (list (if (positive? (ref-args (caddr fnc))) ; these are returned as cadr of list
			   'pair?
			   (gtk-type->s7-type (cadr fnc))))))
	(for-each
	 (lambda (arg)
	   (set! sig (cons (gtk-type->s7-type (car arg)) sig)))
	 (caddr fnc))
	(reverse (compress sig))))))

(define signatures (make-hash-table))
(define (make-signatures lst type)
  (for-each
   (lambda (f)
     (let ((sig (make-signature f)))
       (if (and (pair? sig)
		(not (signatures sig)))
	   (set! (signatures sig) type))))
   lst))

(make-signatures funcs :gtk)
(make-signatures x-funcs :motif)

(define gtk-sigs 0)
(define motif-sigs 0)
(for-each (lambda (sig)
	    (if (eq? (cdr sig) :gtk)
		(set! gtk-sigs (+ gtk-sigs 1))
		(set! motif-sigs (+ motif-sigs 1))))
	  signatures)
;(format *stderr* "~D entries, ~D funcs, ~D ~D~%" (hash-table-entries signatures) (length funcs) gtk-sigs motif-sigs)

(hey "static void define_functions(void)~%")
(hey "{~%")

(hey "#if HAVE_SCHEME~%")
(hey "static s7_pointer s_boolean, s_integer, s_real, s_any;~%")
(hey "static s7_pointer ")

(define (sig-name sig)
  (call-with-output-string
   (lambda (p)
     (display "pl_" p)
     (display (case (car sig)
		((integer?)    "i")
		((boolean?)    "b")
		((real?)       "d")
		((string?)     "s")
		((pair?)       "p")
		((gtk_enum_t?) "g")
		(else          "t"))
	      p)
     (for-each
      (lambda (typ)
	(display (case typ
		   ((integer?)    "i")
		   ((boolean?)    "b")
		   ((real?)       "r")
		   ((string?)     "s")
		   ((pair?)       "u") ; because we're stupidly using #f=null
		   ((gtk_enum_t?) "g")
		   (else          "t"))
		 p))
      (cdr sig)))))
     
(let ((ctr 0))
  (for-each
   (lambda (sigc)
     (if (eq? (cdr sigc) :gtk)
	 (let ((sig (car sigc)))
	   (hey (sig-name sig))
	   (set! ctr (+ ctr 1))
	   (hey (if (< ctr gtk-sigs) ", " ";")))))
   signatures))
(hey "~%")

(hey "#if USE_MOTIF~%")
(hey "static s7_pointer ")
(let ((ctr 0))
  (for-each
   (lambda (sigc)
     (if (eq? (cdr sigc) :motif)
	 (let ((sig (car sigc)))
	   (hey (sig-name sig))
	   (set! ctr (+ ctr 1))
	   (hey (if (< ctr motif-sigs) ", " ";")))))
   signatures))
(hey "~%")
(hey "#endif~%~%")

(hey "  s_boolean = s7_make_symbol(s7, \"boolean?\");~%")
(hey "  s_integer = s7_make_symbol(s7, \"integer?\");~%")
(hey "  s_real = s7_make_symbol(s7, \"real?\");~%")
(hey "  s_any = s7_t(s7);~%~%")

(let ((sigout (lambda (gui)
		(for-each
		 (lambda (sigc)
		   (if (eq? (cdr sigc) gui)
		       (let ((sig (car sigc)))
			 (hey "  ")
			 (hey (sig-name sig))
			 (hey " = s7_make_circular_signature(s7, ")
			 (let ((len (length sig)))
			   (hey (number->string (- len 1)))
			   (hey ", ")
			   (hey (number->string len))
			   (hey ", ")
			   (do ((i 0 (+ i 1))
				(s sig (cdr s)))
			       ((= i len))
			     (let ((typ (car s)))
			       (hey (case typ
				      ((integer?) "s_integer")
				      ((boolean?) "s_boolean")
				      ((real?) "s_real")
				      (else "s_any"))))
			     (if (< i (- len 1)) (hey ", "))))
			 (hey ");~%"))))
		 signatures))))
  (sigout :gtk)
  (hey "~%")
  (hey "#if USE_MOTIF~%")
  (sigout :motif)
  (hey "#endif~%"))
(hey "#endif~%~%")

(hey "#if HAVE_SCHEME~%")
(hey "  #define gl_define_procedure(Name, Value, A1, A2, A3, Help, Sig) s7_define_typed_function(s7, XL_PRE #Name XL_POST, Value, A1, A2, A3, Help, Sig)~%")
(hey "#else~%")
(hey "  #define gl_define_procedure(Name, Value, A1, A2, A3, Help, Sig) Xen_define_safe_procedure(XL_PRE #Name XL_POST, Value, A1, A2, A3, Help)~%")
(hey "#endif~%")
(hey "~%")

(define (defun func)
  (let ((cargs (length (caddr func)))
	(refargs (+ (ref-args (caddr func)) (opt-args (caddr func)))))
    (let ((name (car func))
	  (args (- cargs refargs)))
      (check-glu name)
      (if (member name glu-1-2) (hey "#ifdef GLU_VERSION_1_2~%"))
      
      (hey "  gl_define_procedure(~A, gxg_~A_w, ~D, ~D, ~D, H_~A, ~A);~%"
	   name name
	   (if (>= cargs max-args) 0 args)
	   (if (>= cargs max-args) 0 refargs) ; optional ignored
	   (if (>= cargs max-args) 1 0)
	   name
	   (sig-name (make-signature func)))
      
      (if (member name glu-1-2) (hey "#endif~%")))))

(hey "#if USE_MOTIF~%")
(for-each defun (reverse x-funcs))
(hey "#endif~%")

(for-each defun (reverse funcs))
(uncheck-glu)

(hey "}~%~%")


(hey "/* ---------------------------------------- constants ---------------------------------------- */~%~%")
(hey "static void define_integers(void)~%")
(hey "{~%~%")
(hey "#define DEFINE_INTEGER(Name) Xen_define(XL_PRE #Name XL_POST, C_int_to_Xen_integer(Name))~%")
(hey "~%")

(hey "#if USE_MOTIF~%")
(for-each 
 (lambda (val) 
   (hey "  DEFINE_INTEGER(~A);~%" val)) 
 (reverse x-ints))
(hey "#endif~%")

(for-each 
 (lambda (val) 

     (if in-glu
	 (if (not (string=? "GLU" (substring val 0 3)))
	     (begin
	       (set! in-glu #f)
	       (hey "#endif~%")))
	 (if (string=? "GLU" (substring val 0 3))
	     (begin
	       (set! in-glu #t)
	       (hey "#if HAVE_GLU~%"))))

   (hey "  DEFINE_INTEGER(~A);~%" val)) 
 (reverse ints))

(uncheck-glu)

(hey "}~%~%")

(hey "/* -------------------------------- initialization -------------------------------- */~%~%")
(hey "static bool gl_already_inited = false;~%~%")
(hey "#if HAVE_SCHEME~%")
(hey "void Init_libgl(s7_scheme *sc);~%")
(hey "void Init_libgl(s7_scheme *sc)~%")
(hey "#else~%")
(hey "void Init_libgl(void);~%")
(hey "void Init_libgl(void)~%")
(hey "#endif~%")
(hey "{~%")
(hey "  if (!gl_already_inited)~%")
(hey "    {~%")
(hey "      define_integers();~%")
(hey "      define_functions();~%")
(hey "      Xen_provide_feature(\"gl\");~%")
(hey "      Xen_define(\"gl-version\", C_string_to_Xen_string(\"~A\"));~%" (strftime "%d-%b-%y" (localtime (current-time))))
(hey "      gl_already_inited = true;~%")
(hey "    }~%")
(hey "}~%")
(hey "#else~%")
(hey " void Init_libgl(void);~%")
(hey " void Init_libgl(void)~%")
(hey "{~%")
(hey "}~%")
(hey "#endif~%")

(close-output-port gl-file)

(exit)
