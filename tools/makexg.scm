;;; makexg.scm creates the gtk2|3/gdk/pango/glib/cairo bindings using xgdata.scm, writes xg.c

(define xg-file (open-output-file "xg.c"))
(define lib-file (open-output-file "libgtk_s7.c"))

(define (hey . args)
  (apply format xg-file args))

(define (heyc arg)
  (display arg xg-file))


;;; ----------------
(define (hay . args)
  (apply format lib-file args))

(define (hayc arg)
  (display arg lib-file))


(define (hoy . args)
  (apply format xg-file args)
  (apply format lib-file args))

(define (hoyc arg)
  (display arg xg-file)
  (display arg lib-file))
;;; ----------------


(define names (make-hash-table))
(define types ())
(define ints ())
(define dbls ())
(define funcs ())
(define casts ())
(define checks ())
(define atoms ())
(define strings ())

(define all-types ())

;;; preset some types that are getting confused
(set! types (list "GdkEventMotion*" "gdouble*" "GdkEventAny*" "GdkEvent*" "GdkWindow*" ;"GtkDrawingArea*"
		  "cairo_t*" "cairo_font_options_t*" "PangoFontDescription*"))
(set! all-types (list "GdkEventMotion*" "gdouble*" "GdkEventAny*" "GdkEvent*" "GdkWindow*" ;"GtkDrawingArea*"
		      "cairo_t*" "cairo_font_options_t*" "PangoFontDescription*"))

(define idlers (list "g_source_remove" "g_idle_remove_by_data"
		     "gtk_quit_remove" "gtk_quit_remove_by_data" 
		     ;"gtk_key_snooper_remove"
		     ))

(define no-c-to-xen 
  (list "CellLayoutDataFunc" "GClosureNotify" "GDestroyNotify" "GError**" "GParamSpec*" "GQuark*" 
	"GSignalAccumulator"
	"GSignalCMarshaller" "GSignalEmissionHook" "GSignalQuery*" "GSourceFunc" "GString*" "GTimeVal*" "GType*"
	"GdkBitmap**" ;"GdkDragProtocol*"
	"GdkEventButton*" "GdkEventConfigure*" "GdkEventCrossing*" "GdkEventDND*"
	"GdkEventExpose*" "GdkEventFocus*" "GdkEventMotion*" "GdkEventNoExpose*" "GdkEventProperty*" "GdkEventProximity*"
	"GdkEventScroll*" "GdkEventSelection*" "GdkEventSetting*" "GdkEventVisibility*" "GdkEventWindowState*" "GdkGCValues*"
	"GdkGeometry*" "GdkInterpType" "GdkModifierType*" "GdkPixbufDestroyNotify" "GdkScreen**" "GdkSegment*" "GdkWChar*"
	"GdkWMDecoration*"  "GdkWindowAttr*" "GtkAccelLabel*" "GtkAccelMapForeach" "GtkAccessible*" "GtkActionEntry*"
	"GtkAlignment*" "GtkAllocation*" 
	"GtkArrow*" "GtkAspectFrame*" "GtkBin*" "GtkBox*" "GtkButton*" "GtkButtonBox*"
	"GtkCalendar*" "GtkCellLayout*" "GtkCellLayoutDataFunc" "GtkCellRendererPixbuf*" "GtkCellRendererText*" "GtkCellRendererToggle*"
	"GtkCheckMenuItem*" ;"GtkClipboardTargetsReceivedFunc" 
	"GtkCombo*" "GtkComboBox*" "GtkComboBoxEntry*" "GtkContainer*" "GtkCurve*" "GtkDialog*" "GtkDrawingArea*" "GtkEditable*"
	"GtkEventBox*" "GtkExpander*" "GtkFileChooser*" "GtkFileFilterFunc"
	"GtkFileSelection*" "GtkFixed*" "GtkFontButton*" "GtkFontSelection*" "GtkFontSelectionDialog*" "GtkFrame*" "GtkGammaCurve*"
	"GtkHandleBox*" "GtkIMContextSimple*" "GtkIMMulticontext*" "GtkIconLookupFlags" "GtkImage*" "GtkImageMenuItem*" "GtkInputDialog*"
	"GtkInvisible*" "GtkItem*" "GtkItemFactoryEntry*" "GtkLabel*" "GtkLayout*" "GtkMenuDetachFunc" "GtkMenuItem*" "GtkMenuShell*"
	"GtkMessageDialog*" "GtkMisc*" "GtkNotebook*" "GtkOptionMenu*" "GtkPackType*" "GtkPaned*" "GtkPlug*"
	"GtkProgressBar*" "GtkRadioButton*" "GtkRadioMenuItem*" "GtkRadioToolButton*" "GtkRange*" "GtkRcPropertyParser" "GtkRuler*"
	"GtkScale*" "GtkScrolledWindow*" "GtkSeparatorToolItem*" "GtkSettingsValue*" "GtkSocket*" "GtkSortType*" "GtkSpinButton*"
	"GtkStatusbar*" "GtkTable*" "GtkTextCharPredicate" "GtkTextTagTableForeach" "GtkTextView*"
	"GtkToggleActionEntry*" "GtkToggleButton*" "GtkToggleToolButton*" "GtkToolButton*" "GtkToolbar*" "GtkTreeDragDest*"
	"GtkTreeDragSource*" "GtkTreeModel**" "GtkTreeModelFilter*" "GtkTreeModelSort*" "GtkTreeSortable*" ;"GtkUIManagerItemType"
	"GtkViewport*" "PangoAnalysis*" "PangoAttrList**" "PangoFontDescription**" "PangoRectangle*"
	"gchar***" "gfloat*" "gint8*" "gssize" "guint16*" "gunichar*" "GtkFileChooserButton*" ;"GtkPathPriorityType"
	"GtkCellView*" "GValue*" "GtkAboutDialog*" "PangoAttrFilterFunc" "PangoScript*" "GtkMenuToolButton*"
	;"GtkClipboardImageReceivedFunc" 
	"PangoMatrix*" "GdkTrapezoid*" "GdkPangoRenderer*" "PangoRenderPart"
	"GLogFunc" "GError*" "guint32*"
	
	"GConnectFlags" "GSignalFlags" "GSignalMatchType" 
					;"GdkAxisUse" 
	;"GdkFillRule" 
	;"GdkGCValuesMask"
	"GdkPropMode" ;"GdkRgbDither" 
	"GdkWMFunction" "GdkWindowEdge" "GdkWindowHints" "GtkAccelFlags" ; "GtkArrowType"
	;"GtkAttachOptions"
	"GtkCellRendererState" ;"GtkCurveType"
	"GtkDestDefaults" "GtkDestroyNotify" "GtkDialogFlags"
	"GtkDirectionType" ;"GtkExpanderStyle" 
	"GtkIconLookupFlags" ;"GtkMenuPositionFunc" 
	"GtkPathType" "GtkSpinType"
	"GtkTextSearchFlags" "GtkTreeIterCompareFunc" "GtkTreeSelectionFunc" ;"GtkUIManagerItemType"
	"GtkWindowPosition"
	"PangoGlyph" "PangoUnderline" "gssize" 
	
	"GtkMenuBar*" "GtkTranslateFunc" ;"GtkMenuPositionFunc" 
	"GtkTreeIterCompareFunc" "GtkTreeSelectionFunc"
	"GtkDestroyNotify"
	
	"GtkAssistant*" "GtkRecentChooser*" "GtkRecentChooserMenu*"
	;"GtkTextBufferSerializeFunc" "GtkTextBufferDeserializeFunc" 
	"GtkRecentData*" "GtkNotebookWindowCreationFunc"
	
	"GtkUnit" "GtkPageSetupDoneFunc"
	
	"GtkPrintOperationPreview*" "GtkPrintSettingsFunc" "cairo_matrix_t*"
	"cairo_font_extents_t*" "cairo_text_extents_t*" "cairo_user_data_key_t*" "cairo_destroy_func_t"
	
	"GtkPrintOperationAction"
	"GtkTooltip*" "GtkCalendarDetailFunc" "GtkScaleButton*" "GtkEntryIconPosition"
	"GdkDragAction" ;"GdkImageType"
	
	"gdouble*" ;"GdkFill" 
	;"GdkSubwindowMode" ;"GdkLineStyle" "GdkCapStyle" "GdkJoinStyle"
	"GtkInfoBar*" "GtkSpinner*" "GtkToolShell*" "GtkToolPalette*" "GtkToolPaletteDragTargets"
	;"GdkFunction" ;"GtkWrapBoxPacking" 
	"GtkLinkButton*" "GtkActivatable*" "GtkOrientable*" "GtkCellArea*"
	;"GdkNativeWindow"
	"GdkRectangle*" 
	"PangoRenderer*" "cairo_glyph_t**" "cairo_text_cluster_t**"
;	"cairo_text_cluster_flags_t" 
;	"cairo_rectangle_int_t" 
	"cairo_rectangle_t*"
	"double*"

	"GtkContainerClass*" "GtkComboBoxText*" "GtkGrid*" "GtkScrollable*" "GtkSwitch*" 
	"cairo_text_cluster_flags_t" "cairo_text_cluster_flags_t*" "cairo_rectangle_int_t*"

	"GtkOverlay*" "cairo_pattern_t**" "GtkStyleProperties*" "GtkSymbolicColor*" "GtkWidgetPath*"
	"GtkFontChooser*" "GtkFontChooserDialog*"
	"GdkModifierIntent" "guint**" "GtkApplication*" "GVariant*" "GtkApplicationWindow*"
	"GdkEventKey*" "GtkColorChooser*"

	"GtkLevelBar*" "GtkMenuButton*" "GNormalizeMode"
;	"GIcon*"

	"GBytes" "GtkPlacesSidebar*" "GtkStackSwitcher*" "GtkRevealer*" "GtkHeaderBar*" "GtkListBox*" "GtkSearchBar*"

	"GtkFlowBox*" "GtkActionBar*" "GtkPopover*"
	"GtkGestureDrag*" "GtkGesturePan*" "GtkGestureMultiPress*" "GtkGestureRotate*" "GtkGestureSingle*"
	"GtkGestureSwipe*" "GtkGestureZoom*" "GtkGestureController*" "GtkEventController*"

	"GtkGLArea*" "GtkStyleContext*" "GtkPopoverMenu*" "GtkSearchEntry*" "GtkStackSidebar*" 
	"GtkShortcutsWindow*"
	"GdkDevicePadFeature" "GtkPadActionType"

	"GtkShortcutLabel*" "GtkPadActionEntry*" "GdkDevicePad*" "GActionGroup*"
	))

(define no-xen-p 
  (list "GdkXEvent*" "GdkVisualType*" "GError*" "GSignalInvocationHint*" "GtkAccelGroupEntry*" "GtkIconSize*" "AtkObject*"
	"GtkWidgetAuxInfo*" "PangoFontFamily**" "PangoFontset*" "PangoEngineShape*" "PangoLayoutRun*" "GdkDeviceAxis*"
	"GdkDeviceKey*" "GtkWidget**" "GtkLabelSelectionInfo*" "GtkItemFactoryCallback" "GtkNotebookPage*" "GtkRangeLayout*"
	"GData*" "GtkRangeStepTimer*" "GtkRcContext*" "GdkGC**" "GdkPixmap**" "GArray*" "GtkTextBTree*" "GtkTextLogAttrCache*"
	"GtkTableRowCol*" "GtkAccelMap*" "GtkTooltipsData*" "PangoScript" "PangoFontFace**"
	
	"GValue*" "GdkByteOrder" "GdkCrossingMode" "GdkEventType" "GdkGrabStatus" "GdkNotifyType"
					;"GdkOverlapType" 
	"GdkScrollDirection" "GdkSettingAction" ;"GdkVisibilityState"
	"GdkWindowState" "GdkWindowType"
	"GtkImageType" "GtkTreeModelFlags" "gint8" "gshort" "guint8" "lambda"
	
	"time_t" ;"GtkWindowGroup*" 
	;"GtkSettings*" ;"GdkDevice*" 
	"GtkScaleButton*"
	"GtkPrintOperationResult" "GtkPrintStatus" "GtkSizeRequestMode"
	"GdkEventAny*" "GdkDeviceManager*"
	"cairo_font_type_t" "cairo_pattern_type_t" "cairo_surface_type_t" "cairo_bool_t" "cairo_region_overlap_t"

	"glong" "double" "GdkAxisFlags" "GdkSubpixelLayout" 
	))

(define no-xen-to-c 
  (list "GdkXEvent*" "GSignalInvocationHint*" "GdkVisualType*" "GError*" "GtkAccelGroupEntry*" "GtkIconSize*" "AtkObject*" 
	"GtkWidgetAuxInfo*" "PangoFontFamily**" "PangoFontset*" "PangoEngineShape*" "PangoLayoutRun*" "GdkDeviceAxis*" 
	"GdkDeviceKey*" "GtkWidget**" "GtkItemFactoryCallback" "GtkLabelSelectionInfo*" "GtkNotebookPage*" "GtkRangeLayout*" 
	"GtkRangeStepTimer*" "GData*" "GtkRcContext*" "GdkGC**" "GdkPixmap**" "GArray*" "GtkTableRowCol*" "GtkTextBTree*" 
	"GtkTextLogAttrCache*" "GtkAccelMap*" "GtkTooltipsData*" "PangoScript" "PangoFontFace**"
	
	"GValue*" "GdkByteOrder" "GdkCrossingMode" "GdkEventType" "GdkGrabStatus" "GdkNotifyType"
					;"GdkOverlapType" 
	"GdkScrollDirection" "GdkSettingAction" ;"GdkVisibilityState" 
	"GdkWindowState" "GdkWindowType"
	"GtkImageType" "GtkTreeModelFlags" "etc" "gshort"
	
					;"GtkWindowGroup*" 
	"time_t" ;"GtkSettings*" ;"GdkDevice*" 
	"GtkScaleButton*"
	"GtkPrintOperationResult" "GtkPrintStatus"
	"GdkDeviceManager*" "GdkEventAny*" "GtkSizeRequestMode"
	
	"cairo_surface_type_t" "cairo_pattern_type_t" "cairo_font_type_t" "cairo_bool_t"
	"cairo_region_overlap_t" "cairo_device_type_t"

	"glong" "GdkAxisFlags" "GdkSubpixelLayout" 
	))

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

(define (remove-if p lst)
  (cond ((null? lst) ())
	((p (car lst)) (remove-if p (cdr lst)))
	(else (cons (car lst) 
		    (remove-if p (cdr lst))))))

(define (ref-arg? arg)
  (and (eqv? (length arg) 3)
       (string? (caddr arg))))

(define (null-arg? arg)
  (and (eqv? (length arg) 3)
       (eq? (caddr arg) 'null)))

(define (opt-arg? arg)
  (and (eqv? (length arg) 3)
       (eq? (caddr arg) 'opt)))

(define (ref-args args)
  (let ((ctr 0))
    (for-each
     (lambda (arg)
       (if (ref-arg? arg)
	   (set! ctr (+ ctr 1))))
     args)
    ctr))

(define (opt-args args)
  (let ((ctr 0))
    (for-each
     (lambda (arg)
       (if (opt-arg? arg)
	   (set! ctr (+ ctr 1))))
     args)
    ctr))

(define (deref-type arg)
  (let ((type (car arg)))
    (substring type 0 (- (length type) 1))))

(define (deref-element-type arg)
  (let ((type (car arg)))
    (substring type 0 (- (length type) 2))))

(define (deref-name arg)
  (string-append "ref_" (cadr arg)))

(define (derefable type)
  (let ((st (char-position #\* type)))
    (and st (char-position #\* type (+ st 1)))))

(define (has-stars type)
  (char-position #\* type))

(define (no-stars type)
  (let ((len (length type))
	(val (copy type)))
    (do ((i 0 (+ i 1)))
	((= i len) val)
      (if (char=? (val i) #\*)
	  (set! (val i) #\_)))))

(define cairo-funcs ())
(define cairo-png-funcs ())
(define cairo-ints ())
(define cairo-types ())

(define cairo-funcs-810 ())
(define cairo-ints-810 ())
(define cairo-types-810 ())

(define cairo-funcs-912 ())
(define cairo-ints-912 ())
(define cairo-types-912 ())
(define cairo-strings-912 ())

(define (parse-args args extra)
  (if (string=? args "void")
      ()
      (do ((data ())
	   (sp -1)
	   (type #f)
	   (len (length args))
	   (i 0 (+ i 1)))
	  ((= i len) (reverse data))
	(let ((ch (args i)))
	  (when (or (char=? ch #\space)
		    (= i (- len 1)))
	    (if (not type)
		(if (> i (+ 1 sp))
		    (set! type (substring args (+ 1 sp) i)))
		(begin
		  (let ((given-name (substring args (+ 1 sp) (if (= i (- len 1)) (+ i 1) i))))
		    (case (given-name 0)
		      ((#\@) (set! data (cons (list type (substring given-name 1) 'null) data)))
		      ((#\#) (set! data (cons (list type (substring given-name 1) 'opt) data)))
		      ((#\&) (set! data (cons (list type (substring given-name 1) 'set) data)))
		      ((#\[ #\{ #\|)
		       (let ((reftype (deref-type (list type))))
			 (set! data (cons (list type (substring given-name 1 (- (length given-name) 1)) given-name) data))
			 (if reftype (set! type reftype))))
		      (else (set! data (cons (list type given-name) data)))))
		  
		  (unless (member type all-types)
		    (set! all-types (cons type all-types))
		    (case extra
		      ((g-2.14)     (set! types-2.14 (cons type types-2.14)))
		      ((g-2.16)     (set! types-2.16 (cons type types-2.16)))
		      ((g-2.18)     (set! types-2.18 (cons type types-2.18)))
		      ((g-2.20)     (set! types-2.20 (cons type types-2.20)))
		      ((g-3.0)      (set! types-3.0 (cons type types-3.0)))
		      ((g-3.2)      (set! types-3.2 (cons type types-3.2)))
		      ((g-3.4)      (set! types-3.4 (cons type types-3.4)))
		      ((g-3.6)      (set! types-3.6 (cons type types-3.6)))
		      ((g-3.8)      (set! types-3.8 (cons type types-3.8)))
		      ((g-3.10)     (set! types-3.10 (cons type types-3.10)))
		      ((g-3.12)     (set! types-3.12 (cons type types-3.12)))
		      ((g-3.14)     (set! types-3.14 (cons type types-3.14)))
		      ((g-3.16)     (set! types-3.16 (cons type types-3.16)))
		      ((g-3.18)     (set! types-3.18 (cons type types-3.18)))
		      ((g-3.20)     (set! types-3.20 (cons type types-3.20)))
		      ((g-3.22)     (set! types-3.22 (cons type types-3.22)))
		      ((g-3.99)     (set! types-3.99 (cons type types-3.99)))
		      ((cairo)      (set! cairo-types (cons type cairo-types)))
		      ((cairo-810)  (set! cairo-types-810 (cons type cairo-types-810)))
		      ((cairo-912)  (set! cairo-types-912 (cons type cairo-types-912)))
		      (else  	      (if (not (member type types))
					  (set! types (cons type types))))))
		  (set! type #f)))
	    (set! sp i))))))

(define direct-types 
  (list (cons "void" #f)
	(cons "int" "INT")
	(cons "gint" "INT")
	(cons "gint32" "INT")
	(cons "guint32" "ULONG")
	(cons "gunichar" "ULONG")
	(cons "gunichar2" "INT")
	(cons "gulong" "ULONG")
	(cons "glong" "INT")
	(cons "gboolean" "BOOLEAN")
	(cons "gdouble" "DOUBLE")
	(cons "double" "DOUBLE")
	(cons "gfloat" "DOUBLE")
	(cons "char" "CHAR")
	(cons "gchar" "CHAR")
	(cons "char*" "String")
	(cons "gchar*" "String")
	(cons "guchar*" "String") ; added 30-Jul-02 then removed then put back... -- this is a real mess!

	(cons "guint" "ULONG")
	(cons "guint16" "INT")
	(cons "gint" "INT")
	(cons "gshort" "INT")
					;	(cons "gint16" "INT")
					;	(cons "guint8" "INT")
	(cons "guchar" "INT")
	(cons "gint8" "INT")
	(cons "gssize" "INT")
	(cons "gsize" "INT")
	(cons "xen" #t)
	(cons "etc" #t)
	
	;; since the various enums are handled directly (as ints) below, the associated types
	;;   need also to be direct (so that (= GDK_WHATEVER func-return-val) makes sense)
	;;   or should the constants be tagged? -- seems kinda silly
	
	(cons "GType" "ULONG")
	(cons "GQuark" "ULONG")
	(cons "GConnectFlags" "INT")
	(cons "GSignalMatchType" "INT")
	(cons "GSignalFlags" "INT")
					;(cons "GdkInputCondition" "INT")
	(cons "GdkCursorType" "INT")
	(cons "GdkDragAction" "INT")
					;(cons "GdkDragProtocol" "INT")
					;(cons "GdkAxisUse" "INT")
					;(cons "GdkGCValuesMask" "INT")
					;(cons "GdkFill" "INT")
					;(cons "GdkFunction" "INT")
					;(cons "GdkLineStyle" "INT")
					;(cons "GdkCapStyle" "INT")
					;(cons "GdkJoinStyle" "INT")
	(cons "GdkGrabStatus" "INT")
	(cons "GdkEventMask" "INT")
					;(cons "GdkImageType" "INT")
					;(cons "GdkInputSource" "INT")
					;(cons "GdkInputMode" "INT")
					;(cons "GdkNativeWindow" "ULONG")
	(cons "GdkModifierType" "INT")
					;(cons "GdkExtensionMode" "INT")
	(cons "PangoDirection" "INT")
					;(cons "GdkRgbDither" "INT")
	(cons "GdkPixbufAlphaMode" "INT")
	(cons "GdkPropMode" "INT")
					;(cons "GdkFillRule" "INT")
					;(cons "GdkOverlapType" "INT")
	(cons "GdkVisualType" "INT")
	(cons "GdkWindowType" "INT")
	(cons "GdkWindowState" "INT")
	(cons "GdkWMDecoration" "INT")
	(cons "GdkWMFunction" "INT")
	(cons "GdkWindowEdge" "INT")
	(cons "GtkAccelFlags" "INT")
					;(cons "GtkArrowType" "INT")
	(cons "GtkShadowType" "INT")
	(cons "GtkButtonBoxStyle" "INT")
					;(cons "GtkPathType" "INT")
					;(cons "GtkPathPriorityType" "INT")
	(cons "GtkPackType" "INT")
	(cons "GtkReliefStyle" "INT")
	(cons "GtkCalendarDisplayOptions" "INT")
	(cons "GtkCellRendererState" "INT")
					;(cons "GtkResizeMode" "INT")
					;(cons "GtkCurveType" "INT")
	(cons "GtkDialogFlags" "INT")
	(cons "GtkDestDefaults" "INT")
	(cons "GtkPositionType" "INT")
	(cons "GtkTextDirection" "INT")
	(cons "GtkStateFlags" "INT")
	(cons "GtkImageType" "INT")
					;(cons "GtkIconSize" "INT")
	(cons "GtkJustification" "INT")
	(cons "GtkMessageType" "INT")
	(cons "GtkButtonsType" "INT")
	(cons "GtkTargetFlags" "INT")
					;(cons "GtkProgressBarOrientation" "INT")
					;(cons "GtkUpdateType" "INT")
					;(cons "GtkMetricType" "INT")
	(cons "GtkPolicyType" "INT")
	(cons "GtkCornerType" "INT")
	(cons "GtkSizeGroupMode" "INT")
	(cons "GtkSpinButtonUpdatePolicy" "INT")
	(cons "GtkSpinType" "INT")
	(cons "GtkOrientation" "INT")
					;(cons "GtkExpanderStyle" "INT")
					;(cons "GtkAttachOptions" "INT")
	(cons "GtkTextSearchFlags" "INT")
	(cons "GtkTextWindowType" "INT")
	(cons "GtkWrapMode" "INT")
	(cons "GtkWindowPosition" "INT")
	(cons "GtkTreeViewColumnSizing" "INT")
	(cons "GtkTreeViewDropPosition" "INT")
					;(cons "GtkToolbarChildType" "INT")
	(cons "GtkToolbarStyle" "INT")
	(cons "GtkTreeModelFlags" "INT")
	(cons "GtkSelectionMode" "INT")
	(cons "GtkSortType" "INT")
	(cons "GtkDirectionType" "INT")
	(cons "GtkWindowType" "INT")
	(cons "GdkWindowTypeHint" "INT")
	(cons "GdkGravity" "INT")
	(cons "GdkWindowHints" "INT")
	(cons "GtkSizeRequestMode" "INT")
	
	(cons "GtkEntryIconPosition" "INT")
	
	(cons "PangoAttrType" "INT")
	(cons "PangoStyle" "INT")
	(cons "PangoWeight" "INT")
	(cons "PangoVariant" "INT")
	(cons "PangoStretch" "INT")
	(cons "PangoUnderline" "INT")
	(cons "PangoFontMask" "INT")
	(cons "PangoWrapMode" "INT")
	(cons "PangoEllipsizeMode" "INT")
	(cons "PangoAlignment" "INT")
	(cons "PangoCoverageLevel" "INT")
	(cons "PangoGlyph" "ULONG")
	(cons "PangoScript" "INT")
	
	(cons "GdkEventType" "INT")
					;(cons "GdkVisibilityState" "INT")
	(cons "GdkScrollDirection" "INT")
	(cons "GdkCrossingMode" "INT")
	(cons "GdkNotifyType" "INT")
	(cons "GdkSettingAction" "INT")
	(cons "GdkByteOrder" "INT")
					;(cons "GdkWChar" "ULONG")
	(cons "GtkFileChooserAction" "INT")
					;(cons "GtkUIManagerItemType" "INT")
	(cons "GtkFileFilterFlags" "INT")
	(cons "GtkIconLookupFlags" "INT")
	(cons "GtkIconThemeError" "INT")
	(cons "GtkScrollStep" "INT")
	(cons "GLogLevelFlags" "INT")
	(cons "GtkPackDirection" "INT")
	(cons "GtkIconViewDropPosition" "INT")
	(cons "GtkFileChooserConfirmation" "INT")
					;(cons "GtkFileChooserProp" "INT")
	(cons "GtkFileChooserError" "INT")
					;(cons "GtkLicense" "INT")

					;(cons "GtkWrapAllocationMode" "INT")
					;(cons "GtkWrapBoxSpreading" "INT")
					;(cons "GtkWrapBoxPacking" "INT")
	
	(cons "GtkSensitivityType" "INT")
	(cons "GtkTextBufferTargetInfo" "INT")
	(cons "GtkAssistantPageType" "INT")
	(cons "GtkCellRendererAccelMode" "INT")
	(cons "GtkRecentSortType" "INT")
	(cons "GtkRecentChooserError" "INT")
					;(cons "GtkRecentFilterFlags" "INT")
	(cons "GtkRecentManagerError" "INT")
	(cons "GtkTreeViewGridLines" "INT")

	(cons "GNormalizeMode" "INT")
	(cons "gunichar" "INT")
	(cons "gunichar*" "String")	
					;(cons "GtkPrintCapabilities" "INT")
	(cons "GtkPrintStatus" "INT")
	(cons "GtkPrintOperationResult" "INT")
	(cons "GtkPrintOperationAction" "INT")
	(cons "GtkPrintError" "INT")

	(cons "GtkRevealerTransitionType" "INT")
	(cons "GtkStackTransitionType" "INT")
	(cons "GtkTextViewLayer" "INT")
	(cons "GdkTouchpadGesturePhase" "INT")
	(cons "GtkLevelBarMode" "INT")
					;(cons "GdkWindowClass" "INT")
	(cons "GdkColorspace" "INT")
	(cons "GtkNotebookTab" "INT")
	(cons "GdkPixbufError" "INT")
	(cons "PangoRenderPart" "INT")
	(cons "GtkDragResult" "INT")
	(cons "GtkToolPaletteDragTargets" "INT")
	(cons "GtkInputPurpose" "INT")
	(cons "GtkInputHints" "INT")
	(cons "GdkFullscreenMode" "INT")
	(cons "GtkBaselinePosition" "INT")
	(cons "GtkPlacesOpenFlags" "INT")
	;(cons "GtkRegionFlags" "INT")
	
	(cons "cairo_status_t" "INT")
	(cons "cairo_content_t" "INT")
	(cons "cairo_operator_t" "INT")
	(cons "cairo_antialias_t" "INT")
	(cons "cairo_fill_rule_t" "INT")
	(cons "cairo_line_cap_t" "INT")
	(cons "cairo_line_join_t" "INT")
	(cons "cairo_font_slant_t" "INT")
	(cons "cairo_font_weight_t" "INT")
	(cons "cairo_subpixel_order_t" "INT")
	(cons "cairo_hint_style_t" "INT")
	(cons "cairo_hint_metrics_t" "INT")
	(cons "cairo_font_type_t" "INT")
	(cons "cairo_path_data_type_t" "INT")
	(cons "cairo_surface_type_t" "INT")
	(cons "cairo_format_t" "INT")
	(cons "cairo_pattern_type_t" "INT")
	(cons "cairo_extend_t" "INT")
	(cons "cairo_filter_t" "INT")
	(cons "cairo_bool_t" "INT")
	(cons "cairo_text_cluster_flags_t" "INT")

	(cons "bool" "BOOLEAN")
	
					;(cons "PangoRenderPart" "INT")
	(cons "PangoTabAlign" "INT")
					;(cons "GtkWidgetHelpType" "INT")
					;(cons "GtkWidgetFlags" "INT")
					;(cons "GtkRcTokenType" "INT")
	(cons "GtkTextExtendSelection" "INT")
					;(cons "GtkNotebookTab" "INT")
	(cons "GtkScrollType" "INT")
	(cons "GtkMovementStep" "INT")
	(cons "GtkMenuDirectionType" "INT")
	(cons "GtkDeleteType" "INT")
	(cons "GtkResponseType" "INT")
	(cons "GdkInterpType" "INT")
					;(cons "GdkPixbufError" "INT")
					;(cons "GdkColorspace" "INT")
					;(cons "GdkWindowAttributesType" "INT")
					;(cons "GdkWindowClass" "INT")
	(cons "GdkStatus" "INT")
					;(cons "GdkSubwindowMode" "INT")
	(cons "GdkPropertyState" "INT")
	(cons "GtkScrollablePolicy" "INT")

	(cons "GdkModifierIntent" "INT")
	(cons "GtkAlign" "INT")
					;(cons "GdkGLFlags" "INT")
	(cons "GtkShortcutType" "INT")
	(cons "GtkPopoverConstraint" "INT")
	(cons "GdkSeatCapabilities" "INT")
	(cons "GdkDragCancelReason" "INT")
	(cons "GdkAxisUse" "INT")
	(cons "GdkAxisFlags" "INT")
	(cons "GdkDeviceToolType" "INT")
	(cons "GdkSubpixelLayout" "INT")
	(cons "GdkDevicePadFeature" "INT")
	(cons "GdkAnchorHints" "INT")
	(cons "GtkPadActionType" "INT")

	(cons "GtkUnit" "INT")
	(cons "GtkPropagationPhase" "INT")
	(cons "GtkPageOrientation" "INT")
	(cons "GtkEventSequenceState" "INT")
	(cons "GtkPageSet" "INT")
	(cons "GtkPrintPages" "INT")
	(cons "GtkPrintDuplex" "INT")
	(cons "GtkPrintQuality" "INT")

	(cons "GtkEventControllerScrollFlags" "INT")
	(cons "GdkDeviceType" "INT")
	(cons "GtkIconSize" "INT")
	))

(define (c-to-xen-macro-name type str)
  (cond ((assoc str '(("INT"     . "C_int_to_Xen_integer")
		      ("DOUBLE"  . "C_double_to_Xen_real") 
		      ("BOOLEAN" . "C_bool_to_Xen_boolean")
		      ("ULONG"   . "C_ulong_to_Xen_ulong"))
		string=?) => cdr)
          ((not (string=? str "String")) (format #f "~A unknown" str))
          ((string=? type "guchar*")     (copy "C_to_Xen_String"))
          (else                          (copy "C_string_to_Xen_string"))))

(define (xen-to-c-macro-name str)
  (cond ((assoc str '(("INT"     . "Xen_integer_to_C_int")
		      ("DOUBLE"  . "Xen_real_to_C_double") 
		      ("BOOLEAN" . "Xen_boolean_to_C_bool")
		      ("ULONG"   . "Xen_ulong_to_C_ulong") 
		      ("String"  . "Xen_string_to_C_string"))
		string=?) => cdr)
	(else (format #f "~A unknown" str))))

(define (type-it type)
   (let ((typ (assoc type direct-types)))
     (if typ
	 (when (cdr typ)
	   (let ((c-name (cdr typ))) ; might be #f (see void case below)
	     (cond ((string? c-name)
		    (if (not (member type no-c-to-xen))
			(hey "#define C_to_Xen_~A(Arg) ~A(Arg)~%" (no-stars type) (c-to-xen-macro-name type c-name)))
		    (if (not (member type no-xen-to-c))
			(hey "#define Xen_to_C_~A(Arg) (~A)(~A(Arg))~%" (no-stars type) type (xen-to-c-macro-name c-name)))
		    (if (not (member type no-xen-p))
			(hey "#define Xen_is_~A(Arg) Xen_is_~A(Arg)~%" 
			     (no-stars type)
			     (cond ((assoc c-name '(("INT"    . "integer") 
						    ("DOUBLE" . "number") 
						    ("ULONG"  . "ulong")) 
					   string=?) => cdr)
				   (else (string-downcase c-name))))))
		   ((not c-name) ; void special case
		    (if (not (member type no-xen-p))
			(hey "#define Xen_is_~A(Arg) 1~%" (no-stars type)))
		    (if (not (member type no-xen-to-c))
			(hey "#define Xen_to_C_~A(Arg) ((gpointer)Arg)~%" (no-stars type))))
		   ((string=? type "etc") ; xen special case
		    (hey "#define Xen_is_etc(Arg) (Xen_is_list(Arg))~%"))
		   (else
		    (if (not (member type no-xen-p))
			(hey "#define Xen_is_~A(Arg) ((Xen_is_list(Arg)) && (Xen_list_length(Arg) > 2))~%" (no-stars type)))
		    (if (not (member type no-xen-to-c))
			(hey "#define Xen_to_C_~A(Arg) ((gpointer)Arg)~%" (no-stars type)))))))
	 
	 (if (not (or (member type '("lambda" "lambda_data" "GError*") string=?)
		      (find-callback 
		       (lambda (func)
			 (string=? type (symbol->string (car func)))))
		      (string=? type "GCallback")))
	     (let ((argwrap (cond ((or (has-stars type) 
				       (member type '("gpointer" "GClosureNotify") string=?))
				   (cond ((member type no-c-to-xen)
					  "_Ptr_1")
					 ((member type no-xen-p)
					  (if (member type no-xen-to-c) "_Ptr_2" "_Ptr_no_P"))
					 ((member type '("guint8*" "GtkRecentFilterInfo*") string=?)
					  "_Ptr_const")
					 (else "_Ptr")))
				  ((member type no-c-to-xen)
				   "_1")
				  ((not (member type no-xen-p)) "")
				  ((member type no-xen-to-c) "_no_p_2")
				  (else "_no_p"))))
	       (hey "Xm_type~A(~A, ~A)~%" argwrap
		    (no-stars type) 
		    type))))))


(define callback-name car)
(define callback-type cadr)
(define callback-func caddr)
(define callback-args cadddr)
(define (callback-gc func) (func 4))
(define (callback-version func) (and (> (length func) 5) (func 5)))

(define (func-type strs)
  (call-with-exit
   (lambda (return)
     (for-each
      (lambda (arg)
	(let ((callb (find-callback
		      (lambda (func)
			(and (string=? (car arg) (symbol->string (callback-name func)))
			     func)))))
	  (cond (callb                            (return (callback-name callb)))
		((string=? (car arg) "lambda")    (return 'lambda))
		((string=? (car arg) "GCallback") (return 'GCallback)))))
      strs)
     'fnc)))

(define (no-way str arg)
  (format () str arg))


(define-macro (make-fnc vname)
  (let ((cfnc (symbol "CFNC-" vname))
	(g-fnc (symbol "g-" vname))
	(types (symbol "types-" vname))
	(funcs (symbol "funcs-" vname))
	(strfnc (symbol "CSTR-" vname))
	(strings (symbol "strings-" vname))
	(names (symbol "names-" vname))
	(intfnc (symbol "CINT-" vname))
	(ints (symbol "ints-" vname))
	(castfnc (symbol "CCAST-" vname))
	(casts (symbol "casts-" vname))
	(chkfnc (symbol "CCHK-" vname))
	(checks (symbol "checks-" vname))
	(withfnc (symbol "with-" vname)))
    `(begin
       (define ,funcs ())
       (define ,strings ())
       (define ,ints ())
       (define ,names ())
       (define ,types ())
       (define ,casts ())
       (define ,checks ())

       (define* (,cfnc data spec)         ; CFNC-2.12
	 (let ((name (cadr-str data))
	       (args (caddr-str data)))
	   (if (hash-table-ref names name)
	       (format () "~A: ~A ~A~%" ',cfnc name data)
	       (let ((type (car-str data)))
		 (if (not (member type all-types))
		     (begin
		       (set! all-types (cons type all-types))
		       (set! ,types (cons type ,types))))
		 (let ((strs (parse-args args ',g-fnc)))
		   (if spec
		       (set! ,funcs (cons (list name type strs args spec) ,funcs))
		       (set! ,funcs (cons (list name type strs args) ,funcs)))
		   (hash-table-set! names name (func-type strs)))))))

       (define (,strfnc name)            ; CSTR-2.12
	 (if (assoc name ,names)
	     (format () "~A ~A~%" name ',strfnc)
	     (begin
	       (set! ,strings (cons name ,strings))
	       (set! ,names (cons (cons name 'string) ,names)))))

       (define* (,intfnc name type)      ; CINT-2.12
	 (save-declared-type name type ,vname)
	 (if (and type (not (assoc type direct-types)))
	     (format *stderr* "could be direct int: ~S (~S)~%" type name))
	 (if (hash-table-ref names name)
	     (format () "~A ~A~%" name ',intfnc)
	     (begin
	       (set! ,ints (cons name ,ints))
	       (hash-table-set! names name 'int))))

       (define (,castfnc name type)      ; CCAST-2.12
	 (if (hash-table-ref names name)
	     (format () "~A ~A~%" name ',castfnc)
	     (begin
	       (set! ,casts (cons (list name type) ,casts))
	       (hash-table-set! names name 'def))))

       (define (,chkfnc name type)       ; CCHK-2.12
	 (if (hash-table-ref names name)
	     (format () "~A ~A~%" name ',chkfnc)
	     (begin
	       (set! ,checks (cons (list name type) ,checks))
	       (hash-table-set! names name 'def))))

       (define (,withfnc dpy thunk)      ; with-2.12
	 (dpy (string-append "#if GTK_CHECK_VERSION(" (substring ,vname 0 1) ", " (substring ,vname 2) ", 0)~%"))
	 (thunk)
	 (dpy "#endif~%~%"))
       
       )))


(make-fnc "2.14")
(make-fnc "2.16")
(make-fnc "2.18")
(make-fnc "2.20")
(make-fnc "3.0") 
(make-fnc "3.2")
(make-fnc "3.4")
(make-fnc "3.6")
(make-fnc "3.8")
(make-fnc "3.10")
(make-fnc "3.12")
(make-fnc "3.14")
(make-fnc "3.16")
(make-fnc "3.18")
(make-fnc "3.20")
(make-fnc "3.22")
(make-fnc "3.99")

(define callbacks
  (list            
					;                       (list 'lambda2 ; unnamed gdk_window_invalidate_maybe_recurse argument (2.90.6 now)
					;			      "gboolean"
					;			      "child_func"
					;			      (parse-args "GdkWindow* window lambda_data func_info" 'callback)
					;			      'temporary)
   (list 'lambda3 ; unnamed gtk_accel_group_find argument
	 "gboolean"
	 "find_func"
	 (parse-args "GtkAccelKey* key GClosure* closure lambda_data func_info" 'callback)
	 'temporary) ; ??
   (list 'GtkCallback
	 "void"
	 "func2"
	 (parse-args "GtkWidget* w lambda_data func_info" 'callback)
	 'temporary)
   (list 'GSourceFunc
	 "gboolean"
	 "timer_func"
	 (parse-args "lambda_data func_info" 'callback)
	 'semi-permanent)
   (list 'GtkDestroyNotify
	 "void"
	 "destroy_func"
	 (parse-args "lambda_data func_info" 'callback)
	 'permanent)
   (list 'GdkFilterFunc
	 "GdkFilterReturn"
	 "filter_func"
	 (parse-args "GdkXEvent* xevent GdkEvent* event lambda_data func_info" 'callback)
	 'permanent)
   (list 'GdkEventFunc
	 "void"
	 "event_func"
	 (parse-args "GdkEvent* event lambda_data func_info" 'callback)
	 'permanent)
					;	(list 'GdkSpanFunc
					;	      "void"
					;	      "span_func"
					;	      (parse-args "GdkSpan* span lambda_data func_info" 'callback)
					;	      'temporary)
					;	(list 'GtkFunction
					;	      "gboolean"
					;	      "func1"
					;	      (parse-args "lambda_data func_info" 'callback)
					;	      'semi-permanent)
					;	(list 'GtkKeySnoopFunc
					;	      "gint"
					;	      "snoop_func"
					;	      (parse-args "GtkWidget* widget GdkEventKey* event lambda_data func_info" 'callback)
					;	      'semi-permanent)
					;   (list 'GtkMenuPositionFunc
					;	 "void"
					;	 "menu_position_func"
					;	 (parse-args "GtkMenu* menu gint* [x] gint* [y] gboolean* [push] lambda_data func_info" 'callback)
					;	 'permanent)
   (list 'GtkTextTagTableForeach
	 "void"
	 "text_tag_table_foreach"
	 (parse-args "GtkTextTag* tag lambda_data func_info" 'callback)
	 'temporary)
   (list 'GtkAccelMapForeach
	 "void"
	 "accel_map_foreach"
	 (parse-args "lambda_data func_info gchar* accel_path guint accel_key GdkModifierType accel_mods gboolean changed" 'callback)
	 'temporary)
   (list 'GtkTreeModelForeachFunc
	 "gboolean"
	 "model_func"
	 (parse-args "GtkTreeModel* model GtkTreePath* path GtkTreeIter* iter lambda_data func_info" 'callback)
	 'temporary)
   (list 'GtkTreeSelectionForeachFunc
	 "void"
	 "tree_selection_func"
	 (parse-args "GtkTreeModel* model GtkTreePath* path GtkTreeIter* iter lambda_data func_info" 'callback)
	 'temporary)
#|
   (list 'GtkClipboardReceivedFunc
	 "void"
	 "clip_received"
	 (parse-args "GtkClipboard* clipboard GtkSelectionData* selection_data lambda_data func_info" 'callback)
	 'temporary)
   (list 'GtkClipboardTextReceivedFunc
	 "void"
	 "clip_text_received"
	 (parse-args "GtkClipboard* clipboard gchar* text lambda_data func_info" 'callback)
	 'temporary)
   (list 'GtkClipboardTargetsReceivedFunc
	 "void"
	 "clip_targets_received"
	 (parse-args "GtkClipboard* clipboard GdkAtom* atoms gint n_atoms lambda_data func_info" 'callback)
	 'temporary)
|#
					;			(list 'GtkMenuDetachFunc
					;			      "void"
					;			      "menu_detach_func"
					;			      (parse-args "GtkWidget* attach_widget GtkMenu* menu" 'callback)
					;			      'permanent)
;;; detach func is not passed user-data, so it would have to be implemented by hand
   (list 'GtkTextCharPredicate
	 "gboolean"
	 "text_char_predicate"
	 (parse-args "gunichar ch lambda_data func_info" 'callback)
	 'temporary)
   (list 'GtkTreeViewColumnDropFunc
	 "gboolean"
	 "tree_column"
	 (parse-args "GtkTreeView* tree_view GtkTreeViewColumn* column GtkTreeViewColumn* prev_column GtkTreeViewColumn* next_column lambda_data func_info" 'callback)
	 'temporary)
   (list 'GtkTreeViewMappingFunc
	 "void"
	 "tree_mapping"
	 (parse-args "GtkTreeView* tree_view GtkTreePath* path lambda_data func_info" 'callback)
	 'temporary)
   (list 'GtkTreeViewSearchEqualFunc
	 "gboolean"
	 "tree_search"
	 (parse-args "GtkTreeModel* model gint column gchar* key GtkTreeIter* iter lambda_data func_info" 'callback)
	 'temporary)
   (list 'GtkTreeCellDataFunc
	 "void"
	 "cell_data"
	 (parse-args "GtkTreeViewColumn* tree_column GtkCellRenderer* cell GtkTreeModel* tree_model GtkTreeIter* iter lambda_data func_info" 'callback)
	 'permanent)
   (list 'GtkTreeIterCompareFunc
	 "gint"
	 "iter_compare"
	 (parse-args "GtkTreeModel* model GtkTreeIter* a GtkTreeIter* b lambda_data func_info" 'callback)
	 'permanent)
   (list 'GtkTreeSelectionFunc
	 "gboolean"
	 "tree_selection"
	 (parse-args "GtkTreeSelection* selection GtkTreeModel* model GtkTreePath* path gboolean path_currently_selected lambda_data func_info" 'callback)
	 'permanent)
#|
   (list 'GtkClipboardGetFunc
	 "void"
	 "clip_get"
	 (parse-args "GtkClipboard* clipboard GtkSelectionData* selection_data guint info lambda_data func_info" 'callback)
	 'permanent)
   (list 'GtkClipboardClearFunc
	 "void"
	 "clip_clear"
	 (parse-args "GtkClipboard* clipboard lambda_data func_info" 'callback)
	 'permanent)
|#
   
					; GCallback 'lambda can be whatever is indicated by caller (2 or more args)
   
   (list 'GtkFileFilterFunc
	 "gboolean"
	 "file_filter"
	 (parse-args "GtkFileFilterInfo* info lambda_data func_info" 'callback)
	 'permanent)
   (list 'GtkEntryCompletionMatchFunc
	 "gboolean"
	 "entry_completion_match"
	 (parse-args "GtkEntryCompletion* completion gchar* key GtkTreeIter* iter lambda_data func_info" 'callback)
	 'permanent)
   
   (list 'GtkTreeViewRowSeparatorFunc
	 "gboolean"
	 "row_separator"
	 (parse-args "GtkTreeModel* model GtkTreeIter* iter lambda_data func_info" 'callback)
	 'permanent)
   (list 'GtkIconViewForeachFunc
	 "void"
	 "icon_view_foreach"
	 (parse-args "GtkIconView* icon_view GtkTreePath* path lambda_data func_info" 'callback)
	 'permanent)
#|   
   (list 'GtkClipboardImageReceivedFunc
	 "void"
	 "clip_image_received"
	 (parse-args "GtkClipboard* clipboard GdkPixbuf* pixbuf lambda_data func_info" 'callback) ; 'callback)
	 ;; these arg types are not new in 256, but this parse-args precedes the basic ones, so comment out the callback
	 ;; the problem here (and below callback) is that parse-args sees a new type (new to it so far),
	 ;;   and chooses which type list to put it on based on the "extra" arg -- since these types
	 ;;   are not new in version 2.5.6, we don't want the callback flag to sequester them
	 ;;   on the 256-type list.
	 'permanent)
|#
   (list 'GLogFunc
	 "void"
	 "g_message_log_func"
	 (parse-args "gchar* domain GLogLevelFlags log_level gchar* message lambda_data func_info" 'callback)
	 'permanent)
   
#|
   (list 'GtkClipboardRichTextReceivedFunc
	 "void"
	 "clip_rich_text_received"
	 (parse-args "GtkClipboard* clipboard GdkAtom format guint8* text gsize length lambda_data func_info" 'callback); 'callback)
	 ;; guint8* is const
	 'permanent-gcc)
|#
					;			(list 'GtkRecentFilterFunc
					;			      "gboolean"
					;			      "recent_filter"
					;			      (parse-args "GtkRecentFilterInfo* filter_info lambda_data func_info" 'callback)
					;			      ;; const filter info
					;			      'permanent-gcc)
   (list 'GtkTreeViewSearchPositionFunc
	 "void"
	 "search_position"
	 (parse-args "GtkTreeView* tree_view GtkWidget* search_dialog lambda_data func_info" 'callback)
	 'permanent)
   (list 'GtkAssistantPageFunc
	 "gint"
	 "page_func"
	 (parse-args "gint current_page lambda_data func_info" 'callback)
	 'permanent)
					;			(list 'GtkLinkButtonUriFunc
					;			      "void"
					;			      "link_button_uri"
					;			      (parse-args "GtkLinkButton* button gchar* link lambda_data func_info" 'callback)
					;			      ;; const gchar *link
					;			      'permanent)
   (list 'GtkRecentSortFunc
	 "gint"
	 "recent_sort"
	 (parse-args "GtkRecentInfo* a GtkRecentInfo* b lambda_data func_info" 'callback)
	 'permanent)
   

   (list 'GdkSeatGrabPrepareFunc
	 "void"
	 "prepare_func"
	 (parse-args "GdkSeat* seat GdkWindow* window lambda_data func_info" 'g-3.20)
	 'permanent
	 "3.20")

   (list 'GtkDrawingAreaFunc
	 "void"
	 "draw_func"
	 (parse-args "GtkDrawingArea* self lambda_data func_info" 'g-3.99)
	 'permanent
	 "3.99")
   ))


(define (find-callback test)
  (let find-callback-1 ((test test)
			(funcs callbacks))
    (and (pair? funcs)
	 (or (test (car funcs))
	     (find-callback-1 test (cdr funcs))))))

(define* (CFNC data spec spec-data) ; 'const -> const for arg cast, 'etc for ... args, 'free -> must free C val before return
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (hash-table-ref names name)
	(no-way "~A CFNC~%" name)
	(let ((type (car-str data)))
	  (if (not (member type all-types)) (set! all-types (cons type all-types)))
	  (if (not (member type types))
	      (set! types (cons type types)))
	  (let ((strs (parse-args args 'ok)))
	    (set! funcs 
		  (cons (if spec 
			    (list name type strs args spec spec-data)
			    (list name type strs args)) 
			funcs))
	    (hash-table-set! names name (func-type strs)))))))

(define (CFNC-PA data min-len max-len types)
  (CFNC data 'etc (list min-len max-len types)))

(define* (CAIRO-FUNC data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (hash-table-ref names name)
	(no-way "CAIRO-FUNC: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! cairo-types (cons type cairo-types))))
	  (let ((strs (parse-args args 'cairo)))
	    (set! cairo-funcs 
		  (cons (if spec
			    (list name type strs args spec)
			    (list name type strs args))
			cairo-funcs))
	    (hash-table-set! names name (func-type strs)))))))

(define* (CAIRO-PNG-FUNC data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (hash-table-ref names name)
	(no-way "CAIRO-PNG-FUNC: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! cairo-types (cons type cairo-types))))
	  (let ((strs (parse-args args 'cairo)))
	    (set! cairo-png-funcs 
		  (cons (if spec 
			    (list name type strs args spec)
			    (list name type strs args))
			cairo-png-funcs))
	    (hash-table-set! names name (func-type strs)))))))

(define* (CAIRO-FUNC-810 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (hash-table-ref names name)
	(no-way "CAIRO-FUNC-810: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! cairo-types-810 (cons type cairo-types-810))))
	  (let ((strs (parse-args args 'cairo-810)))
	    (set! cairo-funcs-810 
		  (cons (if spec 
			    (list name type strs args spec)
			    (list name type strs args))
			cairo-funcs-810))
	    (hash-table-set! names name (func-type strs)))))))

(define* (CAIRO-FUNC-912 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (hash-table-ref names name)
	(no-way "CAIRO-FUNC-912: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! cairo-types-912 (cons type cairo-types-912))))
	  (let ((strs (parse-args args 'cairo-912)))
	    (set! cairo-funcs-912 
		  (cons (if spec
			    (list name type strs args spec)
			    (list name type strs args))
			cairo-funcs-912))
	    (hash-table-set! names name (func-type strs)))))))


(define (helpify name type args)
  (let ((initial (format #f "  #define H_~A \"~A ~A(" name type name)))
    (let ((line-len (length initial))
	  (len (length args))
	  (typed #f)
	  (help-max 100))
      (hoy initial)
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(let ((ch (args i)))
	  (if (char=? ch #\space)
	      (if typed
		  (begin
		    (hoyc ", ")
		    (set! line-len (+ line-len 2))
		    (if (> line-len help-max)
			(begin
			  (hoy "\\~%")
			  (set! line-len 0)))
		    (set! typed #f))
		  (begin
		    (set! line-len (+ 1 line-len))
		    (hoyc " ")
		    (set! typed #t)))
	      (if (not (memv ch '(#\@ #\#)))
		  (begin
		    (set! line-len (+ 1 line-len))
		    (hoyc ch))))))
      (hoy ")\"~%"))))

(define (CATOM name)
  (if (hash-table-ref names name)
      (no-way "~A CATOM~%" name)
      (begin
	(set! atoms (cons name atoms))
	(hash-table-set! names name 'atom))))


(define (CSTR name)
  (if (hash-table-ref names name)
      (no-way "~A CSTR~%" name)
      (begin
	(set! strings (cons name strings))
	(hash-table-set! names name 'string))))


(define (CDBL name)
  (if (hash-table-ref names name)
      (no-way "~A CDBL~%" name)
      (begin
	(set! dbls (cons name dbls))
	(hash-table-set! names name 'dbl))))

(define declared-types ()) ; list of string type names
(define declared-names ()) ; list of (enum-name type-name version-string)

(define (save-declared-type name type version)
  (when (string? type)
    (if (not (member type declared-types))
	(set! declared-types (cons type declared-types)))
    (if (or (memv (type 0) '(#\G #\P))
	    (char=? (name 0) #\C))
	(set! declared-names (cons (list name type version) declared-names)))))

(define* (CINT name type)
  (save-declared-type name type "2.0")
  (if (and type (not (assoc type direct-types)))
      (format *stderr* "could be direct int: ~S (~S)~%" type name))
  (if (hash-table-ref names name)
      (no-way "~A CINT~%" name)
      (begin
	(set! ints (cons name ints))
	(hash-table-set! names name 'int))))


(define* (CAIRO-INT name type)
  (save-declared-type name type "2.0")
  (if (hash-table-ref names name)
      (no-way "~A CAIRO-INT~%" name)
      (begin
	(set! cairo-ints (cons name cairo-ints))
	(hash-table-set! names name 'int))))

(define* (CAIRO-INT-810 name type)
  ;(save-declared-type name type)
  (if (hash-table-ref names name)
      (no-way "~A CAIRO-INT-810~%" name)
      (begin
	(set! cairo-ints-810 (cons name cairo-ints-810))
	(hash-table-set! names name 'int))))

(define* (CAIRO-INT-912 name type)
  ;(save-declared-type name type)
  (if (hash-table-ref names name)
      (no-way "~A CAIRO-INT-912~%" name)
      (begin
	(set! cairo-ints-912 (cons name cairo-ints-912))
	(hash-table-set! names name 'int))))

(define (CAIRO-STRING-912 name)
  (if (hash-table-ref names name)
      (no-way "~A CAIRO-STRING-912~%" name)
      (begin
	(set! cairo-strings-912 (cons name cairo-strings-912))
	(hash-table-set! names name 'string))))


(define (CCAST name type) ; this is the cast (type *)obj essentially but here it's (list type* (cadr obj))
  (if (hash-table-ref names name)
      (no-way "~A CCAST~%" name)
      (begin
	;;(if (not (member type types))
	;;    (set! types (cons type types)))
	(set! casts (cons (list name type) casts))
	(hash-table-set! names name 'def))))


(define (CCHK name type)
  (if (hash-table-ref names name)
      (no-way "~A CCHK~%" name)
      (begin
	(set! checks (cons (list name type) checks))
	(hash-table-set! names name 'def))))


(define (no-arg name)
  (let ((len (length name)))
    (call-with-exit
     (lambda (return)
       (do ((i 0 (+ i 1)))
	   ((= i len) name)
	 (if (char=? (name i) #\()
	     (return (substring name 0 i))))))))

;;; ---------------------------------------- read data ---------------------------------------- 
(load "xgdata.scm")

					;(define listable-types (list "gint8*" "int*" "gint*" "gdouble*"))
(define listable-types ())
(for-each
 (lambda (type)
   (let ((dereftype (let ((len (- (length type) 1)))
		      (and (char=? (type len) #\*)
			   ;; these are surely strings (and set would need Xen_to_C_gchar etc)
			   (not (member type '("char*" "GError*" "GError**" "gchar*") string=?))
			   (substring type 0 len)))))
     (if (and dereftype
	      (assoc dereftype direct-types))
	 (set! listable-types (cons type listable-types)))))
 types)

(define (with-cairo dpy thunk)
  (thunk)
  )

(define (with-cairo-png dpy thunk)
  (thunk)
  )

(define (with-cairo-810 dpy thunk)
  (dpy "#if HAVE_CAIRO_1_8~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-cairo-912 dpy thunk)
  (dpy "#if HAVE_CAIRO_1_9_12 && GTK_CHECK_VERSION(3, 0, 0)~%")
  (thunk)
  (dpy "#endif~%~%"))



(define all-ntypes (list types-2.14 types-2.16 types-2.18 types-2.20 
			types-3.0 types-3.2 types-3.4 types-3.6 types-3.8 types-3.10 types-3.12 types-3.14 types-3.16 types-3.18 
			types-3.20 types-3.22 types-3.99
			cairo-types cairo-types-810 cairo-types-912))
(define all-ntype-withs (list with-2.14 with-2.16 with-2.18 with-2.20 
			     with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12 with-3.14 with-3.16 with-3.18 
			     with-3.20 with-3.22 with-3.99
			     with-cairo with-cairo-810 with-cairo-912))

(define all-funcs (list funcs-2.14 funcs-2.16 funcs-2.18 funcs-2.20 
			funcs-3.0 funcs-3.2 funcs-3.4 funcs-3.6 funcs-3.8 funcs-3.10 funcs-3.12 funcs-3.14 funcs-3.16 funcs-3.18 
			funcs-3.20 funcs-3.22 funcs-3.99
			cairo-funcs cairo-png-funcs cairo-funcs-810 cairo-funcs-912))
(define all-func-withs (list with-2.14 with-2.16 with-2.18 with-2.20 
			     with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12 with-3.14 with-3.16 with-3.18 
			     with-3.20 with-3.22 with-3.99
			     with-cairo with-cairo-png with-cairo-810 with-cairo-912))

(define all-ints (list ints-2.14 ints-2.16 ints-2.18  
		       ints-3.0 ints-3.2 ints-3.4 ints-3.6 ints-3.8 ints-3.10 ints-3.12 ints-3.14 ints-3.16 ints-3.18 
		       ints-3.20 ints-3.22 ints-3.99
		       cairo-ints cairo-ints-810 cairo-ints-912))
(define all-int-withs (list with-2.14 with-2.16 with-2.18 
			    with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12 with-3.14 with-3.16 with-3.18 
			    with-3.20 with-3.22 with-3.99
			    with-cairo with-cairo-810 with-cairo-912))

(define all-casts (list casts-2.14 casts-2.16 casts-2.18 casts-2.20 
			casts-3.0 casts-3.2 casts-3.4 casts-3.6 casts-3.8 casts-3.10 casts-3.12 casts-3.14 casts-3.16 casts-3.18 
			casts-3.20 casts-3.22 casts-3.99
			))
(define all-cast-withs (list with-2.14 with-2.16 with-2.18 with-2.20 
			     with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12 with-3.14 with-3.16 with-3.18 
			     with-3.20 with-3.22 with-3.99
			     ))

(define all-checks (list checks-2.14 checks-2.16 checks-2.18 checks-2.20 
			 checks-3.0 checks-3.2 checks-3.4 checks-3.6 checks-3.8 checks-3.10 checks-3.12 checks-3.14 checks-3.16 checks-3.18 
			 checks-3.20 checks-3.22 checks-3.99
			 ))
(define all-check-withs (list with-2.14 with-2.16 with-2.18 with-2.20 
			      with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12 with-3.14 with-3.16 with-3.18 
			      with-3.20 with-3.22 with-3.99
			      ))

(define all-strings (list strings-2.14 strings-2.16 
			  strings-3.0 strings-3.2 strings-3.4 strings-3.6 strings-3.8 strings-3.10 strings-3.12  strings-3.14 strings-3.16 strings-3.18 
			  strings-3.20 strings-3.22 strings-3.99
			  cairo-strings-912))
(define all-string-withs (list with-2.14 with-2.16 
			       with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12  with-3.14 with-3.16 with-3.18 
			       with-3.20 with-3.22 with-3.99
			       with-cairo-912))



;;; ---------------------------------------- write output file ----------------------------------------
(hey "/* xg.c: s7, Ruby, and Forth bindings for gtk/pango/cairo, some of glib~%")
(hey " *   this file generated automatically from makexg.scm and xgdata.scm~%")
(hey " *   needs xen.h~%")
(hey " *~%")
(hey " * reference args initial values are usually ignored, resultant values are returned in a list.~%")
(hey " * null ptrs are passed and returned as #f, trailing \"user_data\" callback function arguments are optional (default: #f).~%")
(hey " * where supported, \"...\" args are passed as a list, trailing NULL or -1 should be omitted.~%")
(hey " * 'xg is added to *features*~%")
(hey " *~%")
(hey " * added funcs:~%")
(hey " *    (xg-version): date string.~%")
(hey " *    (c-array->list arr len) derefs each member of arr, returning lisp list, len=#f: null terminated array~%")
(hey " *    (list->c-array lst ctype) packages each member of list as c-type \"type\" returning (wrapped) c array~%")
(hey " *    (GtkTextIter): GtkTextIter struct~%")
(hey " *    (GtkTreeIter): GtkTreeIter struct~%")
(hey " *    (PangoRectangle): PangoRectangle struct~%")
(hey " *    (cairo_matrix_t): cairo_matrix_t struct (if cairo)~%")
(hey " *~%")
(hey " * omitted functions and macros:~%")
(hey " *     anything with a va_list or GtkArg* argument.~%")
(hey " *     most of the unusual keysym names~%")
(hey " *     all *_CLASS, *_IFACE macros, *_get_type functions~%")
(hey " *     win32-specific functions~%")
(hey " *~%")
(hey " * HISTORY:~%")
(hey " *~%")
(hey " *     28-Jul-17: scheme Init_libxg arg added.~%")
(hey " *     --------~%")
(hey " *     17-Mar-16: gtk_enum_t for better signature checks.~%")
(hey " *     --------~%")
(hey " *     29-Oct:    removed ->string.~%")
(hey " *     21-Aug-15: procedure-signature changes.~%")
(hey " *     --------~%")
(hey " *     27-Dec:    integer procedure stuff.~%")
(hey " *     16-Apr:    changed max-args to 8.~%")
(hey " *     6-Mar:     changed most macros.~%")
(hey " *     21-Feb-14: changed _p to _is_.~%")
(hey " *     --------~%")
(hey " *     3-Sep:     use symbol directly in type checks, not the symbol name.~%")
(hey " *     18-Aug:    changed the gtk version macros to reflect the version number.~%")
(hey " *     7-Jun-13:  added mixed arg types to the ... arg lists.~%")
(hey " *     --------~%")
(hey " *     19-Aug-10: removed lots of Gdk stuff -- we assume Gtk 2.9 and cairo now.~%")
(hey " *     28-Jan-10: removed the rest of the struct accessors.~%")
(hey " *     --------~%")
(hey " *     16-Dec-09: removed Guile support.~%")
(hey " *     --------~%")
(hey " *     16-Oct:    removed Gauche support.~%")
(hey " *     1-Sep:     s7 support.~%")
(hey " *     8-Jul-08:  started removing all struct accessors (for Gtk 3).~%")
(hey " *     --------~%")
(hey " *     9-Mar:     removed all *_get_type functions (nearly 300!).~%")
(hey " *     5-Mar-07:  cairo and more gtkprint.~%")
(hey " *     --------~%")
(hey " *     26-Aug:    removed --with-x11, WITH_GTK_AND_X11, xg-x11.h.~%")
(hey " *     4-Aug:     added a form of g_object_get and gtk_settings_get_for_screen.~%")
(hey " *     20-Jul:    gtkprint stuff.~%")
(hey " *     17-Jul:    g_signal_connect and other related macros.~%")
(hey " *     21-Apr:    Gauche support.~%")
(hey " *     29-Mar:    Forth support.~%")
(hey " *     7-Mar-06:  if g_set_error, return the error message, not the GError pointer~%")
(hey " *     --------~%")
(hey " *     9-Jul:     Collapse 2.3.* into 2.3.6, 2.5.* into 2.5.6.~%")
(hey " *     13-Jun:    folded xg-ruby.c into xg.c.~%")
(hey " *     21-Feb:    changed libxm to libxg, xm-version to xg-version.~%")
(hey " *     10-Jan:    plugged some memory leaks.~%")
(hey " *     4-Jan-05:  removed deprecated Xen_VECTOR_ELEMENTS.~%")
(hey " *     --------~%")
(hey " *     8-Dec:     added some g_log handler funcs.~%")
(hey " *     6-Dec:     check for lost callback context.~%")
(hey " *                tightened type (pointer) checking considerably (#f only acceptable if explicit @ used in xgdata.scm).~%")
(hey " *     3-Dec:     changed GPOINTER cast func to accept non-lists.~%")
(hey " *     27-Aug:    removed the PANGO_ENGINE and PANGO_BACKEND stuff.~%")
(hey " *     2-Jun:     gdk_atom_name needs to free return value~%")
(hey " *     28-May:    GtkFileSelection struct support put back in -- need ok_button et al.~%")
(hey " *     14-Apr:    make-target-entry.~%")
(hey " *     4-Apr:     various additions, deletions, and bugfixes for snd-test 26~%")
(hey " *     29-Mar:    support for some ... args.~%")
(hey " *     22-Mar:    g_source_remove and related changes.~%")
(hey " *     12-Feb-04: g_list_nth_data (Kjetil S. Matheussen).~%")
(hey " *     --------~%")
(hey " *     15-Sep:    removed client_window GtkIMMulticontext struct field.~%")
(hey " *     26-May:    removed nugatory GdkInputFunction stuff and some unused type converters.~%")
(hey " *     1-Apr:     gdk_property_get uses scm_mem2string in some cases now.~%")
(hey " *     31-Mar:    gchar* -> xen string bugfix (thanks to Friedrich Delgado Friedrichs).~%")
(hey " *     10-Mar-03: Ruby Xm_Version.~%")
(hey " *     --------~%")
(hey " *     18-Nov:    Ruby/Gtk bugfixes.~%")
(hey " *     25-Oct:    removed (deprecated) gdk_set_pointer_hooks~%")
(hey " *     31-Jul:    removed GTK 1.n support~%")
(hey " *     24-Jul:    changed Guile prefix (R5RS reserves vertical-bar).~%")
(hey " *     19-Jul:    XG_FIELD_PRE for change from using vertical-bar (reserved in R5RS)~%")
(hey " *     2-Jun:     removed deprecated and broken stuff~%")
(hey " *     12-Mar:    support for GtkDestroyNotify callbacks~%")
(hey " *                Ruby support via xg-ruby.c~%")
(hey " *     21-Feb:    #f=NULL throughout, gdk-pixbuf, GTypes.~%")
(hey " *     11-Feb-02: initial version.~%")
(hey " */~%~%")

(hey "#include \"mus-config.h\"~%~%")

(hey "#define HAVE_CAIRO_1_8    ((CAIRO_VERSION_MAJOR >= 1) && (CAIRO_VERSION_MINOR >= 8))~%")
(hey "#define HAVE_CAIRO_1_9_12 ((CAIRO_VERSION_MAJOR >= 1) && (CAIRO_VERSION_MINOR >= 9) && (CAIRO_VERSION_MICRO >= 12))~%~%")

(hey "#if ((!__NetBSD__) && ((_MSC_VER) || (!defined(__STC__)) || (defined(__STDC_VERSION__) && (__STDC_VERSION__ < 199901L))))~%")
(hey "  #define __func__ __FUNCTION__~%")
(hey "#endif~%~%")

(hey "#if HAVE_EXTENSION_LANGUAGE~%~%")

;(hey "#if UNDEF_USE_SND~%  #undef USE_SND~%  #define USE_SND 0~%#endif~%~%")

(hey "#include <string.h>~%")
(hey "#include <stdlib.h>~%")
(hey "#include <stdint.h>~%")
(hey "#include <inttypes.h>~%~%")

(hey "#include <glib.h>~%")
(hey "#include <gdk/gdk.h>~%")
(hey "#include <gtk/gtk.h>~%")
(hey "#if (!GTK_CHECK_VERSION(3, 0, 0))~%")
(hey "  #include <gdk/gdkkeysyms.h>~%")
(hey "#endif~%")
(hey "#include <glib-object.h>~%")
(hey "#include <pango/pango.h>~%")
(with-cairo #f (lambda () (hey "#include <cairo/cairo.h>~%")))

(hey "#if USE_SND~%")
(hey "  /* USE_SND causes xm to use Snd's error handlers which are much smarter than xen's fallback versions */~%")
(hey "  #include \"snd.h\"~%")
(hey "#else~%")
(hey "  #include \"xen.h\"~%")
(hey "  #define NOT_A_GC_LOC -1~%")
(hey "#endif~%")

(hey "~%#ifndef PROC_FALSE~%")
(hey "  #if HAVE_RUBY~%")
(hey "    #define PROC_FALSE \"false\"~%")
(hey "    #define PROC_TRUE \"true\"~%")
(hey "  #else~%")
(hey "    #define PROC_FALSE \"#f\"~%")
(hey "    #define PROC_TRUE  \"#t\"~%")
(hey "  #endif~%")
(hey "#endif~%~%")

;;; ----------------
(hay "/* libgtk_s7.c */~%~%")
(hay "#include <stdlib.h>~%")
(hay "#include <stdio.h>~%")
(hay "#include <string.h>~%")
(hay "#include <math.h>~%")
(hay "#include <stdbool.h>~%")
(hay "#include \"s7.h\"~%~%")
(hay "#define HAVE_CAIRO_1_8    ((CAIRO_VERSION_MAJOR >= 1) && (CAIRO_VERSION_MINOR >= 8))~%")
(hay "#define HAVE_CAIRO_1_9_12 ((CAIRO_VERSION_MAJOR >= 1) && (CAIRO_VERSION_MINOR >= 9) && (CAIRO_VERSION_MICRO >= 12))~%~%")

(hay "#if ((!__NetBSD__) && ((_MSC_VER) || (!defined(__STC__)) || (defined(__STDC_VERSION__) && (__STDC_VERSION__ < 199901L))))~%")
(hay "  #define __func__ __FUNCTION__~%")
(hay "#endif~%~%")
(hay "#include <string.h>~%")
(hay "#include <stdlib.h>~%")
(hay "#include <stdint.h>~%")
(hay "#include <inttypes.h>~%~%")
(hay "#include <glib.h>~%")
(hay "#include <gdk/gdk.h>~%")
(hay "#include <gtk/gtk.h>~%")
(hay "#if (!GTK_CHECK_VERSION(3, 0, 0))~%")
(hay "  #include <gdk/gdkkeysyms.h>~%")
(hay "#endif~%")
(hay "#include <glib-object.h>~%")
(hay "#include <pango/pango.h>~%")
(with-cairo #f (lambda () (hay "#include <cairo/cairo.h>~%")))
(hay "static s7_pointer lg_true, lg_false;~%")
(hay "static bool lg_boolean(s7_pointer val) {return(val != lg_false);}~%")
(hay "#define s7_make_type_with_c_pointer(Sc, Type, Ptr) s7_make_c_pointer_with_type(Sc, Ptr, Type, lg_false)~%")
(hay "~%~%")
;;; ----------------


(hey "/* -------------------------------- GC -------------------------------- */~%")
(hey "static Xen_object_type_t xm_obj_tag;~%")
(hay "static int xm_obj_tag;~%")

(hey "#if HAVE_RUBY~%")
(hey "static void *xm_obj_free(Xen obj)~%")
(hey "{~%")
(hey "  void *xobj;~%")
(hey "  xobj = (void *)obj;~%")
(hey "  free(xobj);~%")
(hey "  return(NULL);~%")
(hey "}~%")
(hey "#endif~%")

(hey "#if HAVE_FORTH~%")
(hey "static void xm_obj_free(Xen obj)~%")
(hey "{~%")
(hey "  void *val;~%")
(hey "  val = (void *)Xen_object_ref(obj);~%")
(hey "  free(val);~%")
(hey "}~%")
(hey "#endif~%")

(hey "#if HAVE_SCHEME~%")
(hoy "static void xm_obj_free(void *val)~%")
(hoy "{~%")
(hoy "  free(val);~%")
(hoy "}~%")
(hoy "static bool s7_equalp_xm(void *x1, void *x2)~%")
(hoy "{~%")
(hoy "  return(x1 == x2);~%")
(hoy "}~%")
(hey "#endif~%")

(hey "static Xen make_xm_obj(void *ptr)~%")
(hey "{~%")
(hey "  return(Xen_make_object(xm_obj_tag, ptr, 0, xm_obj_free));~%")
(hey "}~%")

(hay "static s7_pointer make_xm_obj(s7_scheme *sc, void *ptr)~%")
(hay "{~%")
(hay "  return(s7_make_c_object(sc, xm_obj_tag, ptr));~%")
(hay "}~%")

(hey "static void define_xm_obj(void)~%")
(hay "static void define_xm_obj(s7_scheme *sc)~%")
(hoy "{~%")
(hey "#if HAVE_SCHEME~%")
(hey "  xm_obj_tag = s7_make_c_type(s7, \"<XmObj>\");~%")
(hay "  xm_obj_tag = s7_make_c_type(sc, \"XgObj\");~%")
(hey "  s7_c_type_set_free(s7, xm_obj_tag, xm_obj_free);~%")
(hay "  s7_c_type_set_free(sc, xm_obj_tag, xm_obj_free);~%")
(hey "  s7_c_type_set_equal(s7, xm_obj_tag, s7_equalp_xm);~%")
(hay "  s7_c_type_set_equal(sc, xm_obj_tag, s7_equalp_xm);~%")
(hey "#else~%")
(hey "  xm_obj_tag = Xen_make_object_type(\"XmObj\", sizeof(void *));~%")
(hey "#endif~%")
(hey "#if HAVE_FORTH~%")
(hey "  fth_set_object_free(xm_obj_tag, xm_obj_free);~%")
(hey "#endif~%")
(hoy "}  ~%")
(hoy "~%")

(hey "/* prefix for all names */~%")
(hey "#if HAVE_SCHEME~%")
(hey "  #define Xg_pre \"\"~%")
(hey "  #define Xg_field_pre \".\"~%")
(hey "  #define Xg_post \"\"~%")
(hey "#endif~%")
(hey "#if HAVE_RUBY~%")
(hey "/* for Ruby, XG PRE needs to be uppercase */~%")
(hey "  #define Xg_pre \"R\"~%")
(hey "  #define Xg_post \"\"~%")
(hey "  #define Xg_field_pre \"R\"~%")
(hey "#endif~%")
(hey "#if HAVE_FORTH~%")
(hey "  #define Xg_pre \"F\"~%")
(hey "  #define Xg_post \"\"~%")
(hey "  #define Xg_field_pre \"F\"~%")
(hey "#endif~%")
(hey "~%")


(hey "static Xen xg_~A_symbol" (no-stars (car all-types)))
(for-each
 (lambda (typ)
   (hey ", xg_~A_symbol" (no-stars typ)))
 (cdr all-types))

(define other-types 
  (list 'idler 'GtkCellRendererPixbuf_ ;'GtkScrollbar_ 
	'GtkSeparator_ 'GtkSeparatorMenuItem_
	'GdkEventExpose_ 'GdkEventNoExpose_ 'GdkEventVisibility_ 'GdkEventButton_ ;'GdkEventScroll_ 
	'GdkEventCrossing_
	'GdkEventFocus_ 'GdkEventConfigure_ 'GdkEventProperty_ 'GdkEventSelection_ 'GdkEventProximity_ 'GdkEventSetting_
	'GdkEventWindowState_ 'GdkEventDND_ 'GtkFileChooserDialog_ 'GtkFileChooserWidget_ 'GtkColorButton_ 'GtkAccelMap
	'GtkCellRendererCombo_ 'GtkCellRendererProgress_ 'GtkCellRendererAccel_ 'GtkCellRendererSpin_ 'GtkRecentChooserDialog_
	'GtkRecentChooserWidget_ 'GtkCellRendererSpinner_ 'gboolean_
	'GtkFontChooserDialog_ 'GtkFontChooserWidget_ 'GtkColorChooserDialog_ 'GtkColorChooserWidget_ 'GtkColorWidget_
	'GtkGestureLongPress_ 
	))

(for-each
 (lambda (typ)
   (if (not (member typ all-types))
       (hey ", xg_~A_symbol" typ)))
 other-types)
 
(hey ";~%~%")

;;; ----------------
(let ((ctr 0))
  (hay "static s7_pointer ~A_sym" (no-stars (car all-types)))
  (for-each
   (lambda (typ)
     (set! ctr (+ ctr 1))
     (if (zero? (modulo ctr 5))
	 (begin
	   (set! ctr 0)
	   (hay ",~%~NC~A_sym" 18 #\space (no-stars typ)))
	 (hay ", ~A_sym" (no-stars typ))))
   (cdr all-types))
  (for-each
   (lambda (typ)
     (unless (member typ all-types)
       (set! ctr (+ ctr 1))
       (if (zero? (modulo ctr 5))
	   (begin
	     (set! ctr 0)
	     (hay ",~%~NC~A_sym" 18 #\space typ))
	   (hay ", ~A_sym" typ))))
   other-types)
  (hay ";~%~%"))
;;; ----------------


(hey "#define wrap_for_Xen(Name, Value) Xen_list_2(xg_ ## Name ## _symbol, Xen_wrap_C_pointer(Value))~%")
(hey "#define is_wrapped(Name, Value) (Xen_is_pair(Value) && (Xen_car(Value) == xg_ ## Name ## _symbol))~%")
(hey "~%")
(hey "#define Xm_type(Name, XType) \\~%")
;; these are not pointers, so should not use wrap_c_pointer and friends 
(hey "  static Xen C_to_Xen_ ## Name (XType val) {return(Xen_list_2(xg_ ## Name ## _symbol, C_ulong_to_Xen_ulong(val)));} \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {return((XType)Xen_ulong_to_C_ulong(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(Name, val));}~%")
(hey "~%")
(hey "#define Xm_type_1(Name, XType) \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {return((XType)Xen_ulong_to_C_ulong(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(Name, val));}~%")
(hey "~%")
					;(hey "#define Xm_type_no_P(Name, XType) \\~%")
					;(hey "  static Xen C_to_Xen_ ## Name (XType val) {return(wrap_for_Xen(Name, val));} \\~%")
					;(hey "  static XType Xen_to_C_ ## Name (Xen val) {return((XType)Xen_unwrap_C_pointer(Xen_cadr(val)));} \\~%")
					;(hey "~%")
(hey "#define Xm_type_no_p_2(Name, XType) \\~%")
(hey "  static Xen C_to_Xen_ ## Name (XType val) {return(wrap_for_Xen(Name, val));}~%")
(hey "~%")
(hey "#define Xm_type_Ptr(Name, XType) \\~%")
(hey "  static Xen C_to_Xen_ ## Name (XType val) {if (val) return(wrap_for_Xen(Name, val)); return(Xen_false);} \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {if (Xen_is_false(val)) return(NULL); return((XType)Xen_unwrap_C_pointer(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(Name, val));}~%")
(hey "~%")
(hey "#define Xm_type_Ptr_const(Name, XType) \\~%")
(hey "  static Xen C_to_Xen_ ## Name (const XType val) {if (val) return(wrap_for_Xen(Name, val)); return(Xen_false);} \\~%")
(hey "  static const XType Xen_to_C_ ## Name (Xen val) {if (Xen_is_false(val)) return(NULL); return((const XType)Xen_unwrap_C_pointer(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(Name, val));}~%")
(hey "~%")
(hey "#define Xm_type_Ptr_1(Name, XType) \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {if (Xen_is_false(val)) return(NULL); return((XType)Xen_unwrap_C_pointer(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(Name, val));}~%")
(hey "~%")
(hey "#define Xm_type_Ptr_2(Name, XType) \\~%")
(hey "  static Xen C_to_Xen_ ## Name (XType val) {if (val) return(wrap_for_Xen(Name, val)); return(Xen_false);} \\~%")
(hey "~%")
(hey "/* type checks for callback wrappers */~%")

(define (callback-p func)
  (hey "#define Xen_is_~A(Arg)  Xen_is_false(Arg) || (Xen_is_procedure(Arg) && (Xen_is_aritable(Arg, ~D)))~%"
       (symbol->string (callback-name func))
       (length (callback-args func))))

(for-each callback-p callbacks)

					;(hey "#define Xen_is_lambda(Arg) Xen_is_procedure(Arg)~%")
(hey "#define Xen_is_GCallback(Arg) (Xen_is_procedure(Arg) && ((Xen_is_aritable(Arg, 2)) || (Xen_is_aritable(Arg, 3)) || (Xen_is_aritable(Arg, 4))))~%")

(define (xen-callback func)
  (hey "#define Xen_to_C_~A(Arg) Xen_is_false(Arg) ? NULL : gxg_~A~%"
       (symbol->string (callback-name func))
       (callback-func func)))

(for-each xen-callback callbacks)

(hey "#define Xen_to_C_GCallback(Arg) ((Xen_is_aritable(Arg, 4)) ? (GCallback)gxg_func4 : ((Xen_is_aritable(Arg, 3)) ? (GCallback)gxg_func3 : (GCallback)gxg_func2))~%")
;;; (hey "#define Xen_to_C_GCallback(Arg) ((Xen_is_aritable(Arg, 3)) ? (GCallback)gxg_func3 : (GCallback)gxg_func2)~%")

(hey "#define Xen_to_C_lambda_data(Arg) (gpointer)gxg_ptr~%")
(hey "#define Xen_is_lambda_data(Arg) 1~%")

;; needed if func returns func of this type
(hey "#define C_to_Xen_GtkTreeViewSearchPositionFunc(Arg) wrap_for_Xen(GtkTreeViewSearchPositionFunc, Arg)~%")
(hey "#define C_to_Xen_GtkTreeViewSearchEqualFunc(Arg) wrap_for_Xen(GtkTreeViewSearchEqualFunc, Arg)~%")
					;(hey "#define C_to_Xen_GtkLinkButtonUriFunc(Arg) wrap_for_Xen(GtkLinkButtonUriFunc, Arg)~%")
					;(hey "#define C_to_Xen_GtkTreeIterCompareFunc(Arg) wrap_for_Xen(GtkTreeViewSearchEqualFunc, Arg)~%")
					;(hey "#define C_to_Xen_GtkTreeSelectionFunc(Arg) wrap_for_Xen(GtkTreeSelectionFunc, Arg)~%")
					;(hey "#define C_to_Xen_GtkMenuPositionFunc(Arg) wrap_for_Xen(GtkMenuPositionFunc, Arg)~%")
					;(hey "#define C_to_Xen_GtkDestroyNotify(Arg) wrap_for_Xen(GtkDestroyNotify, Arg)~%")
(hey "#define Xen_to_C_GdkFilterReturn(Arg) (GdkFilterReturn)Xen_integer_to_C_int(Arg)~%")

;(hey "#define Xen_to_C_String(Arg) Xen_string_to_C_string(Arg)~%")
(hey "#define C_to_Xen_String(Arg) C_string_to_Xen_string((char *)Arg)~%")

(hey "static Xen C_to_Xen_GError_(GError *err)~%")
(hey "{~%")
(hey "  if (err)~%")
(hey "    {~%")
(hey "      Xen msg;~%")
(hey "      msg = C_string_to_Xen_string(err->message);~%")
(hey "      g_error_free(err);~%")
(hey "      return(msg);~%")
(hey "    }~%")
(hey "  return(Xen_false);~%")
(hey "}~%")


(hey "~%~%/* ---------------------------------------- types ---------------------------------------- */~%~%")

(for-each type-it (reverse types))
(for-each
 (lambda (type-list with-func)
   (if (pair? type-list) 
       (with-func hey (lambda () 
			(for-each type-it (reverse type-list))))))
 all-ntypes all-ntype-withs)


(hey "#define XLS(a, b) Xen_to_C_gchar_(Xen_list_ref(a, b))~%")
(hey "#define XLI(a, b) ((int)Xen_integer_to_C_int(Xen_list_ref(a, b)))~%")
(hey "#define XLL(a, b) (Xen_llong_to_C_llong(Xen_list_ref(a, b)))~%")
(hey "#define XLG(a, b) Xen_to_C_GType(Xen_list_ref(a, b))~%")
(hey "#define XLT(a, b) Xen_to_C_GtkTextTag_(Xen_list_ref(a, b))~%")
(hey "#define XLA(a, b) ((Xen_is_integer(Xen_list_ref(a, b))) ? ((gpointer)XLL(a, b)) : ((Xen_is_string(Xen_list_ref(a, b))) ? ((gpointer)XLS(a, b)) : ((gpointer)XLG(a, b))))~%~%")

(hey "/* -------------------------------- gc protection -------------------------------- */~%")
(hey "~%")
(hey "static Xen xm_protected;~%")
(hey "static int xm_protected_size = 0;~%")
(hey "static Xen xm_gc_table;~%")
(hey "static int last_xm_unprotect = NOT_A_GC_LOC;~%")
(hey "~%")
(hey "static int xm_protect(Xen obj)~%")
(hey "{~%")
(hey "  int i, new_size;~%")
(hey "  Xen new_table;~%")
(hey "  if (last_xm_unprotect >= 0)~%")
(hey "    {~%")
(hey "      i = last_xm_unprotect;~%")
(hey "      if (Xen_is_false(Xen_vector_ref(xm_protected, i)))~%")
(hey "	{~%")
(hey "	  Xen_vector_set(xm_protected, i, obj);~%")
(hey "	  last_xm_unprotect = NOT_A_GC_LOC;~%")
(hey "	  return(i);~%")
(hey "	}~%")
(hey "      last_xm_unprotect = NOT_A_GC_LOC;~%")
(hey "    }~%")
(hey "  for (i = 0; i < xm_protected_size; i++)~%")
(hey "    if (Xen_is_false(Xen_vector_ref(xm_protected, i)))~%")
(hey "      {~%")
(hey "	Xen_vector_set(xm_protected, i, obj);~%")
(hey "	return(i);~%")
(hey "      }~%")
(hey "  new_size = xm_protected_size * 2;~%")
(hey "  new_table = Xen_make_vector(new_size, Xen_false);~%")
(hey "  for (i = 0; i < xm_protected_size; i++)~%")
(hey "    {~%")
(hey "      Xen_vector_set(new_table, i, Xen_vector_ref(xm_protected, i));~%")
(hey "      Xen_vector_set(xm_protected, i, Xen_false);~%")
(hey "    }~%")
(hey "  Xen_vector_set(new_table, xm_protected_size, obj);~%")
(hey "  Xen_vector_set(xm_gc_table, 0, new_table);~%")
(hey "  i = xm_protected_size;~%")
(hey "  xm_protected_size = new_size;~%")
(hey "  xm_protected = new_table;~%")
(hey "  return(i);~%")
(hey "}~%")
(hey "~%")
(hey "static void xm_unprotect_at(int ind)~%")
(hey "{~%")
(hey "  Xen_vector_set(xm_protected, ind, Xen_false);~%")
(hey "  last_xm_unprotect = ind;~%")
(hey "}~%~%")


(define c->s7 (make-hash-table))
(define c->s7-checker (make-hash-table))
(define s7->c (make-hash-table))
(define (typer typ)
  (cond ((assoc typ direct-types) 
	 => (lambda (val)
	      (cond ((member (cdr val) '("INT" "ULONG")) 's7_make_integer)
		    ((assoc (cdr val)
			    '(("DOUBLE" . s7_make_real) 
			      ("BOOLEAN" . s7_make_boolean) 
			      ("CHAR" . s7_make_character) 
			      ("String" . s7_make_string)))
		     => cdr)
		    (else 's7_make_c_pointer))))
	((member typ '("gchar_" "char_")) 's7_make_string)
	((equal? typ "time_t")	          's7_make_integer)
	(else 's7_make_c_pointer)))

(define (untyper typ)
  (cond ((assoc typ direct-types) 
	 => (lambda (val)
	      (cond ((member (cdr val) '("INT" "ULONG")) 's7_integer)
		    ((assoc (cdr val)
			    '(("DOUBLE" . s7_real) 
			      ("BOOLEAN" . lg_boolean) 
			      ("CHAR" . s7_character) 
			      ("String" . s7_string)))
		     => cdr)
		    (else 's7_c_pointer))))
	((member typ '("gchar_" "char_")) 's7_string)
	(else 's7_c_pointer)))

(for-each
 (lambda (typ)
   (let ((maker (typer (no-stars typ))))
     (hash-table-set! c->s7 (no-stars typ) maker)
     (hash-table-set! s7->c (no-stars typ) (untyper (no-stars typ)))
     (hash-table-set! c->s7-checker (no-stars typ) 
		      (cond ((assq maker '((s7_make_integer   . s7_is_integer)
					   (s7_make_real      . s7_is_real)
					   (s7_make_boolean   . s7_is_boolean)
					   (s7_make_string    . s7_is_string)
					   (s7_make_c_pointer . s7_is_c_pointer_of_type)
					   (s7_make_character . s7_is_character)))
			     => cdr)
			    (else 's7_is_c_pointer_of_type)))))
 all-types)
;(hay "#define lg_etc(Arg) (Arg)~%")
(hay "#define lg_is_list(Arg) s7_is_list(sc, Arg)~%")
(hash-table-set! c->s7-checker "etc" 'lg_is_list)


(hey "~%~%/* ---------------------------------------- callback handlers ---------------------------------------- */~%~%")

(hay "static s7_scheme *cbsc = NULL;~%")

(let ((xc (let ((funcs-done ()))
	    (lambda (func)
	     (let* ((name (callback-func func))
		    (type (callback-type func))
		    (args (callback-args func))
		    (gcc-permanent? (eq? (callback-gc func) 'permanent-gcc))
		    (fname (callback-name func))
		    (void? (string=? type "void")))
	       (unless (member name funcs-done)
		 (set! funcs-done (cons name funcs-done))
		 (if (callback-version func)
		     (hoy (string-append "#if GTK_CHECK_VERSION(" (substring (callback-version func) 0 1) ", " (substring (callback-version func) 2) ", 0)~%")))
		 
		 (hey "static ~A gxg_~A(" type name)
		 (hay "static ~A lg_~A(" type name)
		 (let ((previous-arg #f)
		       (ctr 0)
		       (ctr1 (memq fname '(GtkAccelMapForeach GtkEntryCompletionMatchFunc)))
		       (ctr2 (memq fname '(GtkTreeViewSearchEqualFunc GLogFunc)))
		       (ctr0 (memq fname '(GtkFileFilterFunc GtkRecentFilterFunc GLogFunc))))
		   (for-each
		    (lambda (arg)
		      (if previous-arg (hoy ", "))
		      ;; ctr is 0-based here
		      (if (or (and ctr1 (= ctr 1))
			      (and ctr2 (= ctr 2))
			      (and ctr0 (= ctr 0)))
			  (hoy "const "))
		      (set! ctr (+ ctr 1))
		      (set! previous-arg #t)
		      (hoy "~A ~A" 
			   (if (string=? (car arg) "lambda_data")
			       "gpointer"
			       (car arg))
			   (cadr arg)))
		    args)
		   (hoy ")~%"))
		 (hoy "{~%  ")
		 ;; I tried to use Xen_error here but it was a no-op for some reason?? 
		 (hey "if (!Xen_is_list((Xen)func_info)) return~A;~%  " (if void? "" (format #f "((~A)0)" (no-stars type))))
		 (hay "if (!s7_is_list(cbsc, (s7_pointer)func_info)) return~A;~%  " (if void? "" (format #f "((~A)0)" (no-stars type))))
		 (if gcc-permanent?
		     (hoy "#if (!(defined(__cplusplus)))~%  ")) ; const arg conversion causes trouble if g++
		 (let ((castlen (+ 12 (if void? 1 (+ 2 (length (format #f "return(Xen_to_C_~A" (no-stars type)))))))
		       (castlen1 19))
		   (unless void?
		     (hey "return(Xen_to_C_~A(" (no-stars type))
		     (hay "return((~A)~A(" type (or (hash-table-ref s7->c (no-stars type)) 's7_c_pointer)))
		   (hey "Xen_call_with_~A_arg~A(~A((Xen)func_info),~%"
			(if (null? args) "no" (length args))
			(if (and (pair? args) (null? (cdr args))) "" "s")
			(case fname 
			  ;((GtkClipboardClearFunc) "Xen_caddr")
			  ((GtkDestroyNotify)      "Xen_cadddr")
			  (else                    "Xen_car")))
		   (hay "s7_apply_function(cbsc, ~%    ~A((s7_pointer)func_info), ~A~%"
			(case fname 
			  ;((GtkClipboardClearFunc) "s7_caddr")
			  ((GtkDestroyNotify)      "s7_cadddr")
			  (else                    "s7_car"))
			(if (null? args) "s7_nil(cbsc" (format #f "~%           s7_list(cbsc, ~D," (length args))))
		   (let ((ctr 1)
			 (argnum (length args)))
		     (for-each
		      (lambda (arg)
			(hey (make-string castlen #\space))
			(hay (make-string castlen1 #\space))
			(if (string=? (car arg) "lambda_data")
			    (begin
			      (hey "Xen_cadr((Xen)func_info),~%")
			      (hay "s7_cadr((s7_pointer)func_info)"))
			    (begin
			      (hey "C_to_Xen_~A(~A~A),~%"
				   (no-stars (car arg))
				   (if (string=? (car arg) "GtkFileFilterInfo*")
				       "(GtkFileFilterInfo *)"
				       "")
				   (cadr arg))
			      (hay "~A(cbsc, ~A~A)"
				   (hash-table-ref c->s7 (no-stars (car arg)))
				   (if (member (car arg) '("GtkFileFilterInfo*" "guint8*"))
				       "(void *)"
				       "")
				   (cadr arg))))
			(hay (if (< ctr argnum) ",~%" ""))
			(set! ctr (+ ctr 1)))
		      args))
		   (hey (make-string castlen #\space))
		   (hey "__func__)")
		   (hey (if void? ";~%" "));~%"))
		   (hay (if void? "));~%" "))));~%")))
		 (if gcc-permanent?
		     (begin
		       (if (not void?)
			   (begin
			     (hoy "  #else~%")
			     (hoy "  return((~A)0);~%" (no-stars type))))
		       (hoy "  #endif~%")))
		 (hoy "}~%")
		 (when (callback-version func)
		   (hoy "#endif~%"))
		 (hoy "~%")
		 ))))))
  (for-each xc callbacks))

  
(hey "~%static gboolean gxg_func3(GtkWidget *w, GdkEventAny *ev, gpointer data)~%")
(hey "{~%")
(hey "  return(Xen_boolean_to_C_bool(Xen_call_with_3_args(Xen_car((Xen)data),~%")
(hey "                                     C_to_Xen_GtkWidget_(w),~%")
(hey "                                     C_to_Xen_GdkEventAny_(ev),~%")
(hey "                                     Xen_cadr((Xen)data),~%")
(hey "                                     __func__)));~%")
(hey "}~%")
(hey "~%static gboolean gxg_func4(GtkPrintOperation *op, GtkPrintContext *context, gint page_nr, gpointer data)~%")
(hey "{~%")
(hey "  return(Xen_boolean_to_C_bool(Xen_call_with_4_args(Xen_car((Xen)data),~%")
(hey "                                     C_to_Xen_GtkPrintOperation_(op),~%")
(hey "                                     C_to_Xen_GtkPrintContext_(context),~%")
(hey "                                     C_int_to_Xen_integer(page_nr),~%")
(hey "                                     Xen_cadr((Xen)data),~%")
(hey "                                     __func__)));~%")
(hey "}~%~%")

(hay "~%static gboolean lg_func3(GtkWidget *w, GdkEventAny *ev, gpointer data)~%")
(hay "{~%")
(hay "  return(s7_apply_function(cbsc, s7_car((s7_pointer)data),~%")
(hay "                             s7_list(cbsc, 3, s7_make_c_pointer_with_type(cbsc, w, GtkWidget__sym, lg_false),~%")
(hay "                                              s7_make_c_pointer_with_type(cbsc, ev, GdkEventAny__sym, lg_false),~%")
(hay "                                              s7_cadr((s7_pointer)data))) != lg_false);~%")
(hay "}~%")
(hay "~%static gboolean lg_func4(GtkPrintOperation *op, GtkPrintContext *context, gint page_nr, gpointer data)~%")
(hay "{~%")
(hay "  return(s7_apply_function(cbsc, s7_car((s7_pointer)data),~%")
(hay "                            s7_list(cbsc, 4, s7_make_c_pointer_with_type(cbsc, op, GtkPrintOperation__sym, lg_false),~%")
(hay "                                             s7_make_c_pointer_with_type(cbsc, context, GtkPrintContext__sym, lg_false),~%")
(hay "                                             s7_make_integer(cbsc, page_nr),~%")
(hay "                                             s7_cadr((s7_pointer)data))) != lg_false);~%")
(hay "}~%~%")

(hoy "#if (!GTK_CHECK_VERSION(3, 90, 0))~%")
(hey "static Xen gxg_gtk_widget_set_events(Xen widget, Xen events)~%")
(hay "static s7_pointer lg_gtk_widget_set_events(s7_scheme *sc, s7_pointer args)~%")
(hoy "{~%")
(hoy "  #define H_gtk_widget_set_events \"void gtk_widget_set_events(GtkWidget* widget, gint events)\"~%")
(hay "  s7_pointer widget, events;~%")
(hay "  widget = s7_car(args);~%")
(hay "  events = s7_cadr(args);~%")
(hey "  Xen_check_type(Xen_is_GtkWidget_(widget), widget, 1, \"gtk_widget_set_events\", \"GtkWidget*\");~%")
(hey "  Xen_check_type(Xen_is_gint(events), events, 2, \"gtk_widget_set_events\", \"gint\");~%")
(hey "  gtk_widget_set_events(Xen_to_C_GtkWidget_(widget), Xen_to_C_gint(events));~%")
(hay "  gtk_widget_set_events((GtkWidget*)s7_c_pointer(widget), (gint)s7_integer(events));~%")
(hey "  return(Xen_false);~%")
(hay "  return(lg_false);~%")
(hoy "}~%")
(hoy "#endif~%~%")



(hey "~%~%/* ---------------------------------------- functions ---------------------------------------- */~%~%")

(define max-args 8)

(define (handle-func data)
  (let ((name (car data))
	(args (caddr data))
	(return-type (cadr data)))
    (let ((cargs (length args))
	  (refargs (ref-args args))
	  (lambda-type (hash-table-ref names name)))
      (let ((callback-data (and (not (eq? lambda-type 'fnc))
				(find-callback 
				 (lambda (func)
				   (and (eq? (callback-name func) lambda-type)
					func)))))
	    (spec (and (> (length data) 4) (data 4)))
	    (spec-data (and (> (length data) 5) (data 5))) ; also callback-version
	    (return-type-void (string=? return-type "void"))
	    (xgargs (- cargs refargs))
	    (argstr (cadddr data))
	    (arg-start 0)
	    (line-len 0))
	
	(define (hey-start)  ; start of checked line
	  (set! line-len 0))
	
	(define (hey-mark)   ; start of checked line
	  (set! arg-start line-len))
	
	(define (hey-on . args)	  ; no cr -- just append
	  (let ((line (apply format #f args)))
	    (set! line-len (+ line-len (length line)))
	    (heyc line)))
	
	(define (hey-ok arg) ; cr ok after arg
	  (set! line-len (+ line-len (length arg)))
	  (heyc arg)
	  (when (> line-len 120) ; line-max originally
	    (format xg-file "~%~NC" arg-start #\space)
	    (set! line-len arg-start)))
	
	(hey "static Xen gxg_~A(" name)
	(if (null? args)
	    (heyc "void")
	    (if (>= cargs max-args)
		(heyc "Xen arglist")
		(let ((previous-arg #f))
		  (for-each 
		   (lambda (arg)
		     (let ((argname (cadr arg)))
		       (if previous-arg (heyc ", "))
		       (set! previous-arg #t)
		       (hey (if (and (ref-arg? arg)
				     (not (member name '("gdk_init" "gdk_init_check" "gtk_init" "gtk_init_check" "gtk_parse_args"))))
				"Xen ignore_~A"
				"Xen ~A")
			    argname)))
		   args))))
	(hey ")~%{~%")
	
	(hay "static s7_pointer lg_~A(s7_scheme *sc, s7_pointer args)~%" name)
	(hay "{~%")

	(helpify name return-type argstr)
	(when (member name '("gdk_init" "gdk_init_check" "gtk_init" "gtk_init_check" "gtk_parse_args"))
	  (hay "  s7_pointer argc, argv;~%"))

	(when (pair? args)
	  (let ((previous-arg #f)
		(any-args #f))
	    (when (and (> cargs 1)
		       (not (member name '("gdk_init" "gdk_init_check"))))
	      (hay "  s7_pointer _p;~%"))
	    (for-each 
	     (lambda (arg)
	       (unless (ref-arg? arg)
		 (unless any-args
		   (hay "  s7_pointer ")
		   (set! any-args #t))
		 (if previous-arg (hay ", "))
		 (hay "~A" (cadr arg))
		 (set! previous-arg #t)))
	     args)
	    (if any-args (hay ";~%"))))

	(when (member name '("gdk_init" "gdk_init_check" "gtk_init" "gtk_init_check" "gtk_parse_args"))
	  (hay "  argc = s7_car(args);~%  argv = s7_cadr(args);~%"))
	;; ----------------

	(if (> refargs 0)
	    (for-each
	     (lambda (arg)
	       (if (ref-arg? arg)
		   (hoy (if (has-stars (deref-type arg))
			    "  ~A ~A = NULL;~%"
			    "  ~A ~A;~%")
			(deref-type arg)
			(deref-name arg))))
	     args))
	(if (and (>= cargs max-args)
		 (> xgargs 0))
	    (begin
	      (heyc "  Xen ")
	      (for-each
	       (let ((previous-arg #f))
		 (lambda (arg)
		   (unless (ref-arg? arg) ;(< (length arg) 3)
		     (if previous-arg (heyc ", "))
		     (set! previous-arg #t)
		     (hey "~A" (cadr arg)))))
	       args)
	      (hey ";~%")
	      (let ((ctr 0)) ; list-ref counts from 0
		(for-each
		 (lambda (arg)
		   (if (not (ref-arg? arg))
		       (hey "  ~A = Xen_list_ref(arglist, ~D);~%" (cadr arg) ctr))
		   (set! ctr (+ ctr 1)))
		 args))))

	(when (pair? args)
	  (let ((ctr 1)
		(argc #f))
	    (when (and (> cargs 1)
		       (not (member name '("gdk_init" "gdk_init_check"))))
	      (hay "  _p = args;~%"))
	    (for-each
	     (lambda (arg)
	       (let ((argname (cadr arg))
		     (argtype (car arg)))
		 (if (ref-arg? arg)
		     (if (>= (length arg) 3)
			 (if (char=? ((arg 2) 0) #\{)
			     (begin
			       (set! argc (deref-name arg))
			       (hey "  ~A = Xen_to_C_~A(~A);~%" (deref-name arg) (deref-type arg) argname)
			       (hay "  ~A = (~A)s7_integer(~A);~%" (deref-name arg) (deref-type arg) argname))

			     (when (char=? ((arg 2) 0) #\|)
			       (hoy "  ~A = (~A)calloc(~A, sizeof(~A));~%" 
				    (deref-name arg)
				    (deref-type arg)
				    argc
				    (deref-element-type arg))
			       (hey "  {~%   int i;~%   Xen lst;~%   lst = Xen_copy_arg(~A);~%" argname)
			       (hey "   for (i = 0; i < ~A; i++, lst = Xen_cdr(lst)) ~A[i] = Xen_to_C_~A(Xen_car(lst));~%"
				    argc
				    (deref-name arg)
				    (no-stars (deref-element-type arg)))
			       (hey "  }~%")

			       (hay "  {~%    int i;~%    s7_pointer lst;~%    lst = ~A;~%" argname)
			       (hay "    for (i = 0; i < ~A; i++, lst = s7_cdr(lst)) ~A[i] = (~A)s7_string(s7_car(lst));~%"
				    argc
				    (deref-name arg)
				    (deref-element-type arg))
			       (hay "  }~%"))))
		     (let ((checker (or (hash-table-ref c->s7-checker (no-stars argtype)) 's7_is_c_pointer_of_type)))
		       (hay (if (> cargs 1)
				"  ~A = s7_car(_p); _p = s7_cdr(_p);~%"
				"  ~A = s7_car(args);~%")
			    argname)
		       (if (null-arg? arg)
			   (begin
			     (hey "  Xen_check_type(Xen_is_~A(~A) || Xen_is_false(~A), ~A, ~D, ~S, ~S);~%" 
				  (no-stars argtype) argname argname argname ctr name argtype)

			     (if (eq? checker 's7_is_c_pointer_of_type)
				 (hay "  if ((!~A(~A, ~A_sym)) && (~A != lg_false)) s7_wrong_type_arg_error(sc, ~S, ~D, ~A, ~S);~%" 
				      checker
				      argname (no-stars argtype) argname
				      name ctr argname argtype)
				 (hay "  if ((!~A(~A)) && (~A != lg_false)) s7_wrong_type_arg_error(sc, ~S, ~D, ~A, ~S);~%" 
				      checker
				      argname argname
				      name ctr argname argtype)))
			   
			   (if (opt-arg? arg)
			       (begin
				 (hey "  if (!Xen_is_bound(~A)) ~A = Xen_false; ~%" argname argname)
				 (hey "  else Xen_check_type(Xen_is_~A(~A), ~A, ~D, ~S, ~S);~%" 
				      (no-stars argtype) argname argname ctr name argtype)

				 (if (eq? checker 's7_is_c_pointer_of_type)
				     (hay "  if ((!~A(~A, ~A_sym)) && (~A != lg_false)) s7_wrong_type_arg_error(sc, ~S, ~D, ~A, ~S);~%" 
					  checker
					  argname (no-stars argtype) argname
					  name ctr argname argtype)
				     (hay "  if ((!~A(~A)) && (~A != lg_false)) s7_wrong_type_arg_error(sc, ~S, ~D, ~A, ~S);~%" 
					  checker
					  argname argname
					  name ctr argname argtype)))
			       
			       (begin
				 (hey "  Xen_check_type(Xen_is_~A(~A), ~A, ~D, ~S, ~S);~%"
				      (no-stars argtype) argname argname ctr name argtype)

				 (if (eq? checker 's7_is_c_pointer_of_type)
				     (hay "  if (!~A(~A, ~A_sym)) s7_wrong_type_arg_error(sc, ~S, ~D, ~A, ~S);~%" 
					  checker
					  argname (no-stars argtype)
					  name ctr argname argtype)
				     (hay "  if (!~A(~A)) s7_wrong_type_arg_error(sc, ~S, ~D, ~A, ~S);~%" 
					  checker
					  argname
					  name ctr argname argtype)))))))
		 (set! ctr (+ ctr 1))))
	     args)))

	(let ((using-result #f))
	  (if (eq? lambda-type 'fnc)
	      (begin 
		(set! using-result (and (> refargs 0)
					(not return-type-void)))
		(when using-result
		  (hoy "  {~%")
		  (hey "    Xen result;~%")
		  (hay "    s7_pointer result;~%"))
		(hey-start)
		
		(cond ((eq? spec 'etc))
		      
		      (return-type-void 
		       (hey-on "  ")
		       (hay "  "))
		      
		      ((not (= refargs 0))
		       (hey-on "    result = C_to_Xen_~A(" (no-stars return-type))
		       (let ((call (hash-table-ref c->s7 (no-stars return-type))))
			 (if (eq? call 's7_make_c_pointer)
			     (hay "    result = s7_make_type_with_c_pointer(sc, ~A_sym, " (no-stars return-type))
			     (hay "    result = ~A(sc, " call))))
		      
		      (else 
		       (case spec
			 ((free) 
			  (hey-on "  {~%   ~A result;~%   Xen rtn;~%   result = " return-type)
			  (hay "  {~%    ~A result;~%    s7_pointer rtn;~%    result = " return-type))

			 ((const-return) 
			  (hey "    return(C_to_Xen_~A((~A)" (no-stars return-type) return-type)
			  (let ((call (hash-table-ref c->s7 (no-stars return-type))))
			    (if (eq? call 's7_make_c_pointer)
				(hay "    return(s7_make_type_with_c_pointer(sc, ~A_sym, (~A)" (no-stars return-type) return-type)
				(hay "    return(~A(sc, (~A)" call return-type))))

			 (else
			  (when (member name idlers)
			    (hey "  xm_unprotect_at(Xen_integer_to_C_int(Xen_caddr(~A)));~%" (cadar args))
			    (set! idlers (remove-if (lambda (x) (string=? x name)) idlers)))
			  (hey-on "  return(C_to_Xen_~A(" (no-stars return-type))
			  (let ((call (hash-table-ref c->s7 (no-stars return-type))))
			    (if (eq? call 's7_make_c_pointer)
				(hay "  return(s7_make_type_with_c_pointer(sc, ~A_sym, " (no-stars return-type))
				(hay "  return(~A(sc, " call))))))))
	      
	      (let ((using-loc (or (eq? lambda-type 'GCallback)
				   (and callback-data
					(memq (callback-gc callback-data) '(temporary semi-permanent))))))
		;; lambda-type != 'fnc
		(set! using-result (not (or return-type-void
					    (eq? lambda-type 'lambda))))
		(hoy "  {~%")
		(when using-result 
		  (hey "    Xen result;~%")
		  (hay "    s7_pointer result;~%"))

		(if using-loc (hoy "    int loc;~%"))

		(hey "    Xen gxg_ptr = Xen_list_5(~A, func_info, Xen_false, Xen_false, Xen_false);~%"
		     (call-with-exit
		      (lambda (name-it)
			(for-each
			 (lambda (arg)
			   (let ((argname (cadr arg)))
			     (if (string=? argname "func")
				 (name-it "func"))))
			 args)
			"Xen_false")))
		(hay "    s7_pointer lg_ptr = s7_list(sc, 5, ~A, func_info, lg_false, lg_false, lg_false);~%"
		     (call-with-exit
		      (lambda (name-it)
			(for-each
			 (lambda (arg)
			   (let ((argname (cadr arg)))
			     (if (string=? argname "func")
				 (name-it "func"))))
			 args)
			"lg_false")))

		(if using-loc
		    (begin
		      (hey "    loc = xm_protect(gxg_ptr);~%")
		      (hay "    loc = s7_gc_protect(sc, lg_ptr);~%")
		      (hey "    Xen_list_set(gxg_ptr, 2, C_int_to_Xen_integer(loc));~%")
		      (hay "    s7_list_set(sc, lg_ptr, 2, s7_make_integer(sc, loc));~%"))
		    (begin
		      (hey "    xm_protect(gxg_ptr);~%")
		      (hay "    s7_gc_protect(sc, lg_ptr);~%")
#|
		      (when (eq? lambda-type 'GtkClipboardGetFunc)
			(hey "    Xen_list_set(gxg_ptr, 2, clear_func);~%")
			(hay "    s7_list_set(sc, lg_ptr, 2, clear_func);~%"))
|#
		      ))

		(for-each
		 (lambda (arg)
		   (let ((argname (cadr arg))
			 (argtype (car arg)))
		     (when (string=? argtype "GtkDestroyNotify")
		       (hey "    Xen_list_set(gxg_ptr, 3, ~A);~%" argname)
		       (hay "    s7_list_set(sc, lg_ptr, 3, ~A);~%" argname))))
		 args)
		(hey-start)
		(if using-result
		    (begin
		      (hey-on "    result = C_to_Xen_~A(" (no-stars return-type))
		      (hay "    result = ~A(sc, " (hash-table-ref c->s7 (no-stars return-type))))
		    (begin
		      (heyc "    ")
		      (hay "    ")))))
	  
	  ;; pass args
	  (if (eq? spec 'etc)
	      ;; need to check ... list, set up locals, send out switch, return result
	      (let ((list-name (cadr (args (- cargs 1))))
		    (types (caddr spec-data))
		    (with-minus-one (member name '("gtk_list_store_set" "gtk_tree_store_set") string=?)))
		(let ((min-len (car spec-data))
		      (max-len (cadr spec-data))
		      (with-null (not (or with-minus-one
					  (and (pair? types)
					       (null? (cdr types))
					       (string=? (car types) "GType")))))
		      (modlen (length types)))
		  (hoy "  {~%")
		  (hoy "    int etc_len = 0;~%")
		  (if (not return-type-void)
		      (hoy "    ~A result = ~A;~%" return-type (if (has-stars return-type) "NULL" "0")))
		  (do ((i 0 (+ i 1)))
		      ((= i (- cargs 1)))
		    (hoy "    ~A p_arg~D;~%" (car (args i)) i))
		  (hey "    if (Xen_is_list(~A)) etc_len = Xen_list_length(~A);~%" list-name list-name)
		  (hay "    if (s7_is_list(sc, ~A)) etc_len = s7_list_length(sc, ~A);~%" list-name list-name)
		  (when (> min-len 0)
		    (hey "    if (etc_len < ~D) Xen_out_of_range_error(~S, ~A, ~A, \"... list must have at least ~D entr~A\");~%"
			 min-len name (- cargs 1) list-name min-len (if (= min-len 1) "y" "ies"))
		    (hay "    if (etc_len < ~D) s7_out_of_range_error(sc, ~S, ~A, ~A, \"... list must have at least ~D entr~A\");~%"
			 min-len name (- cargs 1) list-name min-len (if (= min-len 1) "y" "ies")))

		  (hey "    if (etc_len > ~D) Xen_out_of_range_error(~S, ~A, ~A, \"... list too long (max len: ~D)\");~%"
		       max-len name (- cargs 1) list-name max-len)
		  (hay "    if (etc_len > ~D) s7_out_of_range_error(sc, ~S, ~A, ~A, \"... list too long (max len: ~D)\");~%"
		       max-len name (- cargs 1) list-name max-len)

		  (unless (= modlen 1)
		    (hey "    if ((etc_len % ~D) != 0) Xen_out_of_range_error(~S, ~A, ~A, \"... list len must be multiple of ~D\");~%"
			 modlen name (- cargs 1) list-name modlen)
		    (hay "    if ((etc_len % ~D) != 0) s7_out_of_range_error(sc, ~S, ~A, ~A, \"... list len must be multiple of ~D\");~%"
			 modlen name (- cargs 1) list-name modlen))

		  (do ((i 0 (+ i 1)))
		      ((= i (- cargs 1)))
		    (let ((arg (args i)))
		      (hey "    p_arg~D = Xen_to_C_~A(~A);~%" i (no-stars (car arg)) (cadr arg))
		      (hay "    p_arg~D = (~A)~A(~A);~%" i (car arg) (hash-table-ref s7->c (no-stars (car arg))) (cadr arg))))

		  (hoy "    switch (etc_len)~%")
		  (hoy "      {~%")
		  (do ((name-is-file-chooser (string=? name "gtk_file_chooser_dialog_new"))
		       (i min-len (+ i modlen)))
		      ((> i max-len))
		    (hoy (if (not return-type-void)
			     "        case ~D: result = ~A("
			     "        case ~D: ~A(")
			 i name)
		    (do ((j 0 (+ 1 j)))
			((= j (- cargs 1)))
		      (hoy "p_arg~D, " j))
		    ;; assume ending null for now
		    (do ((modctr 0)
			 (j 0 (+ 1 j)))
			((= j i))
		      (let ((type (types modctr)))
			(set! modctr (+ 1 modctr))
			(if (>= modctr modlen) (set! modctr 0))
			(hey (cond ((assoc type '(("int" . "XLI(") 
						  ("gchar*" . "XLS(") 
						  ("GtkTextTag*" . "XLT(") 
						  ("GType" . "XLG("))
					   string=?) => cdr)
				   (else "XLA(")))
			(hay (cond ((assoc type '(("int" . "s7_integer(s7_list_ref(sc, ") 
						  ("gchar*" . "s7_string(s7_list_ref(sc, "))
					   string=?) => cdr)
				   (else "s7_c_pointer(s7_list_ref(sc, "))))

		      (hoy "~A, ~D)" list-name j)
		      (hay ")")
		      (if (or with-null with-minus-one (< j (- i 1)))
			  (hoy ", ")))
		    (hoy (if with-null
			     (if (and (= i 0) name-is-file-chooser)
				 "NULL, NULL); break;~%" ; extra NULL needed I guess for the valist pass-through -- gcc 4.1 grumbles about it
				 "NULL); break;~%")
			     (if with-minus-one 
				 "-1); break;~%" 
				 "); break;~%"))))
		  (hoy "      }~%")
		  
		  (if return-type-void
		      (begin
			(hey "    return(Xen_false);~%")
			(hay "    return(lg_false);~%"))
		      (begin
			(hey "    return(C_to_Xen_~A(result));~%" (no-stars return-type))
			(hay "    return(~A(sc, result));~%" (hash-table-ref c->s7 (no-stars return-type)))))
		  (hoy "  }~%")))
	      
	      ;; not eq? spec 'etc
	      (if (eq? lambda-type 'lambda)
		  (begin ; 'lambda (see line 1846)
		    (hey "if (Xen_is_aritable(func, 2))~%")
		    (hay "if (s7_is_aritable(sc, func, 2))~%")
		    (hey-start)
		    (if return-type-void
			(begin
			  (hey-on "       ~A(" name)
			  (hay "       ~A(" name))
			(begin
			  (hey-on "       return(C_to_Xen_~A(~A(" (no-stars return-type) name)
			  (let ((call (hash-table-ref c->s7 (no-stars return-type))))
			    (if (eq? call 's7_make_c_pointer)
				(hay "       return(s7_make_type_with_c_pointer(sc, ~A_sym, ~A(" (no-stars return_type) name)
				(hay "       return(~A(sc, ~A(" call name)))))
		    (hey-mark)
		    (let ((previous-arg #f))
		      (for-each
		       (lambda (arg)
			 (let ((argname (cadr arg))
			       (argtype (car arg)))
			   (when previous-arg 
			     (hey-ok ", ")
			     (hay ", "))
			   (set! previous-arg #t)
			   (hey-on "Xen_to_C_~A(~A)" (no-stars argtype) argname)
			   (if (equal? argtype "char*")
			       (hay "(char*)~A(~A)" (hash-table-ref s7->c (no-stars argtype)) argname)
			       (if (equal? argtype "lambda_data")
				   (hay "(gpointer)lg_ptr")
				   (hay "~A(~A)" (hash-table-ref s7->c (no-stars argtype)) argname)))))
		       args))
		    (hoy (if return-type-void
			     ");~%"
			     ")));~%"))
		    (hoy "     else~%")
		    (hey-start)
		    (if return-type-void
			(begin
			  (hey-on "       ~A(" name)
			  (hay "       ~A(" name))
			(begin
			  (hey-on "       return(C_to_Xen_~A(~A(" (no-stars return-type) name)
			  (let ((call (hash-table-ref c->s7 (no-stars return-type))))
			    (if (eq? call 's7_make_c_pointer)
				(hay "       return(s7_make_type_with_c_pointer(sc, ~A_sym, ~A(" (no-stars return_type) name)
				(hay "       return(~A(sc, ~A(" call name)))))
		    (hey-mark)
		    (let ((previous-arg #f))
		      (for-each
		       (lambda (arg)
			 (let ((argname (cadr arg))
			       (argtype (car arg)))
			   (when previous-arg 
			     (hey-ok ", ")
			     (hay ", "))
			   (set! previous-arg #t)
			   (hey-on "Xen_to_C_~A(~A)" (no-stars argtype) argname)
			   (if (equal? argtype "char*")
			       (hay "(char*)~A(~A)" (hash-table-ref s7->c (no-stars argtype)) argname)
			       (if (equal? argtype "lambda_data")
				   (hay "(gpointer)lg_ptr")
				   (hay "~A(~A)" (hash-table-ref s7->c (no-stars argtype)) argname)))))
		       args))
		    (if return-type-void
			(begin
			  (hoy ");~%")
			  (hey "    return(Xen_false);~%")
			  (hay "    return(lg_false);~%"))
			(hoy ")));~%"))
		    (hoy "  }~%")) ;'lambda
		  
		  (begin
		    (hey-on "~A(" name)
		    (hay (if (member name '("gtk_selection_data_get_data_with_length" "gtk_selection_data_get_data"))
			     "(void *)~A("
			     "~A(")
			 name)
		    (hey-mark)
		    (if (pair? args)
			(let ((previous-arg #f))
			  (for-each
			   (lambda (arg)
			     (let ((argname (cadr arg))
				   (argtype (car arg)))
			       (when previous-arg 
				 (hey-ok ", ")
				 (hay ", "))
			       (if (and (eq? spec 'const)
					(member argtype '("char**" "gchar**" "gchar*" "char*" "GValue*") string=?))
				   (hoy "(const ~A)" argtype))
			       (set! previous-arg #t)
			       (if (ref-arg? arg)
				   (begin
				     (hey-on "&~A" (deref-name arg))
				     (hay "&~A" (deref-name arg)))
				   (begin
				     (hey-on "Xen_to_C_~A(~A)" (no-stars argtype) argname)
				     (if (equal? argtype "char*")
					 (hay "(char*)~A(~A)" (hash-table-ref s7->c (no-stars argtype)) argname)
					 (if (equal? argtype "lambda_data")
					     (hay "(gpointer)lg_ptr")
					     (hay "~A(~A)" (hash-table-ref s7->c (no-stars argtype)) argname)))))))
			   args)))
		    (if (not return-type-void)
			(if (not (and (eq? lambda-type 'fnc)
				      (= refargs 0)))
			    (begin
			      (heyc ")")
			      (hay ")"))
			    (if (not (eq? spec 'free))
				(begin
				  (heyc "))")
				  (hay "))")))))
		    (hoy ");~%")
		    (if (not (eq? lambda-type 'fnc))
			(begin
			  (if (and callback-data
				   (eq? (callback-gc callback-data) 'temporary))
			      (hey "    xm_unprotect_at(loc);~%"))
			  (if (and callback-data
				   (eq? (callback-gc callback-data) 'semi-permanent))
			      (hey "    Xen_list_set(gxg_ptr, 2, Xen_list_3(xg_idler_symbol, ~A, C_int_to_Xen_integer(loc)));~%"
				   (if return-type-void "Xen_false" "result")))
			  (if using-result
			      (hoy "    return(result);~%")
			      (begin
				(hey "    return(Xen_false);~%")
				(hay "    return(lg_false);~%")))
			  (hoy "   }~%"))
			
			(if (> refargs 0)
			    (let ((previous-arg using-result))
			      (when using-result 
				(heyc "  ")
				(hay "  "))
			      (if (string=? name "gdk_property_get")   ; special case -- type returned is dependent to some extent on atom
				  (begin
				    
				    (hey "  {~%      Xen data_val = Xen_false;~%\
      if (ref_actual_property_type == GDK_TARGET_STRING)~%\
	data_val = C_string_to_Xen_string((char *)ref_data);~%\
      else if (ref_actual_length > 0) data_val = C_string_to_Xen_string_with_length((char *)ref_data, ref_actual_length * ref_actual_format / 8);~%\
     return(Xen_list_5(result, C_to_Xen_GdkAtom(ref_actual_property_type), C_to_Xen_gint(ref_actual_format), ~%\
                       C_to_Xen_gint(ref_actual_length), data_val));~%\
    }~%  }~%")
				    (hay "  {~%      s7_pointer data_val = lg_false;~%\
      if (ref_actual_property_type == GDK_TARGET_STRING)~%\
	data_val = s7_make_string(sc, (char *)ref_data);~%\
      else if (ref_actual_length > 0) data_val = s7_make_string_with_length(sc, (char *)ref_data, ref_actual_length * ref_actual_format / 8);~%\
     return(s7_list(sc, 5, result, s7_make_c_pointer(sc, ref_actual_property_type), s7_make_integer(sc, ref_actual_format), ~%\
                       s7_make_integer(sc, ref_actual_length), data_val));~%\
    }~%  }~%"))

				  (begin
				    (hey "  return(Xen_list_~D(" (if using-result (+ refargs 1) refargs))
				    (hay "    return(s7_list(sc, ~D, " (if using-result (+ refargs 1) refargs))
				    (when using-result 
				      (heyc "result")
				      (hay "result"))
				    (for-each 
				     (lambda (arg)
				       (when (ref-arg? arg)
					 (when previous-arg 
					   (heyc ", ")
					   (hay ", "))
					 (hey "C_to_Xen_~A(~A)" (no-stars (deref-type arg)) (deref-name arg))
					 (let ((call (hash-table-ref c->s7 (no-stars (deref-type arg)))))
					   (hay "~A(sc, ~A)" call (deref-name arg)))
					 (set! previous-arg #t)))
				     args)
				    (hoy "));~%")
				    (when using-result
				      (hoy "   }~%")))))
			    ;; refargs = 0
			    (begin
			      (if (member name idlers)
				  (hey "  xm_unprotect_at(Xen_integer_to_C_int(Xen_caddr(~A)));~%" (cadar args)))
			      (if return-type-void
				  (begin
				    (hey "  return(Xen_false);~%")
				    (hay "  return(lg_false);~%")))))))))
	  (when (eq? spec 'free)
	    (hey "   rtn = C_to_Xen_~A(result);~%   g_free(result);~%   return(rtn);~%  }~%" (no-stars return-type))
	    (hay "    rtn = ~A(sc, result);~%    g_free(result);~%    return(rtn);~%  }~%" (hash-table-ref c->s7 (no-stars return-type))))
	  (hoy "}~%~%"))))))
  

(for-each handle-func (reverse funcs))
(for-each
 (lambda (func-list with-func)
   (if (pair? func-list) 
       (with-func hoy (lambda () 
			(for-each handle-func (reverse func-list))))))
 all-funcs all-func-withs)

(hay "static s7_pointer lg_g_signal_connect(s7_scheme *sc, s7_pointer args)~%")
(hay "{~%")
(hay "  #define H_g_signal_connect \"gulong g_signal_connect(gpointer instance, gchar* signal_name, GCallback func, lambda_data func_info)\"~%")
(hay "  s7_pointer obj, name, func, data;~%")
(hay "  GCallback callf;~%")
(hay "  obj = s7_car(args);~%")
(hay "  name = s7_cadr(args);~%")
(hay "  if (!s7_is_string(name)) s7_wrong_type_arg_error(sc, \"g_signal_connect\", 2, name, \"gchar*\");~%")
(hay "  func = s7_caddr(args);~%")
(hay "  if (s7_is_aritable(sc, func, 4)) callf = (GCallback)lg_func4;~%")
(hay "  else {if (s7_is_aritable(sc, func, 3)) callf = (GCallback)lg_func3; else callf = (GCallback)lg_func2;}~%")
(hay "  if (s7_is_pair(s7_cdddr(args))) data = s7_cadddr(args); else data = s7_nil(sc);~%")
(hay "  if (data == lg_false) data = s7_nil(sc);~%")
(hay "  return(s7_make_integer(sc, ~%")
(hay "            g_signal_connect_data((gpointer)s7_c_pointer(obj),~%")
(hay "                                  (gchar *)s7_string(name),~%")
(hay "                                  callf,~%")
(hay "                                  (gpointer)s7_cons(sc, func, data), ~%")
(hay "                                  NULL, (GConnectFlags)0)));~%")
(hay "}~%")



(hey "#define Xen_is_wrapped_object(Obj) (Xen_is_list(Obj) && (Xen_list_length(Obj) >= 2) && (Xen_is_symbol(Xen_car(Obj))))~%~%")

(define (cast-it cast)
  (let ((cast-name (car cast))
	(cast-type (cadr cast)))
    (hey "static Xen gxg_~A(Xen obj)" (no-arg cast-name))
    (hey " {return((Xen_is_wrapped_object(obj)) ? Xen_list_2(xg_~A_symbol, Xen_cadr(obj)) : Xen_false);}~%" (no-stars cast-type))
    (hay "static s7_pointer lg_~A(s7_scheme *sc, s7_pointer args) {return(s7_make_c_pointer_with_type(sc, s7_c_pointer(s7_car(args)), ~A_sym, lg_false));}~%"
	 (no-arg cast-name) (no-stars cast-type))))

(hey "static Xen gxg_GPOINTER(Xen obj)")
(hey " {return(Xen_list_2(xg_gpointer_symbol, (Xen_is_wrapped_object(obj)) ? Xen_cadr(obj) : Xen_wrap_C_pointer(obj)));}~%")

(hay "static s7_pointer lg_GPOINTER(s7_scheme *sc, s7_pointer args) {return(s7_make_c_pointer_with_type(sc, s7_c_pointer(s7_car(args)), gpointer_sym, lg_false));}~%")

(for-each cast-it (reverse casts))
(for-each
 (lambda (cast-list cast-func)
   (if (pair? cast-list) 
       (cast-func hoy
		  (lambda () 
		    (for-each cast-it (reverse cast-list))))))
 all-casts all-cast-withs)


;;; checks have to use the built-in macros, not local symbol-based type checks

(define (make-check func)
  (hey "static Xen gxg_~A(Xen obj)" (no-arg (car func)))
  (hey " {return(C_bool_to_Xen_boolean(Xen_is_wrapped_object(obj) && ~A((GTypeInstance *)Xen_unwrap_C_pointer(Xen_cadr(obj)))));}~%" (no-arg (car func)))
  (hay "static s7_pointer lg_~A(s7_scheme *sc, s7_pointer args)~%" (no-arg (car func)))
  (hay "{~%")
  (hay "  return(((s7_is_c_pointer(s7_car(args))) && (~A((GTypeInstance *)s7_c_pointer(s7_car(args))))) ? lg_true : lg_false);~%" (no-arg (car func)))
  (hay "}~%"))

(for-each make-check (reverse checks))
(for-each
 (lambda (check-list check-func)
   (if (pair? check-list) 
       (check-func hoy (lambda () 
			 (for-each make-check (reverse check-list))))))
 all-checks all-check-withs)


(hey "~%~%/* ---------------------------------------- special functions ---------------------------------------- */~%~%")

(hey "#if GTK_CHECK_VERSION(3, 0, 0)~%")
(hey "#if GTK_CHECK_VERSION(3, 22, 0)~%")
(hey "  static GdkWindow *last_window = NULL;~%")
(hey "  static GdkDrawingContext *last_context = NULL;~%")
(hey "#endif~%")
(hey "~%")
(hey "static cairo_t *make_cairo_1(GdkWindow *win)~%")
(hey "{~%")
(hey "#if GTK_CHECK_VERSION(3, 22, 0)~%")
(hey "  last_window = win;~%")
(hey "  /* last_context = gdk_window_begin_draw_frame(win, gdk_window_get_visible_region(win)); */~%")
(hey "  return(gdk_drawing_context_get_cairo_context(last_context));~%")
(hey "#else~%")
(hey "  return(gdk_cairo_create(win));~%")
(hey "#endif~%")
(hey "}~%")
(hey "~%")
(hey "static void free_cairo_1(cairo_t *cr)~%")
(hey "{~%")
(hey "#if GTK_CHECK_VERSION(3, 22, 0)~%")
(hey "  gdk_window_end_draw_frame(last_window, last_context);~%")
(hey "#else~%")
(hey "  cairo_destroy(cr);~%")
(hey "#endif~%")
(hey "}~%")
(hey "~%")
(hey "static Xen gxg_make_cairo(Xen window)~%")
(hey "{~%")
(hey "  #define H_make_cairo \"cairo_t* make_cairo(GdkWindow* window)\"~%")
(hey "  Xen_check_type(Xen_is_GdkWindow_(window), window, 1, \"make_cairo\", \"GdkWindow*\");~%")
(hey "  return(C_to_Xen_cairo_t_(make_cairo_1(Xen_to_C_GdkWindow_(window))));~%")
(hey "}~%")
(hey "~%")
(hey "static Xen gxg_free_cairo(Xen cr)~%")
(hey "{~%")
(hey "  #define H_free_cairo \"void free_cairo(cairo_t* cr)\"~%")
(hey "  Xen_check_type(Xen_is_cairo_t_(cr), cr, 1, \"free_cairo\", \"cairo_t*\");~%")
(hey "  free_cairo_1(Xen_to_C_cairo_t_(cr));~%")
(hey "  return(Xen_false);~%")
(hey "}~%")
(hey "#endif~%")
(hey "~%")

(hoy "#if (!GTK_CHECK_VERSION(3, 90, 0))~%")
;;; from Mike Scholz -- improve the error checking
(hey "static Xen gxg_gtk_init(Xen argc, Xen argv) ~%")
(hay "static s7_pointer lg_gtk_init(s7_scheme *sc, s7_pointer args)~%")
(hoy "{ ~%")
(hoy "  #define H_gtk_init \"void gtk_init(int* argc, char*** argv)\" ~%")
(hoy "  int ref_argc = 0; ~%")
(hoy "  char** ref_argv = NULL; ~%")
(hay "  s7_pointer argc = NULL, argv = NULL; ~%")
(hay "  if (s7_is_pair(args)) {argc = s7_car(args); ref_argc = s7_integer(argc); if (s7_is_pair(s7_cdr(args))) argv = s7_cadr(args);}~%")
(hey "  if (Xen_is_bound(argv)) ~%")
(hey "    { ~%")
(hey "      if (Xen_is_bound(argc) && Xen_is_integer(argc) && Xen_to_C_int(argc) <= Xen_list_length(argv)) ~%")
(hey "         ref_argc = Xen_to_C_int(argc); ~%")
(hey "      else ref_argc = Xen_list_length(argv); ~%")
(hey "    } ~%")
(hay "  if ((argv) && (ref_argc > 0) && (s7_is_pair(argv)) && (ref_argc <= s7_list_length(sc, argv))) {~%")
(hoy "  ref_argv = (char**)calloc(ref_argc, sizeof(char*)); ~%")
(hoy "  { ~%")
(hoy "    int i; ~%")
(hey "    Xen lst; ~%")
(hey "    lst = Xen_copy_arg(argv); ~%")
(hey "    for (i = 0; i < ref_argc; i++, lst = Xen_cdr(lst)) ref_argv[i] = Xen_to_C_char_(Xen_car(lst));~%")
(hay "    for (i = 0; i < ref_argc; i++, argv = s7_cdr(argv)) ref_argv[i] = (char *)s7_string(s7_car(argv));}~%")
(hoy "  }~%")
(hoy "  gtk_init(&ref_argc, &ref_argv);~%")
(hey "  return(Xen_list_2(C_to_Xen_int(ref_argc), C_to_Xen_char__(ref_argv)));~%")
(hay "  return(argc);~%")
(hoy "} ~%")
(hoy "~%")
(hey "static Xen gxg_gtk_init_check(Xen argc, Xen argv) ~%")
(hay "static s7_pointer lg_gtk_init_check(s7_scheme *sc, s7_pointer args)~%")
(hoy "{ ~%")
(hoy "  #define H_gtk_init_check \"gboolean gtk_init_check(int* argc, char*** argv)\" ~%")
(hoy "  int ref_argc = 0; ~%")
(hoy "  char** ref_argv = NULL; ~%")
(hay "  s7_pointer argc = NULL, argv = NULL;~%")
(hay "  if (s7_is_pair(args)) {argc = s7_car(args); ref_argc = s7_integer(argc); if (s7_is_pair(s7_cdr(args))) argv = s7_cadr(args);}~%")
(hay "  if ((argv) && (ref_argc > 0) && (s7_is_pair(argv)) && (ref_argc <= s7_list_length(sc, argv))) {~%")
(hay "  ref_argv = (char**)calloc(ref_argc, sizeof(char*)); ~%")
(hay "  { ~%")
(hay "    int i; ~%")
(hay "    for (i = 0; i < ref_argc; i++, argv = s7_cdr(argv)) ref_argv[i] = (char *)s7_string(s7_car(argv));}~%")
(hay "  }~%")
(hay "  gtk_init(&ref_argc, &ref_argv);~%")
(hay "  return(argc);~%")
(hay "} ~%")
(hay "~%")
(hey "  if (Xen_is_bound(argc) && Xen_is_list(argc)) ~%")
(hey "    { ~%")
(hey "      argv = argc; ~%")
(hey "      ref_argc = Xen_list_length(argv); ~%")
(hey "    } ~%")
(hey "  else ~%")
(hey "    {~%")
(hey "      if (Xen_is_bound(argv)) ~%")
(hey "	{ ~%")
(hey "	  int len; ~%")
(hey "	  Xen_check_type(Xen_is_integer(argc), argc, 1, \"gtk_init_check\", \"int argc\"); ~%")
(hey "	  Xen_check_type(Xen_is_list(argv), argv, 2, \"gtk_init_check\", \"char *argv[]\"); ~%")
(hey "	  len = Xen_list_length(argv); ~%")
(hey "	  ref_argc = Xen_to_C_int(argc); ~%")
(hey "	  if (ref_argc > len) ref_argc = len; ~%")
(hey "	}~%")
(hey "    }~%")
(hey "  ref_argv = (char**)calloc(ref_argc, sizeof(char*)); ~%")
(hey "  { ~%")
(hey "    int i; ~%")
(hey "    Xen lst; ~%")
(hey "    lst = Xen_copy_arg(argv); ~%")
(hey "    for (i = 0; i < ref_argc; i++, lst = Xen_cdr(lst)) ref_argv[i] = Xen_to_C_char_(Xen_car(lst));~%")
(hey "  }~%")
(hey "  {~%")
(hey "    Xen result;~%")
(hey "    result = C_to_Xen_gboolean(gtk_init_check(&ref_argc, &ref_argv));~%")
(hey "    return(Xen_list_3(result, C_to_Xen_int(ref_argc), C_to_Xen_char__(ref_argv)));~%")
(hey "  }~%")
(hey "}~%")
(hoy "#endif~%~%")

(define (array->list type)
  (hey "  if (ctype == xg_~A_symbol)~%" (no-stars type))
  (hey "    {~%")
  (hey "      ~A arr; arr = (~A)Xen_unwrap_C_pointer(Xen_cadr(val)); ~%" type type)
  (hey "      if (len == -1) {for (i = 0; arr[i]; i++) {}; len = i;}~%")
  (hey "      for (i = len - 1; i >= 0; i--) result = Xen_cons(C_to_Xen_~A(arr[i]), result);~%" (no-stars (deref-type (list type))))
  (hey "    }~%"))

(define (list->array type)
  (hey "  if (type == xg_~A_symbol)~%" (no-stars type))
  (hey "    {~%")
  (hey "      ~A arr; arr = (~A)calloc(len + 1, sizeof(~A));~%" type type (deref-type (list type)))
  (hey "      for (i = 0; i < len; i++, val = Xen_cdr(val)) arr[i] = Xen_to_C_~A(Xen_car(val));~%" (no-stars (deref-type (list type))))
  (hey "      return(Xen_list_3(xg_~A_symbol, Xen_wrap_C_pointer(arr), make_xm_obj(arr)));~%" (no-stars type))
  (hey "    }~%"))

(hey "/* conversions */~%")
(hey "static Xen c_array_to_xen_list(Xen val_1, Xen clen)~%")
(hey "{~%")
(hey "  Xen result = Xen_empty_list;~%")
(hey "  Xen val, ctype;~%")
(hey "  int i, len = -1;~%")
(hey "  if (Xen_is_integer(clen))~%")
(hey "    len = Xen_integer_to_C_int(clen);~%")
(hey "  if (!(Xen_is_list(val_1))) return(Xen_false); /* type:location cons */~%")
(hey "  val = Xen_copy_arg(val_1); /* protect Ruby arg */~%")
(hey "  ctype = Xen_car(val);~%")
(for-each array->list listable-types)
(for-each
 (lambda (type)
   (if (and (derefable type)
	    (not (or (member type listable-types)
		     (member type '("GError*" "GError**") string=?)))
	    (member (deref-type (list type)) types))
       (array->list type)))
 types)

;;; gotta handle GList* by hand
(hey "  if (ctype == xg_GList__symbol)~%")
(hey "    { /* tagging these pointers is currently up to the caller */~%")
(hey "      GList* lst;~%")
(hey "      lst = (GList*)Xen_unwrap_C_pointer(Xen_cadr(val));~%")
(hey "      len = g_list_length(lst);~%")
(hey "      for (i = len - 1; i >= 0; i--) result = Xen_cons(C_ulong_to_Xen_ulong(g_list_nth_data(lst, i)), result);~%")
(hey "    }~%")
(hey "  return(result);~%")
(hey "}~%~%")

(hey "static Xen xg_object_get(Xen val, Xen name, Xen string_type)~%")
(hey "{~%")
(hey "  gint temp; gchar *str;~%")
(hey "  Xen_check_type(Xen_is_gpointer(val), val, 1, \"g_object_get\", \"gpointer\");~%")
(hey "  Xen_check_type(Xen_is_string(name), name, 2, \"g_object_get\", \"string\");~%")
(hey "  if (Xen_is_false(string_type))~%")
(hey "    {g_object_get(Xen_to_C_gpointer(val), (const gchar *)(Xen_string_to_C_string(name)), &temp, NULL); return(C_int_to_Xen_integer(temp));}~%")
(hey "  else {g_object_get(Xen_to_C_gpointer(val), (const gchar *)(Xen_string_to_C_string(name)), &str, NULL); return(C_string_to_Xen_string(str));}~%")
(hey "}~%~%")

;;; (g_object_get (GPOINTER (gtk_settings_get_default)) "gtk-enable-tooltips" #f)

(hey "static Xen xg_object_set(Xen val, Xen name, Xen new_val)~%")
(hey "{~%")
(hey "  Xen_check_type(Xen_is_gpointer(val), val, 1, \"g_object_set\", \"gpointer\");~%")
(hey "  Xen_check_type(Xen_is_string(name), name, 2, \"g_object_set\", \"string\");~%")
(hey "  if (Xen_is_boolean(new_val))~%")
(hey "    g_object_set(Xen_to_C_gpointer(val), (const gchar *)(Xen_string_to_C_string(name)), Xen_boolean_to_C_bool(new_val), NULL);~%")
(hey "  else~%")
(hey "    {~%")
(hey "      if (Xen_is_number(new_val))~%")
(hey "        g_object_set(Xen_to_C_gpointer(val), (const gchar *)(Xen_string_to_C_string(name)), Xen_integer_to_C_int(new_val), NULL);~%")
(hey "      else g_object_set(Xen_to_C_gpointer(val), (const gchar *)(Xen_string_to_C_string(name)), Xen_string_to_C_string(new_val), NULL);~%")
(hey "    }~%")
(hey "  return(new_val);~%")
(hey "}~%~%")

(hey "static Xen xg_gtk_event_keyval(Xen event)~%")
(hey "{~%")
(hey "  GdkEventKey *e;~%")
(hey "  e = Xen_to_C_GdkEventKey_(event);~%")
(hey "#if (GTK_CHECK_VERSION(3, 92, 1))~%")
(hey "  if (e) {guint val = 0; gdk_event_get_keyval((GdkEvent *)e, &val); return(C_int_to_Xen_integer((int)val));} return(XEN_ZERO);~%")
(hey "#else~%")
(hey "  if (e) return(C_int_to_Xen_integer((int)(e->keyval)));~% return(XEN_ZERO);~%")
(hey "#endif~%")
(hey "}~%~%")


(hay "static s7_pointer lg_g_object_get(s7_scheme *sc, s7_pointer args)~%")
(hay "{~%")
(hay "  s7_pointer val, name, string_type;~%")
(hay "  gint temp; gchar *str;~%")
(hay "  val = s7_car(args);~%")
(hay "  if (!s7_is_c_pointer(val)) s7_wrong_type_arg_error(sc, \"g_object_get\", 1, val, \"gpointer\");~%")
(hay "  name = s7_cadr(args);~%")
(hay "  if (!s7_is_string(name)) s7_wrong_type_arg_error(sc, \"g_object_get\", 2, name, \"string\");~%")
(hay "  string_type = s7_caddr(args);~%")
(hay "  if (string_type == lg_false)~%")
(hay "    {g_object_get((gpointer)s7_c_pointer(val), (const gchar *)s7_string(name), &temp, NULL); return(s7_make_integer(sc, temp));}~%")
(hay "  else {g_object_get((gpointer)s7_c_pointer(val), (const gchar *)s7_string(name), &str, NULL); return(s7_make_string(sc, str));}~%")
(hay "}~%~%")

(hay "static s7_pointer lg_g_object_set(s7_scheme *sc, s7_pointer args)~%")
(hay "{~%")
(hay "  s7_pointer val, name, new_val;~%")
(hay "  val = s7_car(args);~%")
(hay "  if (!s7_is_c_pointer(val)) s7_wrong_type_arg_error(sc, \"g_object_set\", 1, val, \"gpointer\");~%")
(hay "  name = s7_cadr(args);~%")
(hay "  if (!s7_is_string(name)) s7_wrong_type_arg_error(sc, \"g_object_set\", 2, name, \"string\");~%")
(hay "  new_val = s7_caddr(args);~%")
(hay "  if (s7_is_boolean(new_val))~%")
(hay "    g_object_set((gpointer)s7_c_pointer(val), (const gchar *)s7_string(name), s7_boolean(sc, new_val), NULL);~%")
(hay "  else~%")
(hay "    {~%")
(hay "      if (s7_is_number(new_val))~%")
(hay "        g_object_set((gpointer)s7_c_pointer(val), (const gchar *)s7_string(name), s7_integer(new_val), NULL);~%")
(hay "      else g_object_set((gpointer)s7_c_pointer(val), (const gchar *)s7_string(name), s7_string(new_val), NULL);~%")
(hay "    }~%")
(hay "  return(new_val);~%")
(hay "}~%~%")

(hay "static s7_pointer lg_gtk_event_keyval(s7_scheme *sc, s7_pointer args)~%")
(hay "{~%")
(hay "  GdkEventKey *e;~%")
(hay "  e = (GdkEventKey*)s7_c_pointer(s7_car(args));~%")
(hay " if (e) return(s7_make_integer(sc, (int)(e->keyval)));~% return(s7_make_integer(sc, 0));~%")
(hay "}~%~%")


(hey "static Xen xen_list_to_c_array(Xen val, Xen type)~%")
(hey "{~%")
(hey "  int i, len;~%")
(hey "  len = Xen_list_length(val);~%")

(for-each list->array listable-types)
(for-each
 (lambda (type)
   (if (and (derefable type)
	    (not (or (member type listable-types)
		     (member type '("GError*" "GError**") string=?)))
	    (member (deref-type (list type)) types))
       (list->array type)))
 types)
(hey "  return(Xen_false);~%")
(hey "}~%")


(hey "static Xen gxg_make_GtkTextIter(void)~%")
(hay "static s7_pointer lg_make_GtkTextIter(s7_scheme *sc, s7_pointer args)~%")
(hoy "{~%")
(hoy "  GtkTextIter* result;~%")
(hoy "  result = (GtkTextIter*)calloc(1, sizeof(GtkTextIter));~%")
(hey "  return(Xen_list_3(C_string_to_Xen_symbol(\"GtkTextIter_\"), Xen_wrap_C_pointer(result), make_xm_obj(result)));~%")
(hay "  return(s7_make_c_pointer_with_type(sc, result, s7_make_symbol(sc, \"GtkTextIter_\"), make_xm_obj(sc, result)));~%")
(hoy "}~%")
(hoy "~%")
(hey "static Xen gxg_make_GtkTreeIter(void)~%")
(hay "static s7_pointer lg_make_GtkTreeIter(s7_scheme *sc, s7_pointer args)~%")
(hoy "{~%")
(hoy "  GtkTreeIter* result;~%")
(hoy "  result = (GtkTreeIter*)calloc(1, sizeof(GtkTreeIter));~%")
(hey "  return(Xen_list_3(C_string_to_Xen_symbol(\"GtkTreeIter_\"), Xen_wrap_C_pointer(result), make_xm_obj(result)));~%")
(hay "  return(s7_make_c_pointer_with_type(sc, result, s7_make_symbol(sc, \"GtkTreeIter_\"), make_xm_obj(sc, result)));~%")
(hoy "}~%")
(hoy "~%")
(hey "static Xen gxg_make_PangoRectangle(void)~%")
(hay "static s7_pointer lg_make_PangoRectangle(s7_scheme *sc, s7_pointer args)~%")
(hoy "{~%")
(hoy "  PangoRectangle* result;~%")
(hoy "  result = (PangoRectangle*)calloc(1, sizeof(PangoRectangle));~%")
(hey "  return(Xen_list_3(C_string_to_Xen_symbol(\"PangoRectangle_\"), Xen_wrap_C_pointer(result), make_xm_obj(result)));~%")
(hay "  return(s7_make_c_pointer_with_type(sc, result, s7_make_symbol(sc, \"PangoRectangle_\"), make_xm_obj(sc, result)));~%")
(hoy "}~%")
(hoy "~%")
(hey "static Xen gxg_make_cairo_matrix_t(void)~%")
(hay "static s7_pointer lg_make_cairo_matrix_t(s7_scheme *sc, s7_pointer args)~%")
(hoy "{~%")
(hoy "  cairo_matrix_t* result;~%")
(hoy "  result = (cairo_matrix_t*)calloc(1, sizeof(cairo_matrix_t));~%")
(hey "  return(Xen_list_3(C_string_to_Xen_symbol(\"cairo_matrix_t_\"), Xen_wrap_C_pointer(result), make_xm_obj(result)));~%")
(hay "  return(s7_make_c_pointer_with_type(sc, result, s7_make_symbol(sc, \"cairo_matrix_t_\"), make_xm_obj(sc, result)));~%")
(hoy "}~%~%")
(hoy "#if GTK_CHECK_VERSION(3, 0, 0)~%")
(hey "static Xen gxg_make_GdkRGBA(void)~%")
(hay "static s7_pointer lg_make_GdkRGBA(s7_scheme *sc, s7_pointer args)~%")
(hoy "{~%")
(hoy "  GdkRGBA* result;~%")
(hoy "  result = (GdkRGBA*)calloc(1, sizeof(GdkRGBA));~%")
(hey "  return(Xen_list_3(C_string_to_Xen_symbol(\"GdkRGBA_\"), Xen_wrap_C_pointer(result), make_xm_obj(result)));~%")
(hay "  return(s7_make_c_pointer_with_type(sc, result, s7_make_symbol(sc, \"GdkRGBA_\"), make_xm_obj(sc, result)));~%")
(hoy "}~%")
(hoy "#endif~%~%")


;;; these changed from void to gboolean
(hey "static Xen gxg_gtk_text_view_get_iter_at_position(Xen text_view, Xen iter, Xen ignore_trailing, Xen x, Xen y)~%")
(hey "{~%")
(hey "  #define H_gtk_text_view_get_iter_at_position \"gboolean gtk_text_view_get_iter_at_position(GtkTextView* text_view, GtkTextIter* iter, gint* [trailing], gint x, gint y)\"~%")
(hey "  gint ref_trailing;~%")
(hey "  Xen_check_type(Xen_is_GtkTextView_(text_view), text_view, 1, \"gtk_text_view_get_iter_at_position\", \"GtkTextView*\");~%")
(hey "  Xen_check_type(Xen_is_GtkTextIter_(iter), iter, 2, \"gtk_text_view_get_iter_at_position\", \"GtkTextIter*\");~%")
(hey "  Xen_check_type(Xen_is_gint(x), x, 4, \"gtk_text_view_get_iter_at_position\", \"gint\");~%")
(hey "  Xen_check_type(Xen_is_gint(y), y, 5, \"gtk_text_view_get_iter_at_position\", \"gint\");~%")
(hey "#if GTK_CHECK_VERSION(3, 20, 0)~%")
(hey "  {~%")
(hey "    Xen result;~%")
(hey "    result = C_to_Xen_gboolean(gtk_text_view_get_iter_at_position(Xen_to_C_GtkTextView_(text_view), Xen_to_C_GtkTextIter_(iter), ~%")
(hey "                                                                  &ref_trailing, Xen_to_C_gint(x), Xen_to_C_gint(y)));~%")
(hey "    return(Xen_list_2(result, C_to_Xen_gint(ref_trailing)));~%")
(hey "   }~%")
(hey "#else~%")
(hey "  gtk_text_view_get_iter_at_position(Xen_to_C_GtkTextView_(text_view), Xen_to_C_GtkTextIter_(iter), &ref_trailing, Xen_to_C_gint(x), Xen_to_C_gint(y));~%")
(hey "  return(Xen_list_1(C_to_Xen_gint(ref_trailing)));~%")
(hey "#endif~%")
(hey "}~%~%")
(hey "static Xen gxg_gtk_text_view_get_iter_at_location(Xen text_view, Xen iter, Xen x, Xen y)~%")
(hey "{~%")
(hey "  #define H_gtk_text_view_get_iter_at_location \"gboolean gtk_text_view_get_iter_at_location(GtkTextView* text_view, GtkTextIter* iter, gint x, gint y)\"~%")
(hey "  Xen_check_type(Xen_is_GtkTextView_(text_view), text_view, 1, \"gtk_text_view_get_iter_at_location\", \"GtkTextView*\");~%")
(hey "  Xen_check_type(Xen_is_GtkTextIter_(iter), iter, 2, \"gtk_text_view_get_iter_at_location\", \"GtkTextIter*\");~%")
(hey "  Xen_check_type(Xen_is_gint(x), x, 3, \"gtk_text_view_get_iter_at_location\", \"gint\");~%")
(hey "  Xen_check_type(Xen_is_gint(y), y, 4, \"gtk_text_view_get_iter_at_location\", \"gint\");~%")
(hey "#if GTK_CHECK_VERSION(3, 20, 0)~%")
(hey "  return(C_to_Xen_gboolean(gtk_text_view_get_iter_at_location(Xen_to_C_GtkTextView_(text_view), Xen_to_C_GtkTextIter_(iter), Xen_to_C_gint(x), Xen_to_C_gint(y))));~%")
(hey "#else~%")
(hey "  gtk_text_view_get_iter_at_location(Xen_to_C_GtkTextView_(text_view), Xen_to_C_GtkTextIter_(iter), Xen_to_C_gint(x), Xen_to_C_gint(y));~%")
(hey "  return(Xen_false);~%")
(hey "#endif~%")
(hey "}~%~%")

(hay "static s7_pointer lg_gtk_text_view_get_iter_at_position(s7_scheme *sc, s7_pointer args)~%")
(hay "{~%")
(hay "  #define H_gtk_text_view_get_iter_at_position \"gboolean gtk_text_view_get_iter_at_position(GtkTextView* text_view, GtkTextIter* iter, gint* [trailing], gint x, gint y)\"~%")
(hay "  gint ref_trailing;~%")
(hay "  s7_pointer text_view, iter, x, y, p;~%")
(hay "  text_view = s7_car(args);~%")
(hay "  if (!s7_is_c_pointer_of_type(text_view, GtkTextView__sym)) s7_wrong_type_arg_error(sc, \"gtk_text_view_get_iter_at_position\", 1, text_view, \"GtkTextView*\");~%")
(hay "  iter = s7_cadr(args);~%")
(hay "  if (!s7_is_c_pointer_of_type(iter, GtkTextIter__sym)) s7_wrong_type_arg_error(sc, \"gtk_text_view_get_iter_at_position\", 2, iter, \"GtkTextIter*\");~%")
(hay "  p = s7_cdddr(args); x = s7_car(p); y = s7_cadr(p);~%")
(hay "  if (!s7_is_integer(x)) s7_wrong_type_arg_error(sc, \"gtk_text_view_get_iter_at_position\", 4, x, \"gint\");~%")
(hay "  if (!s7_is_integer(y)) s7_wrong_type_arg_error(sc, \"gtk_text_view_get_iter_at_position\", 5, y, \"gint\");~%")
(hay "#if GTK_CHECK_VERSION(3, 20, 0)~%")
(hay "  {~%")
(hay "    s7_pointer result;~%")
(hay "    result = s7_make_boolean(sc, gtk_text_view_get_iter_at_position((GtkTextView*)s7_c_pointer(text_view), (GtkTextIter*)s7_c_pointer(iter), ~%")
(hay "                                                                  &ref_trailing, s7_integer(x), s7_integer(y)));~%")
(hay "    return(s7_list(sc, 2, result, s7_make_integer(sc, ref_trailing)));~%")
(hay "   }~%")
(hay "#else~%")
(hay "  gtk_text_view_get_iter_at_position((GtkTextView*)s7_c_pointer(text_view), (GtkTextIter*)s7_c_pointer(iter), &ref_trailing, s7_integer(x), s7_integer(y));~%")
(hay "  return(s7_list(sc, 1, s7_make_integer(sc, (ref_trailing)));~%")
(hay "#endif~%")
(hay "}~%~%")
(hay "static s7_pointer lg_gtk_text_view_get_iter_at_location(s7_scheme *sc, s7_pointer args)~%")
(hay "{~%")
(hay "  #define H_gtk_text_view_get_iter_at_location \"gboolean gtk_text_view_get_iter_at_location(GtkTextView* text_view, GtkTextIter* iter, gint x, gint y)\"~%")
(hay "  s7_pointer text_view, iter, x, y, p;~%")
(hay "  text_view = s7_car(args);~%")
(hay "  if (!s7_is_c_pointer_of_type(text_view, GtkTextView__sym)) s7_wrong_type_arg_error(sc, \"gtk_text_view_get_iter_at_location\", 1, text_view, \"GtkTextView*\");~%")
(hay "  iter = s7_cadr(args);~%")
(hay "  if (!s7_is_c_pointer_of_type(iter, GtkTextIter__sym)) s7_wrong_type_arg_error(sc, \"gtk_text_view_get_iter_at_location\", 2, iter, \"GtkTextIter*\");~%")
(hay "  p = s7_cddr(args); x = s7_car(p); y = s7_cadr(p);~%")
(hay "  if (!s7_is_integer(x)) s7_wrong_type_arg_error(sc, \"gtk_text_view_get_iter_at_location\", 4, x, \"gint\");~%")
(hay "  if (!s7_is_integer(y)) s7_wrong_type_arg_error(sc, \"gtk_text_view_get_iter_at_location\", 5, y, \"gint\");~%")
(hay "#if GTK_CHECK_VERSION(3, 20, 0)~%")
(hay "  return(s7_make_boolean(sc, gtk_text_view_get_iter_at_location((GtkTextView*)s7_c_pointer(text_view), (GtkTextIter*)s7_c_pointer(iter), s7_integer(x), s7_integer(y))));~%")
(hay "#else~%")
(hay "  gtk_text_view_get_iter_at_location((GtkTextView*)s7_c_pointer(text_view), (GtkTextIter*)s7_c_pointer(iter), s7_integer(x), s7_integer(y));~%")
(hay "  return(lg_false);~%")
(hay "#endif~%")
(hay "}~%~%")


(hey "#if HAVE_SCHEME~%")
(hey "  #define Xg_define_procedure(Name, Value, A1, A2, A3, Help, Sig) s7_define_typed_function(s7, Xg_pre #Name Xg_post, Value, A1, A2, A3, Help, Sig)~%")
(hey "#else~%")
(hey "  #define Xg_define_procedure(Name, Value, A1, A2, A3, Help, Sig) Xen_define_safe_procedure(Xg_pre #Name Xg_post, Value, A1, A2, A3, Help)~%")
(hey "#endif~%")
(hey "~%")

(hey "Xen_wrap_no_args(gxg_make_GtkTextIter_w, gxg_make_GtkTextIter)~%")
(hey "Xen_wrap_no_args(gxg_make_GtkTreeIter_w, gxg_make_GtkTreeIter)~%")
(hey "Xen_wrap_no_args(gxg_make_PangoRectangle_w, gxg_make_PangoRectangle)~%")
(hey "Xen_wrap_no_args(gxg_make_cairo_matrix_t_w, gxg_make_cairo_matrix_t)~%")
(hey "Xen_wrap_4_args(gxg_gtk_text_view_get_iter_at_location_w, gxg_gtk_text_view_get_iter_at_location)~%")
(hey "Xen_wrap_5_optional_args(gxg_gtk_text_view_get_iter_at_position_w, gxg_gtk_text_view_get_iter_at_position)~%")

(hey "#if GTK_CHECK_VERSION(3, 0, 0)~%")
(hey "Xen_wrap_no_args(gxg_make_GdkRGBA_w, gxg_make_GdkRGBA)~%")
(hey "Xen_wrap_1_arg(gxg_make_cairo_w, gxg_make_cairo)~%")
(hey "Xen_wrap_1_arg(gxg_free_cairo_w, gxg_free_cairo)~%")
(hey "#endif~%")

(hey "~%~%")
(hey "static void define_structs(void)~%")
(hey "{~%")
(hey "  Xg_define_procedure(GtkTextIter, gxg_make_GtkTextIter_w, 0, 0, 0, \"(GtkTextIter): a new GtkTextIter struct\", NULL);~%")
(hey "  Xg_define_procedure(GtkTreeIter, gxg_make_GtkTreeIter_w, 0, 0, 0, \"(GtkTreeIter): a new GtkTreeIter struct\", NULL);~%")
(hey "  Xg_define_procedure(PangoRectangle, gxg_make_PangoRectangle_w, 0, 0, 0, \"(PangoRectangle): a new PangoRectangle struct\", NULL);~%")
(hey "  Xg_define_procedure(cairo_matrix_t, gxg_make_cairo_matrix_t_w, 0, 0, 0, \"(cairo_matrix_t): a new cairo_matrix_t struct\", NULL);~%")
(hey "  Xg_define_procedure(gtk_text_view_get_iter_at_location, gxg_gtk_text_view_get_iter_at_location_w, 4, 0, 0, H_gtk_text_view_get_iter_at_location, NULL);~%")
(hey "  Xg_define_procedure(gtk_text_view_get_iter_at_position, gxg_gtk_text_view_get_iter_at_position_w, 4, 1, 0, H_gtk_text_view_get_iter_at_position, NULL);~%")
(hey "#if GTK_CHECK_VERSION(3, 0, 0)~%")
(hey "  Xg_define_procedure(GdkRGBA, gxg_make_GdkRGBA_w, 0, 0, 0, \"(GdkRGBA): a new GdkRGBA struct\", NULL);~%")
(hey "#endif~%")
(hey "}~%~%")
  
(hay "~%~%")
(hay "static void define_structs(s7_scheme *sc)~%")
(hay "{~%")
(hay "  s7_define_function(sc, \"GtkTextIter\", lg_make_GtkTextIter, 0, 0, 0, \"(GtkTextIter): a new GtkTextIter struct\");~%")
(hay "  s7_define_function(sc, \"GtkTreeIter\", lg_make_GtkTreeIter, 0, 0, 0, \"(GtkTreeIter): a new GtkTreeIter struct\");~%")
(hay "  s7_define_function(sc, \"PangoRectangle\", lg_make_PangoRectangle, 0, 0, 0, \"(PangoRectangle): a new PangoRectangle struct\");~%")
(hay "  s7_define_function(sc, \"cairo_matrix_t\", lg_make_cairo_matrix_t, 0, 0, 0, \"(cairo_matrix_t): a new cairo_matrix_t struct\");~%")
(hay "  s7_define_function(sc, \"gtk_text_view_get_iter_at_location\", lg_gtk_text_view_get_iter_at_location, 4, 0, 0, H_gtk_text_view_get_iter_at_location);~%")
(hay "  s7_define_function(sc, \"gtk_text_view_get_iter_at_position\", lg_gtk_text_view_get_iter_at_position, 4, 1, 0, H_gtk_text_view_get_iter_at_position);~%")
(hay "#if GTK_CHECK_VERSION(3, 0, 0)~%")
(hay "  s7_define_function(sc, \"GdkRGBA\", lg_make_GdkRGBA, 0, 0, 0, \"(GdkRGBA): a new GdkRGBA struct\");~%")
(hay "#endif~%")
(hay "}~%~%")



;;; ---------------- argify ----------------

(define (argify-func func)
  (let ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 ;(args (- cargs refargs))
	 )
    (hey "Xen_wrap_~A(gxg_~A_w, gxg_~A)~%" 
	 (if (>= cargs max-args) 
	     "any_args"
	     (format #f (if (> refargs 0)
			    (values "~D_optional_arg~A" cargs (if (= cargs 1) "" "s"))
			    (values "~A_arg~A" (if (zero? cargs) "no" (number->string cargs)) (if (= cargs 1) "" "s")))))
	 (car func) (car func))))
#|
(define (unargify-func func)
  (hey "#define gxg_~A_w gxg_~A~%" 
       (car func) (car func)))
|#

(for-each argify-func (reverse funcs))
(for-each
 (lambda (func-list with-func)
   (if (pair? func-list) 
       (with-func hey (lambda () 
			(for-each argify-func (reverse func-list))))))
 all-funcs all-func-withs)


(hey "Xen_wrap_1_arg(gxg_GPOINTER_w, gxg_GPOINTER)~%")
(hey "Xen_wrap_2_args(c_array_to_xen_list_w, c_array_to_xen_list)~%")
(hey "Xen_wrap_2_args(xen_list_to_c_array_w, xen_list_to_c_array)~%")
(hey "Xen_wrap_3_args(xg_object_get_w, xg_object_get)~%")
(hey "Xen_wrap_3_args(xg_object_set_w, xg_object_set)~%")
(hey "Xen_wrap_1_arg(xg_gtk_event_keyval_w, xg_gtk_event_keyval)~%")

(hey "#if (!GTK_CHECK_VERSION(3, 90, 0))~%")
(hey "Xen_wrap_2_optional_args(gxg_gtk_init_w, gxg_gtk_init)~%")
(hey "Xen_wrap_2_optional_args(gxg_gtk_init_check_w, gxg_gtk_init_check)~%")
(hey "Xen_wrap_2_args(gxg_gtk_widget_set_events_w, gxg_gtk_widget_set_events)~%")
(hey "#endif~%")

(define (ruby-cast func) (hey "Xen_wrap_1_arg(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func)))) 
(for-each ruby-cast (reverse casts))
(for-each
 (lambda (cast-list cast-func)
   (if (pair? cast-list) 
       (cast-func hey (lambda () 
			(for-each ruby-cast (reverse cast-list))))))
 all-casts all-cast-withs)

;;(define (ruby-check func) (hey "Xen_wrap_1_arg(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func))))
(define ruby-check ruby-cast)
(for-each ruby-check (reverse checks))
(for-each
 (lambda (check-list check-func)
   (if (pair? check-list) 
       (check-func hey (lambda () 
			 (for-each ruby-check (reverse check-list))))))
 all-checks all-check-withs)


;;; --------------------------------------------------------------------------------

(define (gtk-type->s7-type gtk)
  (cond ((member gtk declared-names (lambda (a b)
				      (string=? a (cadr b))))
	 'gtk_enum_t?)
	((assoc gtk direct-types) => (lambda (dt)
				       (or (not (string? (cdr dt)))
					   (let ((direct (cdr dt)))
					     (cond ((member direct '("INT" "ULONG") string=?) 'integer?)
						   ((assoc direct '(("BOOLEAN" . boolean?) 
								    ("DOUBLE"  . real?) 
								    ("String"  . string?)) string=?)
						    => cdr)
						   (else #t))))))
	(else (or (not (has-stars gtk)) 'pair?))))
	       
(define make-signature
  (let ((compress (lambda (sig)
		    (do ((sig sig (cdr sig)))
			((not (and (pair? sig)
				   (pair? (cdr sig))
				   (not (and (eq? (car sig) 'pair?) 
					     (null? (cddr sig))))
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
(define (make-signatures lst)
  (for-each
   (lambda (f)
     (let ((sig (make-signature f)))
       (if (pair? sig)
	   (let ((count (signatures sig)))
	     (set! (signatures sig) (if (not (real? count)) 0 (+ count 1)))))))
   lst))

(make-signatures funcs)
(for-each make-signatures all-funcs)

;(format *stderr* "~D entries, ~D funcs~%" (hash-table-entries signatures) (length funcs))

;;; --------------------------------------------------------------------------------
(hey "static void define_functions(void)~%")
(hay "static void define_functions(s7_scheme *sc)~%")
(hoy "{~%")
(hey "#if HAVE_SCHEME~%")

(hoy "  s7_pointer s_boolean, s_integer, s_real, s_string, s_any, s_pair, s_float, s_gtk_enum_t, s_pair_false;~%")
(hoy "  s7_pointer ")

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
     
(for-each
 (lambda (sigc)
   (let ((sig (car sigc)))
     (hoy (sig-name sig))
     (hoy ", ")))
 signatures)
(hoy "pl_bpt;~%")
(hey "#endif~%~%")
(hay "~%")
;(hay "  s7_pointer cur_env;~%")
;(hay "  cur_env = s7_curlet(sc);~%~%")

(hey "  xm_gc_table = Xen_make_vector(1, Xen_false);~%")
(hey "  Xen_GC_protect(xm_gc_table);~%")
(hey "  xm_protected_size = 512;~%")
(hey "  xm_protected = Xen_make_vector(xm_protected_size, Xen_false);~%")
(hey "  Xen_vector_set(xm_gc_table, 0, xm_protected);~%~%")

(hey "#if HAVE_SCHEME~%")
(hey "  s_boolean = s7_make_symbol(s7, \"boolean?\");~%")
(hey "  s_integer = s7_make_symbol(s7, \"integer?\");~%")
(hey "  s_real = s7_make_symbol(s7, \"real?\");~%")
(hey "  s_float = s7_make_symbol(s7, \"float?\");~%")
(hey "  s_string = s7_make_symbol(s7, \"string?\");~%")
(hey "  s_pair = s7_make_symbol(s7, \"pair?\");~%")
(hey "  s_pair_false = s7_make_signature(s7, 2, s_pair, s_boolean);~%")
(hey "  s_gtk_enum_t = s7_make_symbol(s7, \"gtk_enum_t?\");~%")
(hey "  s_any = s7_t(s7);~%~%")

(hay "  s_boolean = s7_make_symbol(sc, \"boolean?\");~%")
(hay "  s_integer = s7_make_symbol(sc, \"integer?\");~%")
(hay "  s_real = s7_make_symbol(sc, \"real?\");~%")
(hay "  s_float = s7_make_symbol(sc, \"float?\");~%")
(hay "  s_string = s7_make_symbol(sc, \"string?\");~%")
(hay "  s_pair = s7_make_symbol(sc, \"pair?\");~%")
(hay "  s_pair_false = s7_make_signature(sc, 2, s_pair, s_boolean);~%")
(hay "  s_gtk_enum_t = s7_make_symbol(sc, \"gtk_enum_t?\");~%")
(hay "  s_any = s7_t(sc);~%~%")

(for-each
 (lambda (sigc)
   (let ((sig (car sigc)))
     (hoy "  ")
     (hoy (sig-name sig))
     (hey " = s7_make_circular_signature(s7, ")
     (hay " = s7_make_circular_signature(sc, ")
     (let ((len (length sig)))
       (hoy (number->string (- len 1)))
       (hoy ", ")
       (hoy (number->string len))
       (hoy ", ")
       (hoy (case (car sig)
	      ((integer?)    "s_integer")
	      ((boolean?)    "s_boolean")
	      ((real?)       "s_float")
	      ((string?)     "s_string")
	      ((pair?)       "s_pair")
	      ((gtk_enum_t?) "s_gtk_enum_t")
	      (else          "s_any")))
       (if (> len 1) (hoy ", "))
       (do ((i 1 (+ i 1))
	    (s (cdr sig) (cdr s)))
	   ((= i len))
	 (let ((typ (car s)))
	   (hoy (case typ
		  ((integer?)    "s_integer")
		  ((boolean?)    "s_boolean")
		  ((real?)       "s_real")
		  ((string?)     "s_string")
		  ((pair?)       "s_pair_false")
		  ((gtk_enum_t?) "s_gtk_enum_t")
		  (else       "s_any"))))
	 (if (< i (- len 1)) (hoy ", "))))
     (hoy ");~%")))
 signatures)
(hey "  pl_bpt = s7_make_signature(s7, 2, s_pair_false, s_any);~%")
(hay "  pl_bpt = s7_make_signature(sc, 2, s_pair_false, s_any);~%")
(hey "#endif~%~%")
(hay "~%")

(define (defun func)
  (let* ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs))
	 ;(return-type (cadr func))
	 ;(typ (assoc return-type direct-types))
	 )
    (hey "  Xg_define_procedure(~A, gxg_~A_w, ~D, ~D, ~D, H_~A, ~A);~%"
	 (car func) (car func) 
	 (if (>= cargs max-args) 
             (values 0 0 1)
             (values args refargs 0))
	 (car func)
	 (sig-name (make-signature func)))
    (hay "  s7_define_typed_function(sc, \"~A\", lg_~A, ~D, ~D, ~D, H_~A, ~A);~%"
	 (car func) (car func) 
	 (if (>= cargs max-args) 
             (values 0 0 1)
             (values args refargs 0))
	 (car func)
	 (sig-name (make-signature func)))))

(for-each defun (reverse funcs))
(for-each
 (lambda (func-list with-func)
   (if (pair? func-list) 
       (with-func hoy (lambda () 
			(for-each defun (reverse func-list))))))
 all-funcs all-func-withs)

(define (cast-out func)
  (let ((f (car func)))
    (hey "  Xg_define_procedure(~A, gxg_~A_w, 1, 0, 0, \"(~A obj) casts obj to ~A\", pl_bpt);~%" 
	 (no-arg f)
	 (no-arg f)
	 (no-arg f)
	 (no-arg f))
    (hay "  s7_define_typed_function(sc, \"~A\", lg_~A, 1, 0, 0, \"(~A obj) casts obj to ~A\", pl_bpt);~%" 
	 (no-arg f)
	 (no-arg f)
	 (no-arg f)
	 (no-arg f))))


(hey "  Xg_define_procedure(GPOINTER, gxg_GPOINTER_w, 1, 0, 0, \"(GPOINTER obj) casts obj to GPOINTER\", NULL);~%")
(hay "  s7_define_typed_function(sc, \"GPOINTER\", lg_GPOINTER, 1, 0, 0, \"(GPOINTER obj) casts obj to GPOINTER\", NULL);~%")

(for-each cast-out (reverse casts))
(for-each
 (lambda (cast-list cast-func)
   (if (pair? cast-list) 
       (cast-func hoy 
		  (lambda () 
		    (for-each cast-out (reverse cast-list))))))
 all-casts all-cast-withs)


(hey "  Xg_define_procedure(c-array->list, c_array_to_xen_list_w, 2, 0, 0, NULL, NULL);~%")
(hey "  Xg_define_procedure(list->c-array, xen_list_to_c_array_w, 2, 0, 0, NULL, NULL);~%")
(hey "  Xg_define_procedure(g_object_get, xg_object_get_w, 3, 0, 0, NULL, NULL);~%")
(hey "  Xg_define_procedure(g_object_set, xg_object_set_w, 3, 0, 0, NULL, NULL);~%")
(hey "  Xg_define_procedure(gtk_event_keyval, xg_gtk_event_keyval_w, 1, 0, 0, NULL, NULL);~%")

(hay "  s7_define_function(sc, \"g_object_get\", lg_g_object_get, 3, 0, 0, NULL);~%")
(hay "  s7_define_function(sc, \"g_object_set\", lg_g_object_set, 3, 0, 0, NULL);~%")
(hay "  s7_define_function(sc, \"gtk_event_keyval\", lg_gtk_event_keyval, 1, 0, 0, NULL);~%")

(hoy "#if (!GTK_CHECK_VERSION(3, 90, 0))~%")
(hey "  Xg_define_procedure(gtk_init, gxg_gtk_init_w, 0, 2, 0, H_gtk_init, NULL);~%")
(hey "  Xg_define_procedure(gtk_init_check, gxg_gtk_init_check_w, 0, 2, 0, H_gtk_init_check, NULL);~%")
(hay "  s7_define_function(sc, \"gtk_init\", lg_gtk_init, 0, 2, 0, NULL);~%")
(hay "  s7_define_function(sc, \"gtk_init_check\", lg_gtk_init_check, 0, 2, 0, NULL);~%")
(hey "  Xg_define_procedure(gtk_widget_set_events, gxg_gtk_widget_set_events_w, 2, 0, 0, H_gtk_widget_set_events, pl_tui);~%")
(hay "  s7_define_function(sc, \"gtk_widget_set_events\", lg_gtk_widget_set_events, 2, 0, 0, H_gtk_widget_set_events);~%")
(hoy "#endif~%")

(define (check-out func)
  (let ((f (car func))
	(typ (cadr func)))
    (hey "  Xg_define_procedure(~A, gxg_~A_w, 1, 0, 0, \"(~A obj): \" PROC_TRUE \" if obj is a ~A\", pl_bt);~%" 
       (no-arg f)
       (no-arg f)
       (no-arg f)
       typ)
    (hay "  s7_define_typed_function(sc, \"~A\", lg_~A, 1, 0, 0, \"(~A obj): #t if obj is a ~A\", pl_bt);~%" 
       (no-arg f)
       (no-arg f)
       (no-arg f)
       typ)))

(for-each check-out (reverse checks))
(for-each
 (lambda (check-list check-func)
   (if (pair? check-list) 
       (check-func hoy (lambda () 
			 (for-each check-out (reverse check-list))))))
 all-checks all-check-withs)

(hey "#if GTK_CHECK_VERSION(3, 0, 0)~%")
(hey "Xg_define_procedure(make_cairo, gxg_make_cairo_w, 1, 0, 0, H_make_cairo, pl_pu);~%")
(hey "Xg_define_procedure(free_cairo, gxg_free_cairo_w, 1, 0, 0, H_free_cairo, pl_pu);~%")
(hey "#endif~%")

(hoy "}~%~%")



(hey "/* ---------------------------------------- constants ---------------------------------------- */~%~%")
(hey "static void define_integers(void)~%")
(hey "{~%")
(hey "#define define_integer(Name) Xen_define(Xg_pre #Name Xg_post, C_int_to_Xen_integer(Name))~%")
(hey "~%")
(hey "#if !GLIB_CHECK_VERSION(2,35,0)~%")
(hey "  g_type_init();~%")
(hey "#endif~%")

(for-each 
 (lambda (val) 
   (hey "  define_integer(~A);~%" val)) 
 (reverse ints))

(for-each
 (lambda (ints-list with-ints)
   (if (pair? ints-list)
       (with-ints hey (lambda () 
			(for-each (lambda (val) 
				    (hey "  define_integer(~A);~%" val)) 
				  (reverse ints-list))))))
 all-ints all-int-withs)

(hey "}~%~%")

(hey "static void define_doubles(void)~%")
(hey "{~%")
(hey "#define define_double(Name) Xen_define(Xg_pre #Name Xg_post, C_double_to_Xen_real(Name))~%")
(hey "~%")

(for-each
 (lambda (val)
   (hey "  define_double(~A);~%" val))
 (reverse dbls))
(hey "}~%~%")

;;; ----------------
(hay "static void define_integers(s7_scheme *sc)~%")
(hay "{~%")
(hay "  s7_pointer cur_env;~%")
(hay "  cur_env = s7_curlet(sc);~%~%")
(hay "#if !GLIB_CHECK_VERSION(2,35,0)~%")
(hay "  g_type_init();~%")
(hay "#endif~%")
(for-each 
 (lambda (val) 
   (hay "  s7_define(sc, cur_env, s7_make_symbol(sc, \"~A\"), s7_make_integer(sc, ~A));~%" val val))
 (reverse ints))
(for-each
 (lambda (ints-list with-ints)
   (if (pair? ints-list)
       (with-ints hay (lambda () 
			(for-each (lambda (val)
				    (hay "  s7_define(sc, cur_env, s7_make_symbol(sc, \"~A\"), s7_make_integer(sc, ~A));~%" val val))
				  (reverse ints-list))))))
 all-ints all-int-withs)
(hay "}~%~%")
(hay "static void define_doubles(s7_scheme *sc)~%")
(hay "{~%")
(hay "  s7_pointer cur_env;~%")
(hay "  cur_env = s7_curlet(sc);~%~%")
(for-each
 (lambda (val)
   (hay "  s7_define(sc, cur_env, s7_make_symbol(sc, \"~A\"), s7_make_real(sc, ~A));~%" val val))
 (reverse dbls))
(hay "}~%~%")
;;; ----------------


(hey "/* -------------------------------- predefined Atoms -------------------------------- */~%")
(hey "~%")
(hey "static void define_atoms(void)~%")
(hey "{~%")
(hey "#define define_atom(Name) Xen_define(Xg_pre #Name Xg_post, C_to_Xen_GdkAtom(Name))~%~%")
(for-each
 (lambda (atom)
   (hey "  define_atom(~A);~%" atom))
 (reverse atoms))
(hey "}~%~%")

;;; ----------------
(hay "~%")
(hay "static void define_atoms(s7_scheme *sc)~%")
(hay "{~%")
(hay "  s7_pointer cur_env, gdkatom_symbol;~%")
(hay "  cur_env = s7_curlet(sc);~%")
(hay "  gdkatom_symbol = s7_make_symbol(sc, \"GdkAtom\");~%~%")
(for-each
 (lambda (atom)
   (hay "  s7_define(sc, cur_env, s7_make_symbol(sc, \"~A\"), s7_make_c_pointer_with_type(sc, ~A, gdkatom_symbol, lg_false));~%" atom atom))
 (reverse atoms))
(hay "}~%~%")
;;; ----------------


(hey "/* -------------------------------- symbols -------------------------------- */~%")
(hey "~%")
(hey "static void define_symbols(void)~%")
(hey "{~%")

(for-each
 (lambda (typ)
   (hey "  xg_~A_symbol = C_string_to_Xen_symbol(\"~A\");~%" (no-stars typ) (no-stars typ)))
 all-types)
(for-each
 (lambda (typ)
   (hey "  xg_~A_symbol = C_string_to_Xen_symbol(\"~A\");~%" typ typ))
 other-types)
(hey "}~%~%")

;;; ----------------
(hay "~%")
(hay "static void define_symbols(s7_scheme *sc)~%")
(hay "{~%")
(for-each
 (lambda (typ)
   (let ((typsym (hash-table-ref c->s7 (no-stars typ))))
     (if (memq typsym '(s7_make_c_pointer #f))
	 (hay "  ~A_sym = s7_make_symbol(sc, \"~A\");~%" (no-stars typ) (no-stars typ)))))
 all-types)
(hay "}~%~%")
;;; ----------------


(hey "/* -------------------------------- strings -------------------------------- */~%")
(hey "~%")
(hey "static void define_strings(void)~%")
(hey "{~%")
(hey "  ~%")
(hey "#define define_string(Name) Xen_define(Xg_pre #Name Xg_post, C_string_to_Xen_string(Name))~%")

(for-each (lambda (str) (hey "  define_string(~A);~%" str)) (reverse strings))
(for-each
 (lambda (strings-list with-strings)
   (if (pair? strings-list)
       (with-strings hey (lambda () 
			   (for-each (lambda (str) 
				       (hey "  define_string(~A);~%" str)) 
				     (reverse strings-list))))))
 all-strings all-string-withs)

(hey "}~%~%")

;;; ----------------
(hay "~%")
(hay "static void define_strings(s7_scheme *sc)~%")
(hay "{~%")
(hay "  s7_pointer cur_env;~%")
(hay "  cur_env = s7_curlet(sc);~%~%")
(for-each (lambda (str) 
	    (hay "  s7_define(sc, cur_env, s7_make_symbol(sc, \"~A\"), s7_make_string(sc, ~A));~%" str str))
(reverse strings))
(for-each
 (lambda (strings-list with-strings)
   (if (pair? strings-list)
       (with-strings hay (lambda () 
			   (for-each (lambda (str) 
				       (hay "  s7_define(sc, cur_env, s7_make_symbol(sc, \"~A\"), s7_make_string(sc, ~A));~%" str str))
				     (reverse strings-list))))))
 all-strings all-string-withs)
(hay "}~%~%")
;;; ----------------

(hey "/* -------------------------------- lint -------------------------------- */~%")
(hey "~%")
(hey "#if HAVE_SCHEME~%")
(hoy "typedef struct {const char *name, *type; int64_t value;} enummer_t;~%")
(hoy "static enummer_t enum_info[] = {~%")
(set! declared-names (sort! declared-names (lambda (a b)
					     (string<? (caddr a) (caddr b)))))
(do ((version "")
     (names declared-names (cdr names)))
    ((null? names)
     (hoy "#endif~%~NC{NULL, NULL, 0}};~%~%" 8 #\space)) ; end marker
  (unless (string=? (caddar names) version)
    (if (> (length version) 0)
	(hoy "#endif~%"))
    (set! version (caddar names))
    (hoy (string-append "#if GTK_CHECK_VERSION(" (substring version 0 1) ", " (substring version 2) ", 0)~%")))
  (hoy "~NC{~S, ~S, ~A},~%" 8 #\space (caar names) (cadar names) (caar names)))

(hoy "static s7_pointer enum_value_to_name(s7_scheme *sc, int64_t val, const char *type) ~%")
(hoy "{ ~%")
(hoy "  int k; ~%")
(hoy "  int64_t range_min = 0, range_max = 0; ~%")
(hoy "  bool range_set = false; ~%")
(hoy "  for (k = 0; ; k++) ~%")
(hoy "    { ~%")
(hoy "      enummer_t nt; ~%")
(hoy "      nt = enum_info[k]; ~%")
(hoy "      if (!nt.name) ~%")
(hoy "	break; ~%")
(hoy "      if (strcmp(nt.type, type) == 0) ~%")
(hoy "	{ ~%")
(hoy "	  if (nt.value == val)        /* ... value should be <nt.name> */ ~%")
(hoy "	    return(s7_make_string(sc, nt.name)); ~%")
(hoy "	  if (!range_set) ~%")
(hoy "	    { ~%")
(hoy "	      range_min = nt.value; ~%")
(hoy "	      range_max = nt.value; ~%")
(hoy "	      range_set = true; ~%")
(hoy "	    } ~%")
(hoy "	  else ~%")
(hoy "	    { ~%")
(hoy "	      if (range_min > nt.value) range_min = nt.value; ~%")
(hoy "	      if (range_max < nt.value) range_max = nt.value; ~%")
(hoy "	    } ~%")
(hoy "	} ~%")
(hoy "    } ~%")
(hoy "  if (range_set)       /* here we found a matching name, its type is wrong, and it's out of range */ ~%")
(hoy "    { ~%")
(hoy "      char *range_string; ~%")
(hoy "      s7_pointer str; ~%")
(hoy "      range_string = (char *)malloc(256 * sizeof(char)); ~%")
(hoy "      snprintf(range_string, 256, \"between %\" PRId64 \" and %\" PRId64, range_min, range_max); ~%")
(hoy "      str = s7_make_string(sc, range_string); ~%")
(hoy "      free(range_string); ~%")
(hoy "      return(str);                            /* ... value should be between <min> and <max> */ ~%")
(hoy "    } ~%")
(hoy "  return(s7_make_symbol(sc, \"integer?\")); ~%")
(hoy "}~%~%")

(hoy "static s7_pointer g_gtk_enum_t(s7_scheme *sc, s7_pointer args) ~%")
(hoy "{ ~%")
(hoy "  s7_pointer form, argn, func, arg; ~%")
(hoy "  const char *doc_string, *p; ~%")
(hoy "  int arg_number; ~%")
(hoy "  form = s7_car(args);  ~%")
(hoy "  argn = s7_cadr(args); ~%")
(hoy "  arg_number = s7_integer(argn); ~%")
(hoy "  arg = s7_list_ref(sc, form, arg_number); ~%")
(hoy "  if ((!s7_is_integer(arg)) && ~%")
(hoy "      (!s7_is_symbol(arg))) ~%")
(hoy "    return(s7_make_symbol(sc, \"integer?\")); ~%")
(hoy "  func = s7_car(form); ~%")
(hoy "  doc_string = s7_documentation(sc, func); ~%")
(hoy "  p = strchr(doc_string, (int)'('); ~%")
(hoy "  if (p) ~%")
(hoy "    { ~%")
(hoy "      int i; ~%")
(hoy "      for (i = 1; i < arg_number; i++) ~%")
(hoy "	p = strchr((char *)(p + 1), (int)','); ~%")
(hoy "      if (p) ~%")
(hoy "	{ ~%")
(hoy "	  const char *e; ~%")
(hoy "	  p += 2; /* past comma and space */ ~%")
(hoy "	  e = strchr(p, (int)' '); ~%")
(hoy "	  if (e) ~%")
(hoy "	    { ~%")
(hoy "	      int len; ~%")
(hoy "	      char *type; ~%")
(hoy "	      len = e - p + 1; ~%")
(hoy "	      type = (char *)malloc(len * sizeof(char)); ~%")
(hoy "	      for (i = 0; i < len; i++) type[i] = p[i]; ~%")
(hoy "	      type[len - 1] = '\\0'; ~%")
(hoy "	      if (s7_is_symbol(arg)) ~%")
(hoy "		{ ~%")
(hoy "		  const char *arg_name; ~%")
(hoy "		  arg_name = s7_symbol_name(arg); /* no free */ ~%")
(hoy "		  for (i = 0; ; i++) ~%")
(hoy "		    { ~%")
(hoy "		      enummer_t et; ~%")
(hoy "		      et = enum_info[i]; ~%")
(hoy "		      if (!et.name) ~%")
(hoy "			break; ~%")
(hoy "		      if (strcmp(et.name, arg_name) == 0) ~%")
(hoy "			{ ~%")
(hoy "			  if (strcmp(et.type, type) == 0)                 /* success -- name and type match */ ~%")
(hoy "			    { ~%")
(hoy "			      free(type); ~%")
(hoy "			      return(s7_t(sc)); ~%")
(hoy "			    } ~%")
(hoy "			  return(enum_value_to_name(sc, et.value, type)); /* here the type is wrong, so try to find the correct name */ ~%")
(hoy "			} ~%")
(hoy "		    } ~%")
(hoy "		  return(s7_make_symbol(sc, \"integer?\"));               /* here we got no matches, so return 'integer? */ ~%")
(hoy "		} ~%")
(hoy "	      return(enum_value_to_name(sc, s7_integer(arg), type));      /* here arg is an integer */ ~%")
(hoy "	    } ~%")
(hoy "	} ~%")
(hoy "    } ~%")
(hoy "  return(s7_make_symbol(sc, \"integer?\")); ~%")
(hoy "}~%~%")

(hey "static void define_lint(void)~%")
(hey "{~%")
(hey "  s7_define_safe_function(s7, \"gtk_enum_t?\", g_gtk_enum_t, 2, 0, 0, \"lint helper\");~%")
(hey "}~%")
(hey "#endif~%")
(hey "~%~%")

;;; ----------------
(hay "static void define_lint(s7_scheme *sc)~%")
(hay "{~%")
(hay "  s7_define_safe_function(sc, \"gtk_enum_t?\", g_gtk_enum_t, 2, 0, 0, \"lint helper\");~%")
(hay "}~%~%")
;;; ----------------


(hey "/* -------------------------------- initialization -------------------------------- */~%~%")
(hey "static bool xg_already_inited = false;~%~%")
(hey "#if HAVE_SCHEME~%")
(hey "void Init_libxg(s7_scheme *sc);~%")
(hey "void Init_libxg(s7_scheme *sc)~%")
(hey "#else~%")
(hey "void Init_libxg(void);~%")
(hey "void Init_libxg(void)~%")
(hey "#endif~%")
(hey "{~%")
(hey "  if (!xg_already_inited)~%")
(hey "    {~%")
(hey " #if HAVE_SCHEME~%")
(hey "      s7_xen_initialize(sc);~%")
(hey " #endif~%")
(hey "      define_symbols();~%")
(hey "      define_xm_obj();~%")
(hey "      define_integers();~%")
(hey "      define_doubles();~%")
(hey "      define_functions();~%")
(hey "      define_atoms();~%")
(hey "      define_strings();~%")
(hey "      define_structs();~%")
(hey " #if HAVE_SCHEME~%")
(hey "      define_lint();~%")
(hey " #endif~%")
(hey "      Xen_provide_feature(\"xg\");~%")
(hey "      #if GTK_CHECK_VERSION(3, 90, 0)~%")
(hey "        Xen_provide_feature(\"gtk4\");~%")
(hey "      #else~%")
(hey "        #if GTK_CHECK_VERSION(3, 0, 0)~%")
(hey "          Xen_provide_feature(\"gtk3\");~%")
(hey "        #else~%")
(hey "          Xen_provide_feature(\"gtk2\");~%")
(hey "        #endif~%")
(hey "      #endif~%")
(hey "      Xen_define(\"xg-version\", C_string_to_Xen_string(\"~A\"));~%" (strftime "%d-%b-%y" (localtime (current-time))))
(hey "      xg_already_inited = true;~%")
(hey "#if HAVE_SCHEME~%")
(hey "#if USE_SND~%")
(hey "      /* these are macros in glib/gobject/gsignal.h, but we want the types handled in some convenient way in the extension language */~%")
(hey "      s7_define(s7, s7_nil(s7), s7_make_symbol(s7, \"g_signal_connect\"),\n         Xen_eval_C_string(\"(lambda (obj name func . data)\\\n           ((*gtk* 'g_signal_connect_data) ((*gtk* 'GPOINTER) obj) name func (and (pair? data) (car data)) #f 0))\"));~%")
(hey "      s7_define(s7, s7_nil(s7), s7_make_symbol(s7, \"g_signal_connect_after\"),\n         Xen_eval_C_string(\"(lambda (obj name func . data)\\\n           ((*gtk* 'g_signal_connect_data) ((*gtk* 'GPOINTER) obj) name func (and (pair? data) (car data)) #f (*gtk* 'G_CONNECT_AFTER)))\"));~%")
(hey "      s7_define(s7, s7_nil(s7), s7_make_symbol(s7, \"g_signal_connect_swapped\"),\n         Xen_eval_C_string(\"(lambda (obj name func . data)\\\n           ((*gtk* 'g_signal_connect_data) ((*gtk* 'GPOINTER) obj) name func (and (pair? data) (car data)) #f (*gtk* 'G_CONNECT_SWAPPED)))\"));~%")
(hey "#else~%")
(hey "      Xen_eval_C_string(\"(define (g_signal_connect obj name func . data) (g_signal_connect_data (GPOINTER obj) name func (and (pair? data) (car data)) #f 0))\");~%")
(hey "      Xen_eval_C_string(\"(define (g_signal_connect_after obj name func . data) (g_signal_connect_data (GPOINTER obj) name func (and (pair? data) (car data)) #f G_CONNECT_AFTER))\");~%")
(hey "      Xen_eval_C_string(\"(define (g_signal_connect_swapped obj name func . data) (g_signal_connect_data (GPOINTER obj) name func (and (pair? data) (car data)) #f G_CONNECT_SWAPPED))\");~%")
(hey "#endif~%")
(hey "#endif~%")
(hey "#if HAVE_RUBY ~%")
(hey "      Xen_eval_C_string(\"def Rg_signal_connect(obj, name, func, data = false); Rg_signal_connect_data(RGPOINTER(obj), name, func, data, false, 0); end\"); ~%")
(hey "      Xen_eval_C_string(\"def Rg_signal_connect_after(obj, name, func, data = false); Rg_signal_connect_data(RGPOINTER(obj), name, func, data, false, RG_CONNECT_AFTER); end\"); ~%")
(hey "      Xen_eval_C_string(\"def Rg_signal_connect_swapped(obj, name, func, data = false); Rg_signal_connect_data(RGPOINTER(obj), name, func, data, false, RG_CONNECT_SWAPPED); end\"); ~%")
(hey "#endif ~%")
(hey "#if HAVE_FORTH ~%")
(hey "      Xen_eval_C_string(\": Fg_signal_connect <{ obj name func :optional data #f -- n }> obj FGPOINTER name func data #f 0 Fg_signal_connect_data ;\"); ~%")
(hey "      Xen_eval_C_string(\": Fg_signal_connect_after <{ obj name func :optional data #f -- n }> obj FGPOINTER name func data #f FG_CONNECT_AFTER Fg_signal_connect_data ;\"); ~%")
(hey "      Xen_eval_C_string(\": Fg_signal_connect_swapped <{ obj name func :optional data #f -- n }> obj FGPOINTER name func data #f FG_CONNECT_SWAPPED Fg_signal_connect_data ;\"); ~%")
(hey "#endif ~%")
(hey "    }~%")
(hey "}~%")
(hey "#else~%")
(hey " void Init_libxg(void);~%")
(hey " void Init_libxg(void)~%")
(hey "{~%")
(hey "}~%")
(hey "#endif~%") ; have_extension_language

;;; ----------------
(hay "void libgtk_s7_init(s7_scheme *sc);~%")
(hay "void libgtk_s7_init(s7_scheme *sc)~%")
(hay "{~%")
(hay "  s7_pointer cur_env;~%")
(hay "  cur_env = s7_curlet(sc);~%~%")
(hay "  cbsc = sc;~%")
(hay "  lg_true = s7_t(sc);~%")
(hay "  lg_false = s7_f(sc);~%")
(hay "  define_xm_obj(sc);~%")
(hay "  define_integers(sc);~%")
(hay "  define_doubles(sc);~%")
(hay "  define_atoms(sc);~%")
(hay "  define_strings(sc);~%")
(hay "  define_symbols(sc);~%")
(hay "  define_lint(sc);~%")
(hay "  define_structs(sc);~%")
(hay "  define_functions(sc);~%")
(hay "  s7_define_function(sc, \"g_signal_connect\", lg_g_signal_connect, 3, 1, 0, H_g_signal_connect);~%")

;;; structs
;;; xm_obj?  used to free the struct in GC?
;;; signals?

(hay "~%")
(hay "  s7_provide(sc, \"libgtk\");~%")
(hay "  #if GTK_CHECK_VERSION(3, 90, 0)~%")
(hay "    s7_provide(sc, \"gtk4\");~%")
(hay "  #else~%")
(hay "    #if GTK_CHECK_VERSION(3, 0, 0)~%")
(hay "      s7_provide(sc, \"gtk3\");~%")
(hay "    #else~%")
(hay "      s7_provide(sc, \"gtk2\");~%")
(hay "    #endif~%")
(hay "  #endif~%")
(hay "  s7_define(sc, cur_env, s7_make_symbol(sc, \"libgtk-version\"), s7_make_string(sc, \"~A\"));~%" (strftime "%d-%b-%y" (localtime (current-time))))
(hay "}~%")

(hay "/* gcc -c libgtk_s7.c -o libgtk_s7.o -I. -fPIC `pkg-config --libs gtk+-3.0 --cflags` -lm -ldl */~%")
(hay "/* gcc libgtk_s7.o -shared -o libgtk_s7.so */~%")
(hay "/* (load \"libgtk_s7.so\" (define *gtk* (inlet 'init_func 'libgtk_s7_init))) */~%")
;;; ----------------

(close-output-port xg-file)
(close-output-port lib-file)

;(format *stderr* "declared types: ~A ~A~%" (length declared-types) (length declared-names)) 157 1288
#|
(for-each
 (lambda (type)
   (if (not (assoc type direct-types))
       (format *stderr* ";not direct: ~A~%" type)))
 declared-types)

(format *stderr* "~%")
(for-each
 (lambda (v)
   (if (not (member (car v) declared-types))
       (format *stderr* "~A~%" (car v))))
 direct-types)
|#

(exit)

;;; gcc -c libgtk_s7.c -o libgtk_s7.o -g3 -I. -fPIC `pkg-config --libs gtk+-3.0 --cflags` -lm -ldl
;;; gcc libgtk_s7.o -shared -o libgtk_s7.so
