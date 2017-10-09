;;; inf-snd.el -- Inferior Snd Process (Ruby/Scheme/Forth)

;; Copyright (c) 2002--2010 Michael Scholz <mi-scholz@users.sourceforge.net>
;; All rights reserved.
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Commentary:

;; This file defines a snd-in-a-buffer package built on top of
;; comint-mode.  It includes inferior mode for Snd-Ruby
;; (inf-snd-ruby-mode), Snd-Scheme (inf-snd-scheme-mode) and Snd-Forth
;; (inf-snd-forth-mode), furthermore a Snd-Ruby mode (snd-ruby-mode),
;; a Snd-Scheme mode (snd-scheme-mode) and a Snd-Forth mode
;; (snd-forth-mode) for editing source files.  It is tested with
;; Snd-Ruby, Snd-Scheme and Snd-Forth 10.3 and GNU Emacs 22.3.1.

;; Since this mode is built on top of the general command-interpreter-
;; in-a-buffer mode (comint-mode), it shares a common base
;; functionality, and a common set of bindings, with all modes derived
;; from comint mode.  This makes these modes easier to use.  For
;; documentation on the functionality provided by comint-mode, and the
;; hooks available for customizing it, see the file comint.el.

;; A nice feature may be the commands `inf-snd-help' and `snd-help',
;; which shows the description which Snd provides for many functions.
;; With tab-completion in the minibuffer you can scan all functions at
;; a glance.  It should be easy to extent this mode with new commands
;; and key bindings; the example below and the code in this file may
;; show the way.

;; There exist six main modes in this file: the three inferior
;; Snd-process-modes (inf-snd-ruby-mode, inf-snd-scheme-mode and
;; inf-snd-forth-mode) and the replacements of ruby-mode
;; (snd-ruby-mode), of scheme-mode (snd-scheme-mode) and of
;; gforth-mode (snd-forth-mode).

;; Variables of the inferior Snd-process-modes
;; inf-snd-ruby|scheme|forth-mode (defaults):
;;
;; inf-snd-scheme-program-name "snd-s7"      Snd-Scheme program name
;; inf-snd-ruby-program-name   "snd-ruby"    Snd-Ruby program name
;; inf-snd-forth-program-name  "snd-fth"     Snd-Forth program name
;; inf-snd-working-directory   "~/"          where Ruby, Scheme or Forth scripts reside
;; inf-snd-ruby-mode-hook      nil           to customize inf-snd-ruby-mode
;; inf-snd-scheme-mode-hook    nil           to customize inf-snd-scheme-mode
;; inf-snd-forth-mode-hook     nil           to customize inf-snd-forth-mode
;; inf-snd-ruby-quit-hook      nil           to reset snd variables before exit
;; inf-snd-scheme-quit-hook    nil           to reset snd variables before exit
;; inf-snd-forth-quit-hook     nil           to reset snd variables before exit
;; inf-snd-index-path          "~/"          path to snd-xref.c
;; inf-snd-prompt              ">"           listener prompt

;; Variables of the editing modes snd-ruby|scheme|forth-mode
;; (defaults):
;;
;; snd-scheme-mode-hook        nil     	     to customize snd-scheme-mode
;; snd-ruby-mode-hook          nil     	     to customize snd-ruby-mode
;; snd-forth-mode-hook         nil     	     to customize snd-forth-mode

;; You can start inf-snd-ruby-mode interactive either with prefix-key
;; (C-u M-x run-snd-ruby)--you will be ask for program name and
;; optional arguments--or direct (M-x run-snd-ruby).  In the latter
;; case, variable inf-snd-ruby-program-name should be set correctly.
;; The same usage goes for inf-snd-scheme-mode and inf-snd-forth-mode.

;; Example for your .emacs file:
;;
;; (autoload 'run-snd-scheme   "inf-snd" "Start inferior Snd-Scheme process" t)
;; (autoload 'run-snd-ruby     "inf-snd" "Start inferior Snd-Ruby process" t)
;; (autoload 'run-snd-forth    "inf-snd" "Start inferior Snd-Forth process" t)
;; (autoload 'snd-scheme-mode  "inf-snd" "Load snd-scheme-mode." t)
;; (autoload 'snd-ruby-mode    "inf-snd" "Load snd-ruby-mode." t)
;; (autoload 'snd-forth-mode   "inf-snd" "Load snd-forth-mode." t)
;;
;; ;; These variables should be set to your needs!
;; (setq inf-snd-scheme-program-name "snd-s7 -notehook")
;; (setq inf-snd-ruby-program-name "snd-ruby -notebook")
;; (setq inf-snd-forth-program-name "snd-forth")
;; (setq inf-snd-working-directory "~/Snd/")
;; (setq inf-snd-index-path "~/Snd/snd/")

;; The hook-variables may be used to set new key bindings and menu
;; entries etc. in your .emacs file, e.g.:
;;
;; (defun snd-sounds ()
;;   (interactive)
;;   (inf-snd-send-string "(sounds)"))
;;
;; (add-hook 'inf-snd-ruby-mode-hook
;; 	  '(lambda ()
;; 	    (define-key (current-local-map) [menu-bar inf-snd-ruby-mode foo]
;; 	      '("Sounds" . snd-sounds))
;; 	    (define-key (current-local-map) "\C-c\C-t" 'snd-sounds)))
;;
;; To edit source code with special key bindings:
;;
;; (add-hook 'snd-ruby-mode-hook
;; 	  '(lambda ()
;;	    (define-key (current-local-map) "\C-co" 'snd-send-buffer)
;; 	    (define-key (current-local-map) "\C-cb" 'snd-send-block)
;; 	    (define-key (current-local-map) "\C-cr" 'snd-send-region)
;; 	    (define-key (current-local-map) "\C-ce" 'snd-send-definition)))
;;
;; (add-hook 'snd-scheme-mode-hook
;; 	  '(lambda ()
;;	    (define-key (current-local-map) "\C-co" 'snd-send-buffer)
;; 	    (define-key (current-local-map) "\C-cr" 'snd-send-region)
;; 	    (define-key (current-local-map) "\C-ce" 'snd-send-definition)))
;; 
;; (add-hook 'snd-forth-mode-hook
;; 	  '(lambda ()
;; 	    (define-key (current-local-map) "\C-co" 'snd-send-buffer)
;; 	    (define-key (current-local-map) "\C-cr" 'snd-send-region)
;; 	    (define-key (current-local-map) "\C-ce" 'snd-send-definition)))

;; You can change the mode in a source file by M-x snd-ruby-mode (or
;; snd-scheme-mode, snd-forth-mode).  To determine automatically which
;; mode to set, you can decide to use special file-extensions.  One
;; may use file-extension `.rbs' for Snd-Ruby source files and `.cms'
;; for Snd-Scheme.
;;
;; (set-default 'auto-mode-alist
;; 	     (append '(("\\.rbs$" . snd-ruby-mode)
;;                     ("\\.cms$" . snd-scheme-mode))
;; 		     auto-mode-alist))
;;
;; Or you can use the local mode variable in source files, e.g. by
;; `-*- snd-ruby -*-', `-*- snd-scheme -*-' or `-*- snd-forth -*-' in
;; first line.

;; Key bindings for inf-* and snd-*-modes
;; 
;; \e\TAB        snd-completion    symbol completion at point
;; C-h m     	 describe-mode	   describe current major mode

;; Key binding of inf-snd-ruby|scheme|forth-mode:
;;
;; C-c C-s   	 inf-snd-run-snd   (Snd-Ruby|Scheme|Forth from a dead Snd process buffer)
;; M-C-l 	 inf-snd-load      load script in current working directory
;; C-c C-f   	 inf-snd-file      open view-files-dialog of Snd
;; M-C-p 	 inf-snd-play      play current sound file
;; C-c C-t 	 inf-snd-stop      stop playing all sound files
;; C-c C-i   	 inf-snd-help      help on Snd-function (snd-help)
;; C-u C-c C-i   inf-snd-help-html help on Snd-function (html)
;; C-c C-q   	 inf-snd-quit      send exit to Snd process
;; C-c C-k   	 inf-snd-kill      kill Snd process and buffer

;; Key bindings of snd-ruby|scheme|forth-mode editing source
;; files:
;;
;; C-c C-s   	 snd-run-snd
;; M-C-x     	 snd-send-definition
;; C-x C-e   	 snd-send-last-sexp
;; C-c M-e   	 snd-send-definition
;; C-c C-e   	 snd-send-definition-and-go
;; C-c M-r   	 snd-send-region
;; C-c C-r   	 snd-send-region-and-go
;; C-c M-o   	 snd-send-buffer
;; C-c C-o   	 snd-send-buffer-and-go
;; C-c M-b   	 snd-send-block          (Ruby only)
;; C-c C-b   	 snd-send-block-and-go   (Ruby only)
;; C-c C-z   	 snd-switch-to-snd
;; C-c C-l   	 snd-load-file
;; C-u C-c C-l 	 snd-load-file-protected (Ruby only)
;;
;; and in addition:
;; 
;; C-c C-f   	 snd-file    	   open view-files-dialog of Snd
;; C-c C-p   	 snd-play    	   play current sound file
;; C-c C-t   	 snd-stop    	   stop playing all sound files
;; C-c C-i   	 snd-help    	   help on Snd-function (snd-help)
;; C-u C-c C-i   snd-help-html 	   help on Snd-function (html)
;; C-c C-q   	 snd-quit    	   send exit to Snd process
;; C-c C-k   	 snd-kill    	   kill Snd process and buffer

;;; News:
;;
;; All variables and functions containing the string `guile' are
;; renamed to `scheme'.  There are aliases for the following functions
;; and variables:
;;
;; new name                     alias for backward compatibility
;; 
;; run-snd-scheme	        run-snd-guile	       
;; snd-scheme-mode-hook	      	snd-guile-mode-hook	      
;; snd-scheme-mode	      	snd-guile-mode	      
;; inf-snd-scheme-mode	      	inf-snd-guile-mode	      
;; inf-snd-scheme-quit-hook   	inf-snd-guile-quit-hook   
;; inf-snd-scheme-program-name	inf-snd-guile-program-name

;;; Code:

;;;; The inf-snd-ruby-mode, inf-snd-scheme-mode, and inf-snd-forth-mode.

(require 'comint)
(require 'scheme)
(require 'cmuscheme)
;; (require FEATURE &optional FILENAME NOERROR)
(require 'inf-ruby   "inf-ruby"  t)
(require 'ruby-mode  "ruby-mode" t)
(require 'forth-mode "gforth"    t)

(defconst inf-snd-version "05-May-2010"
  "Version date of inf-snd.el.")

;; snd-ruby
(defvar inf-snd-ruby-buffer "*Snd-Ruby*"
  "Inferior Snd-Ruby process buffer.")

(defvar inf-snd-ruby-buffer-name "Snd-Ruby"
  "Inferior Snd-Ruby process buffer name.")

(defvar inf-snd-ruby-mode-hook nil
  "*User hook variable of `inf-snd-ruby-mode'.
Will be called after `comint-mode-hook' and before starting
inferior Snd-Ruby process.")

(defvar inf-snd-ruby-quit-hook nil
  "*User hook variable of `inf-snd-ruby-mode'.
Will be called before finishing inferior Snd-Ruby process.")

(defvar inf-snd-ruby-program-name "snd-ruby"
  "*User variable to set Snd-Ruby-program name and optional arguments.")

;; snd-forth
(defvar inf-snd-forth-buffer "*Snd-Forth*"
  "Inferior Snd-Forth process buffer.")

(defvar inf-snd-forth-buffer-name "Snd-Forth"
  "Inferior Snd-Forth process buffer name.")

(defvar inf-snd-forth-mode-hook nil
  "*User hook variable of `inf-snd-forth-mode'.
Will be called after `comint-mode-hook' and before starting
inferior Snd-Forth process.")

(defvar inf-snd-forth-quit-hook nil
  "*User hook variable of `inf-snd-forth-mode'.
Will be called before finishing inferior Snd-Forth process.")

(defvar inf-snd-forth-program-name "snd-forth"
  "*User variable to set Snd-Forth-program name and optional arguments.")

;; snd-scheme
(defvar inf-snd-scheme-buffer "*Snd-Scheme*"
  "Inferior Snd-Scheme process buffer.")

(defvar inf-snd-scheme-buffer-name "Snd-Scheme"
  "Inferior Snd-Scheme process buffer name.")

(defvar inf-snd-scheme-mode-hook nil
  "*User hook variable of `inf-snd-scheme-mode'.
Will be called after `comint-mode-hook' and before starting
inferior Snd-Scheme process.")

(defvar inf-snd-scheme-quit-hook nil
  "*User hook variable of `inf-snd-scheme-mode'.
Will be called before finishing inferior Snd-Scheme process.")

(defvar inf-snd-scheme-program-name "snd-s7"
  "*User variable to set Snd-Scheme-program name and optional args.")

(if (fboundp 'defvaralias)
    (progn
      (defvaralias 'inf-snd-guile-mode-hook    'inf-snd-scheme-mode-hook)
      (defvaralias 'inf-snd-guile-quit-hook    'inf-snd-scheme-quit-hook)
      (defvaralias 'inf-snd-guile-program-name 'inf-snd-scheme-program-name)))

;; general
(defvar snd-completions-buffer "*Completions*"
  "Snd completions buffer.")

(defvar inf-snd-prompt ">"
  "*User variable to determine Snd's listener prompt.
Example: (setq inf-snd-prompt \"snd> \")")

(defvar inf-snd-working-directory "~/"
  "*User variable where Emacs will find the Ruby, Forth, or Scheme scripts.")

(defvar inf-snd-kind nil
  "Options are 'ruby, 'forth, or 'scheme.
Needed to determine which extension language to use.  This variable is
buffer-local.")

(defvar inf-snd-comint-line-end "\n"
  "*User variable for terminating comint-send.
Interesting perhaps only for Snd-Forth.  The default '\n' should
be changed to '\n\n' with snd-forth-xm and snd-forth-xg but not
with snd-forth-nogui.  A double carriage return forces a prompt
while a single carriage return does it not in every case.")

(defvar inf-snd-index-path "~/"
  "*User variable to path where snd-xref.c is located.")

(defvar snd-send-eval-file (expand-file-name
			    (concat
			     (user-login-name) "-snd-eval-file.rb") temporary-file-directory)
  "*User variable of `inf-snd-ruby-mode' and `snd-ruby-mode'.
File where the commands will be collected before sending to
inferior Snd process.")

(defvar inf-snd-to-comment-regexp "^\\(Exception\\|undefined\\|([-A-Za-z]+)\\)"
  "*User variable of `inf-snd-ruby-mode'.
Lines with regexp will be prepended by ruby's comment sign and space '# '.")

(defvar inf-snd-ruby-keywords nil
  "Snd keywords providing online help.
\\<inf-snd-ruby-mode-map> Will be used by
`inf-snd-help' (\\[inf-snd-help], \\[universal-argument]
\\[inf-snd-help]) and `snd-help' (\\[snd-help],
\\[universal-argument] \\[snd-help]), taken from
snd/snd-xref.c.  The user variable `inf-snd-index-path' should
point to the correct path where snd-xref.c is located.")

(defvar inf-snd-scheme-keywords nil
  "Snd keywords providing online help.
\\<inf-snd-scheme-mode-map> Will be used by
`inf-snd-help' (\\[inf-snd-help], \\[universal-argument]
\\[inf-snd-help]) and `snd-help' (\\[snd-help],
\\[universal-argument] \\[snd-help]), taken from
snd/snd-xref.c.  The user variable `inf-snd-index-path' should
point to the correct path where snd-xref.c is located.")

(defvar inf-snd-ruby-keyword-regexp "^  \"\\([A-Za-z0-9$_?!()]+?\\)\"[,}]+?"
  "*User variable to find Snd-Ruby's keywords in snd-xref.c.")

(defvar inf-snd-scheme-keyword-regexp "^  \"\\([-A-Za-z0-9*>?!()]+?\\)\"[,}]+?"
  "*User variable to find Snd-Scheme's and Snd-Forth's keywords in snd-xref.c.")

(defun inf-snd-set-keywords ()
  "Set the keywords for `inf-snd-help'.
The user variable `inf-snd-index-path' should point to the
correct path of snd-xref.c to create valid keywords."
  (let ((fbuf (find-file-noselect (concat (expand-file-name inf-snd-index-path) "snd-xref.c")))
	(regex (if (eq 'ruby inf-snd-kind)
		   inf-snd-ruby-keyword-regexp
		 inf-snd-scheme-keyword-regexp))
	(keys '()))
    (with-current-buffer fbuf
      (goto-char (point-min))
      (setq case-fold-search nil)
      (while (re-search-forward regex nil t)
	(let ((val (match-string 1)))
	  (or (member val keys)
	      (setq keys (cons val keys))))))
    (kill-buffer fbuf)
    keys))

;;; from share/emacs/22.0.50/lisp/thingatpt.el
;;; lisp-complete-symbol (&optional predicate)
(defun snd-completion ()
  "Perform completion on symbols preceding point.
Compare that symbol against the known Snd symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list."
  (interactive)
  (let ((window (get-buffer-window snd-completions-buffer)))
    (if (and (eq last-command this-command)
	     window (window-live-p window) (window-buffer window)
	     (buffer-name (window-buffer window)))
	;; If this command was repeated, and
	;; there's a fresh completion window with a live buffer,
	;; and this command is repeated, scroll that window.
	(with-current-buffer (window-buffer window)
	  (if (pos-visible-in-window-p (point-max) window)
	      (set-window-start window (point-min))
	    (save-selected-window
	      (select-window window)
	      (scroll-up))))
      ;; Do completion.
      (let* ((end (point))
	     (beg (save-excursion
		    (backward-sexp 1)
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point)))
	     (pattern (buffer-substring-no-properties beg end))
	     (key-list (if (eq 'ruby inf-snd-kind) inf-snd-ruby-keywords inf-snd-scheme-keywords))
	     (completion (try-completion pattern key-list)))
	(cond ((eq completion t))
	      ((null completion)
	       (message "Can't find completion for \"%s\"" pattern))
	      ((not (string= pattern completion))
	       (delete-region beg end)
	       (insert completion))
	      (t
	       (let ((list (all-completions pattern key-list)))
		 (setq list (sort list 'string<))
		 (with-output-to-temp-buffer snd-completions-buffer
		   (display-completion-list list)))))))))

(defun inf-snd-set-keys (mode name)
  "Set the key bindings and menu entries for MODE.
Menu name is NAME.  You can extend the key bindings and menu entries
here or via hook variables in .emacs file."
  ;; key bindings
  (define-key (current-local-map) "\C-c\C-f" 'inf-snd-file)
  (define-key (current-local-map) "\M-\C-l"  'inf-snd-load)
  (define-key (current-local-map) "\M-\C-p"  'inf-snd-play)
  (define-key (current-local-map) "\C-c\C-s" 'inf-snd-run-snd)
  (define-key (current-local-map) "\C-c\C-t" 'inf-snd-stop)
  (define-key (current-local-map) "\C-c\C-i" 'inf-snd-help)
  (define-key (current-local-map) "\C-c\C-k" 'inf-snd-kill)
  (define-key (current-local-map) "\C-c\C-q" 'inf-snd-quit)
  (define-key (current-local-map) "\e\C-i"   'snd-completion)
  ;; menu entries in reverse order of appearance
  (define-key (current-local-map) [menu-bar mode]
    (cons name (make-sparse-keymap name)))
  (define-key (current-local-map) [menu-bar mode kill]
    '(menu-item "Kill Snd Process and Buffer" inf-snd-kill
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode quit]
    '(menu-item "Send exit to Snd Process" inf-snd-quit
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode start-r]
    '(menu-item "Start Snd-Ruby Process" inf-snd-run-snd
		:enable (not (inf-snd-proc-p))
		:visible (eq 'ruby inf-snd-kind)))
  (define-key (current-local-map) [menu-bar mode start-f]
    '(menu-item "Start Snd-Forth Process" inf-snd-run-snd
		:enable (not (inf-snd-proc-p))
		:visible (eq 'forth inf-snd-kind)))
  (define-key (current-local-map) [menu-bar mode start-g]
    '(menu-item "Start Snd-Scheme Process" inf-snd-run-snd
		:enable (not (inf-snd-proc-p))
		:visible (eq 'scheme inf-snd-kind)))
  (define-key (current-local-map) [menu-bar mode sep-quit] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode desc]
    '(menu-item "Describe Mode" describe-mode))
  (define-key (current-local-map) [menu-bar mode help-html]
    '(menu-item "Describe Snd Function (html) ..." inf-snd-help-html
		:enable (inf-snd-proc-p)
		:visible (not (eq 'ruby inf-snd-kind))))
  (define-key (current-local-map) [menu-bar mode help]
    '(menu-item "Describe Snd Function (snd-help) ..." inf-snd-help
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-reset] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode stop]
    '(menu-item "Stop Playing" inf-snd-stop
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode play]
    '(menu-item "Start Playing" inf-snd-play
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode file]
    '(menu-item "Open Snd-File Dialog" inf-snd-file
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-play] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode load-r]
    '(menu-item "Load Ruby Script ..." inf-snd-load
		:enable (inf-snd-proc-p)
		:visible (eq 'ruby inf-snd-kind)))
  (define-key (current-local-map) [menu-bar mode load-f]
    '(menu-item "Load Forth Script ..." inf-snd-load
		:enable (inf-snd-proc-p)
		:visible (eq 'forth inf-snd-kind)))
  (define-key (current-local-map) [menu-bar mode load-g]
    '(menu-item "Load Scheme Script ..." inf-snd-load
		:enable (inf-snd-proc-p)
		:visible (eq 'scheme inf-snd-kind))))
	  
(defun inf-snd-send-string (str &optional no-strip-p)
  "Print STR in buffer and send it to the inferior Snd process.
If NO-STRIP-P is nil, the default, all dashes (-) will be translated
to underlines (_), if `inf-snd-kind' is 'ruby.  If NO-STRIP-P is
non-nil, it won't translate.  See `inf-snd-load' for the latter case."
  (interactive)
  (and (not no-strip-p)
       (eq 'ruby inf-snd-kind)
       (while (string-match "-" str)
	 (setq str (replace-match "_" t t str))))
  (if (eq 'scheme inf-snd-kind)
      (setq str (concat "(" str ")")))
  (with-current-buffer (inf-snd-proc-buffer)
    (insert str)
    (comint-send-input)))

(defun inf-snd-run-snd ()
  "Start inferior Snd-Ruby, Snd-Forth, or Snd-Scheme process.
Started from dead Snd process buffer."
  (interactive)
  (cond ((eq 'ruby inf-snd-kind)
	 (run-snd-ruby inf-snd-ruby-program-name))
	((eq 'forth inf-snd-kind)
	 (run-snd-forth inf-snd-forth-program-name))
	(t
	 (run-snd-scheme inf-snd-scheme-program-name))))

(defun inf-snd-file ()
  "Open Snd's view-files-dialog widget."
  (interactive)
  (inf-snd-send-string "view-files-dialog"))

(defun inf-snd-load (file)
  "Load the required Ruby, Forth, or Scheme script.
Asks for FILE interactively in minibuffer."
  (interactive "fLoad Snd Script: ")
  (unless (file-directory-p file)
    (inf-snd-send-string
     (if (eq 'forth inf-snd-kind)
	 (format "include %s" (car (file-expand-wildcards file t)))
       (format "load %S" (car (file-expand-wildcards file t)))) t)))

(defun inf-snd-play ()
  "Play current sound."
  (interactive)
  (inf-snd-send-string "play"))

(defun inf-snd-stop ()
  "Stop playing of all sound files."
  (interactive)
  (inf-snd-send-string "stop-playing"))

(defun inf-snd-help (&optional html-help)
  "Receive a string in minibuffer and show corresponding help.
\\<inf-snd-ruby-mode-map>\\<inf-snd-forth-mode-map>\\<inf-snd-scheme-mode-map>
This is done via Snd's function snd_help() or html() if HTML-HELP
is non-nil, i.e. it's called by \\[universal-argument]
\\[inf-snd-help], putting result at the end of the inferior Snd
process buffer.  If point is near a function name in inferior Snd
process buffer, that function will be used as default value in
minibuffer; tab-completion is activated.  `inf-snd-ruby-keywords'
and `inf-snd-scheme-keywords' hold the help strings, the user
variable `inf-snd-index-path' should point to the correct path of
snd-xref.c."
  (interactive "P")
  (let ((prompt (format "Snd%s Help: " (if html-help " HTML" "")))
	(default (thing-at-point 'sexp)))
    (if default
	(setq prompt (format "%s(default %s): " prompt default)))
    (let ((str (completing-read prompt
				(if (eq 'ruby inf-snd-kind)
				    inf-snd-ruby-keywords
				  inf-snd-scheme-keywords)
				nil nil nil nil default)))
      (unless (string= str "")
	(unless html-help
	  (while (string-match " " str)
	    (setq str (replace-match "" t t str))))
	(let ((inf-str (if (and html-help
				(not (eq 'forth inf-snd-kind)))
			   (format "(html \"%s\")" str)
			 (cond ((eq 'ruby inf-snd-kind)
				(format "Snd.display(snd_help(\"%s\", true))" str))
			       ((eq 'forth inf-snd-kind)
				(format "\"%s\" #t snd-help" str))
			       (t
				(format "snd-help \"%s\" #t" str))))))
	  (with-current-buffer (inf-snd-proc-buffer)
	    (goto-char (point-max))
	    (if (and (string= (char-to-string (preceding-char)) inf-snd-prompt)
		     (eobp))
		(inf-snd-send-string inf-str t)
	      (beginning-of-line)
	      (kill-region (point) (point-max))
	      (inf-snd-send-string inf-str t)
	      (yank))))))))

(defun inf-snd-help-html ()
  "Start html help."
  (interactive)
  (inf-snd-help t))

(defun inf-snd-quit ()
  "Send exit to inferior Snd process."
  (interactive)
  (cond ((eq 'ruby inf-snd-kind)
	 (run-hooks 'inf-snd-ruby-quit-hook))
	((eq 'forth inf-snd-kind)
	 (run-hooks 'inf-snd-forth-quit-hook))
	(t
	 (run-hooks 'inf-snd-scheme-quit-hook)))
  (if (bufferp snd-completions-buffer)
      (kill-buffer snd-completions-buffer))
  (get-buffer-process (inf-snd-proc-buffer))
  (goto-char (point-max))
  (cond ((eq 'ruby inf-snd-kind)
	 (snd-send-invisible "exit(0)"))
	((eq 'forth inf-snd-kind)
	 (snd-send-invisible "0 snd-exit drop"))
	(t
	 (snd-send-invisible "(exit 0)")))
  (and (file-exists-p snd-send-eval-file)
       (delete-file snd-send-eval-file)))

(defun inf-snd-kill ()
  "Kill current inferior Snd process and buffer."
  (interactive)
  (inf-snd-quit)
  (delete-process (get-buffer-process (inf-snd-proc-buffer)))
  (kill-buffer (current-buffer))
  (unless (one-window-p)
    (delete-window (get-buffer-window (inf-snd-proc-buffer)))))

(defun inf-snd-proc-buffer ()
  "Return the current process buffer."
  (cond ((eq 'ruby inf-snd-kind)
	 inf-snd-ruby-buffer)
	((eq 'forth inf-snd-kind)
	 inf-snd-forth-buffer)
	(t
	 inf-snd-scheme-buffer)))

(defun inf-snd-proc-p ()
  "Return non-nil if process buffer is available."
  (save-current-buffer
    (comint-check-proc (inf-snd-proc-buffer))))

(defun snd-send-invisible (str &optional no-newline)
  "Send a STR to the process running in the current buffer.
Non-nil NO-NEWLINE means string without carriage return append."
  (let ((proc (get-buffer-process (current-buffer))))
    (cond ((not proc)
	   (error "Current buffer has no process"))
	  ((stringp str)
	   (comint-snapshot-last-prompt)
	   (if no-newline
	       (comint-send-string proc str)
	     (comint-simple-send proc str))))))
  
(defun inf-snd-comint-put-prompt-ruby (string)
  "Look for `inf-snd-to-comment-regexp' in STRING in the current output.
Prepends matching lines with ruby's comment sign and space `# '.
Showing a prompt is forced by run_emacs_eval_hook() in
snd/examp.rb.  This function could be on the so called abnormal
hook with one arg `comint-preoutput-filter-functions'."
  ;; Drop trailing '\n' ("...snd(0)> \n" => "...snd(0)> ").
  (if (string-match inf-snd-prompt string)
      (setq string (substring string 0 (match-end 0))))
  (while (string-match inf-snd-to-comment-regexp string)
    (setq string (replace-match "# \\1" t nil string 1)))
  string)
  
(defun inf-snd-comint-put-prompt-forth (string)
  "If STRING contains one or more undef strings, replace them with `inf-snd-prompt'.
This function could be on the so called abnormal hook with one
arg `comint-preoutput-filter-functions'."
  (if (string-match "\\s-?\\(\\(undef\\s-\\)+\\)$" string)
      (replace-match inf-snd-prompt t t string 1)
    string))

(defun inf-snd-comint-put-prompt-scheme (string)
  "Appends `inf-snd-prompt' to STRING.
This function could be on the so called abnormal hook with one
arg `comint-preoutput-filter-functions'."
  (if (string-match "\n" string)
      (concat string inf-snd-prompt)
    string))

(defun inf-snd-comint-snd-send (proc line)
  "Special function for sending input LINE to PROC.
Variable `comint-input-sender' is set to this function.  Running
Snd-Ruby it is necessary to load snd/examp.rb in your ~/.snd file
which contains run_emacs_eval_hook(line).  inf-snd.el uses this
function to evaluate one line or multi-line input (Ruby only)."
  (if (= (length line) 0)
      (if (eq 'scheme inf-snd-kind)
	  (setq line "#f")
	(setq line "nil")))
  (comint-send-string proc (if (eq 'ruby inf-snd-kind)
			       (format "run_emacs_eval_hook(%%(%s))\n" line)
			     (concat line inf-snd-comint-line-end))))

(defun inf-snd-get-old-input ()
  "Snarf the whole pointed line."
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (buffer-substring-no-properties (point) end))))

(defun inf-snd-args-to-list (string)
  "Return a list containing the program and optional arguments list.
Argument STRING is the Snd command and optional arguments."
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (inf-snd-args-to-list (substring string (+ 1 where) (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (inf-snd-args-to-list (substring string pos
						 (length string)))))))))

(define-derived-mode inf-snd-ruby-mode comint-mode inf-snd-ruby-buffer-name
  "Inferior mode running Snd-Ruby, derived from `comint-mode'.

Snd is a sound editor created by Bill Schottstaedt
\(bil@ccrma.Stanford.EDU).  You can find it on
ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-7.tar.gz.

You can type in Ruby commands in inferior Snd process buffer which
will be sent via `comint-send-string' to the inferior Snd process.
The return value will be shown in the process buffer, other output
goes to the listener of Snd.

You should set variable `inf-snd-ruby-program-name' and
`inf-snd-working-directory' in your .emacs file to set the appropriate
program name and optional arguments and to direct Snd to the Ruby
scripts directory, you have.

The hook variables `comint-mode-hook' and
`inf-snd-ruby-mode-hook' will be called in that special order
after calling the inferior Snd process.  You can use them e.g. to
set additional key bindings.  The hook variable
`inf-snd-ruby-quit-hook' will be called before finishing the
inferior Snd process.  You may use it for resetting Snd
variables, e.g. the listener prompt.

\\<inf-snd-ruby-mode-map> Interactive start is possible either by
\\[universal-argument] \\[run-snd-ruby], you will be ask for the Snd
program name, or by \\[run-snd-ruby].  Emacs shows an additional menu
entry ``Snd-Ruby'' in the menu bar.

The following key bindings are defined:
\\{inf-snd-ruby-mode-map}"
  (ruby-mode-variables)
  (add-hook 'comint-preoutput-filter-functions 'inf-snd-comint-put-prompt-ruby nil t)
  (add-hook 'comint-input-filter-functions 'ruby-input-filter nil t)
  (setq comint-get-old-input (function inf-snd-get-old-input))
  (setq comint-input-sender (function inf-snd-comint-snd-send))
  (setq default-directory inf-snd-working-directory)
  (make-local-variable 'inf-snd-prompt)
  (make-local-variable 'inf-snd-kind)
  (make-local-variable 'inf-snd-comint-line-end)
  (setq comint-prompt-regexp (concat "^\\(" inf-snd-prompt "\\)+"))
  (setq inf-snd-kind 'ruby)
  (setq mode-line-process '(":%s"))
  (unless inf-snd-ruby-keywords
    (setq inf-snd-ruby-keywords (inf-snd-set-keywords)))
  (inf-snd-set-keys 'inf-snd-ruby-mode inf-snd-ruby-buffer-name)
  (pop-to-buffer inf-snd-ruby-buffer)
  (goto-char (point-max))
  (run-hooks 'inf-snd-ruby-mode-hook))

(define-derived-mode inf-snd-forth-mode comint-mode inf-snd-forth-buffer-name
  "Inferior mode running Snd-Forth, derived from `comint-mode'.

Snd is a sound editor created by Bill Schottstaedt
\(bil@ccrma.Stanford.EDU).  You can find it on
ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-7.tar.gz.

You can type in Forth commands in inferior Snd process buffer which
will be sent via `comint-send-string' to the inferior Snd process.
The return value will be shown in the process buffer, other output
goes to the listener of Snd.

You should set variable `inf-snd-forth-program-name' and
`inf-snd-working-directory' in your .emacs file to set the appropriate
program name and optional arguments and to direct Snd to the Forth
scripts directory, you have.

The hook variables `comint-mode-hook' and
`inf-snd-forth-mode-hook' will be called in that special order
after calling the inferior Snd process.  You can use them e.g. to
set additional key bindings.  The hook variable
`inf-snd-forth-quit-hook' will be called before finishing the
inferior Snd process.  You may use it for resetting Snd
variables, e.g. the listener prompt.

\\<inf-snd-forth-mode-map> Interactive start is possible either by
\\[universal-argument] \\[run-snd-forth], you will be ask for the Snd
program name, or by \\[run-snd-forth].  Emacs shows an additional menu
entry ``Snd-Forth'' in the menu bar.

The following key bindings are defined:
\\{inf-snd-forth-mode-map}"
  (add-hook 'comint-preoutput-filter-functions 'inf-snd-comint-put-prompt-forth nil t)
  (setq comint-get-old-input (function inf-snd-get-old-input))
  (setq comint-input-sender (function inf-snd-comint-snd-send))
  (setq default-directory inf-snd-working-directory)
  (make-local-variable 'inf-snd-prompt)
  (make-local-variable 'inf-snd-kind)
  (make-local-variable 'inf-snd-comint-line-end)
  (setq comint-prompt-regexp (concat "^\\(" inf-snd-prompt "\\)+"))
  (setq inf-snd-kind 'forth)
  (setq mode-line-process '(":%s"))
  (unless inf-snd-scheme-keywords
    (setq inf-snd-scheme-keywords (inf-snd-set-keywords)))
  (inf-snd-set-keys 'inf-snd-forth-mode inf-snd-forth-buffer-name)
  (pop-to-buffer inf-snd-forth-buffer)
  (goto-char (point-max))
  (run-hooks 'inf-snd-forth-mode-hook))

(define-derived-mode inf-snd-scheme-mode comint-mode inf-snd-scheme-buffer-name
  "Inferior mode running Snd-Scheme, derived from `comint-mode'.

Snd is a sound editor created by Bill Schottstaedt
\(bil@ccrma.Stanford.EDU).  You can find it on
ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-7.tar.gz.

You can type in Scheme commands in inferior Snd process buffer which
will be sent via `comint-send-string' to the inferior Snd process.
The return value will be shown in the process buffer, other output
goes to the listener of Snd.

You sould set variable `inf-snd-scheme-program-name' and
`inf-snd-working-directory' in your .emacs file to set the appropriate
program name and optional arguments and to direct Snd to the Scheme
scripts directory, you have.

The hook variables `comint-mode-hook' and
`inf-snd-scheme-mode-hook' will be called in that special order
after calling the inferior Snd process.  You can use them e.g. to
set additional key bindings.  The hook variable
`inf-snd-scheme-quit-hook' will be called before finishing the
inferior Snd process.  You may use it for resetting Snd
variables, e.g. the listener prompt.

\\<inf-snd-scheme-mode-map> Interactive start is possible either by
\\[universal-argument] \\[run-snd-scheme], you will be ask for the Snd
program name, or by \\[run-snd-scheme].  Emacs shows an additional menu
entry ``Snd-Scheme'' in the menu bar.

The following key bindings are defined:
\\{inf-snd-scheme-mode-map}"
  (scheme-mode-variables)
  (add-hook 'comint-preoutput-filter-functions 'inf-snd-comint-put-prompt-scheme nil t)
  (add-hook 'comint-input-filter-functions 'scheme-input-filter nil t)
  (setq comint-get-old-input (function scheme-get-old-input))
  (setq comint-input-sender (function inf-snd-comint-snd-send))
  (setq default-directory inf-snd-working-directory)
  (make-local-variable 'inf-snd-prompt)
  (make-local-variable 'inf-snd-kind)
  (make-local-variable 'inf-snd-comint-line-end)
  (setq comint-prompt-regexp (concat "^\\(" inf-snd-prompt "\\)+"))
  (setq inf-snd-kind 'scheme)
  (setq mode-line-process '(":%s"))
  (unless inf-snd-scheme-keywords
    (setq inf-snd-scheme-keywords (inf-snd-set-keywords)))
  (inf-snd-set-keys 'inf-snd-scheme-mode inf-snd-scheme-buffer-name)
  (pop-to-buffer inf-snd-scheme-buffer)
  (goto-char (point-max))
  (run-hooks 'inf-snd-scheme-mode-hook))

(defalias 'inf-snd-guile-mode 'inf-snd-scheme-mode)

(defun run-snd-ruby (cmd)
  "Start inferior Snd-Ruby process.
CMD is used for determine which program to run.  If interactively
called, one will be asked for program name to run."
  (interactive (list (if current-prefix-arg
 			 (read-string "Run Snd Ruby: " inf-snd-ruby-program-name)
 		       inf-snd-ruby-program-name)))
  (unless (comint-check-proc inf-snd-ruby-buffer)
    (let ((cmdlist (inf-snd-args-to-list cmd)))
      (setq inf-snd-ruby-program-name cmd)
      (set-buffer (apply 'make-comint inf-snd-ruby-buffer-name (car cmdlist) nil (cdr cmdlist))))
    (inf-snd-ruby-mode)))

(defun run-snd-forth (cmd)
  "Start inferior Snd-Forth process.
CMD is used for determine which program to run.  If interactively
called, one will be asked for program name to run."
  (interactive (list (if current-prefix-arg
 			 (read-string "Run Snd Forth: " inf-snd-forth-program-name)
 		       inf-snd-forth-program-name)))
  (unless (comint-check-proc inf-snd-forth-buffer)
    (let ((cmdlist (inf-snd-args-to-list cmd)))
      (setq inf-snd-forth-program-name cmd)
      (set-buffer (apply 'make-comint inf-snd-forth-buffer-name (car cmdlist) nil (cdr cmdlist))))
    (inf-snd-forth-mode)
    (snd-send-invisible "undef")))	;for inf-snd-comint-put-prompt-forth

(defun run-snd-scheme (cmd)
  "Start inferior Snd-Scheme process.
CMD is used for determine which program to run.  If interactively
called, one will be asked for program name to run."
  (interactive (list (if current-prefix-arg
 			 (read-string "Run Snd Scheme: " inf-snd-scheme-program-name)
 		       inf-snd-scheme-program-name)))
  (unless (comint-check-proc inf-snd-scheme-buffer)
    (let ((cmdlist (inf-snd-args-to-list cmd)))
      (setq inf-snd-scheme-program-name cmd)
      (set-buffer (apply 'make-comint inf-snd-scheme-buffer-name (car cmdlist) nil (cdr cmdlist))))
    (inf-snd-scheme-mode)
    (snd-send-invisible "#f")))

(defalias 'run-snd-guile 'run-snd-scheme)

;;;; The snd-ruby-, snd-scheme-, and snd-forth-mode

;;; Commentary:

;; These three modes are derived from ruby-mode, scheme-mode, and
;; forth-mode.  The main changes are the key bindings, which now refer
;; to special Snd-process-buffer-related ones.  I took commands from
;; inf-ruby.el, from cmuscheme.el and from gforth.el and changed them
;; appropriately.

(defvar snd-ruby-buffer-name "Snd/Ruby"
  "Buffer name of `snd-ruby-mode'.")

(defvar snd-forth-buffer-name "Snd/Forth"
  "Buffer name of `snd-forth-mode'.")

(defvar snd-scheme-buffer-name "Snd/Scheme"
  "Buffer name of `snd-scheme-mode'.")

(defvar snd-ruby-mode-hook nil
  "User hook variable.
Called after `ruby-mode-hook' and before starting inferior Snd
process.")

(defvar snd-forth-mode-hook nil
  "*User hook variable.
Called after `forth-mode-hook' and before starting inferior Snd
process.")

(defvar snd-scheme-mode-hook nil
  "*User hook variable.
Called after `scheme-mode-hook' and before starting inferior Snd
process.")

(defalias 'snd-guile-mode-hook 'snd-scheme-mode-hook)

(defvar snd-source-modes '(snd-ruby-mode)
  "Used to determine if a buffer contains Snd source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Snd source file by `snd-load-file'.  Used by this command
to determine defaults.  This variable is buffer-local in
`snd-ruby-mode', `snd-forth-mode' and `snd-scheme-mode'.")

(defvar snd-inf-kind 'ruby
  "Options are 'ruby, 'forth, and 'scheme.
Needed to determine which extension language should be used.
This variable is buffer-local in `snd-ruby-mode',
`snd-forth-mode', and `snd-scheme-mode'.")

(defvar snd-prev-l/c-dir/file nil
  "Cache the (directory . file) pair used in the last `snd-load-file'.
Used for determining the default in the next one.")

(define-derived-mode snd-ruby-mode ruby-mode snd-ruby-buffer-name
  "Major mode for editing Snd-Ruby code.

Editing commands are similar to those of `ruby-mode'.

In addition, you can start an inferior Snd process and some
additional commands will be defined for evaluating expressions.
A menu ``Snd/Ruby'' appears in the menu bar.  Entries in this
menu are disabled if no inferior Snd process exist.

You can use the hook variables `ruby-mode-hook' and
`snd-ruby-mode-hook', which will be called in that order.

The current key bindings are:
\\{snd-ruby-mode-map}"
  (make-local-variable 'snd-inf-kind)
  (make-local-variable 'snd-source-modes)
  (setq snd-inf-kind 'ruby)
  (setq snd-source-modes '(snd-ruby-mode))
  (unless inf-snd-ruby-keywords
    (setq inf-snd-ruby-keywords (inf-snd-set-keywords)))
  (snd-set-keys 'snd-ruby-mode snd-ruby-buffer-name)
  (run-hooks 'snd-ruby-mode-hook))

(define-derived-mode snd-forth-mode forth-mode snd-forth-buffer-name
  "Major mode for editing Snd-Forth code.

Editing commands are similar to those of `forth-mode'.

In addition, you can start an inferior Snd process and some
additional commands will be defined for evaluating expressions.
A menu ``Snd/Forth'' appears in the menu bar.  Entries in this
menu are disabled if no inferior Snd process exist.

You can use the hook variables `forth-mode-hook' and
`snd-forth-mode-hook', which will be called in that order.

The current key bindings are:
\\{snd-forth-mode-map}"
  (make-local-variable 'snd-inf-kind)
  (make-local-variable 'snd-source-modes)
  (setq snd-inf-kind 'forth)
  (setq snd-source-modes '(snd-forth-mode))
  (unless inf-snd-scheme-keywords
    (setq inf-snd-scheme-keywords (inf-snd-set-keywords)))
  (snd-set-keys 'snd-forth-mode snd-forth-buffer-name)
  (run-hooks 'snd-forth-mode-hook))

(define-derived-mode snd-scheme-mode scheme-mode snd-scheme-buffer-name
  "Major mode for editing Snd-Scheme code.

Editing commands are similar to those of `scheme-mode'.

In addition, you can start an inferior Snd process and some
additional commands will be defined for evaluating expressions.
A menu ``Snd/Scheme'' appears in the menu bar.  Entries in this
menu are disabled if no inferior Snd process exist.

You can use variables `scheme-mode-hook' and
`snd-scheme-mode-hook', which will be called in that order.

The current key bindings are:
\\{snd-scheme-mode-map}"
  (make-local-variable 'snd-inf-kind)
  (make-local-variable 'snd-source-modes)
  (setq snd-inf-kind 'scheme)
  (setq snd-source-modes '(snd-scheme-mode))
  (unless inf-snd-scheme-keywords
    (setq inf-snd-scheme-keywords (inf-snd-set-keywords)))
  (snd-set-keys 'snd-scheme-mode snd-scheme-buffer-name)
  (run-hooks 'snd-scheme-mode-hook))

(defalias 'snd-guile-mode 'snd-scheme-mode)

(defun snd-send-region (start end)
  "Send the current region to the inferior Snd process.
START and END define the region."
  (interactive "r")
  (if (eq 'ruby snd-inf-kind)
      (progn
	(write-region start end snd-send-eval-file nil 0)
 	(comint-send-string
 	 (snd-proc)
 	 (format "eval(File.open(%S).read, TOPLEVEL_BINDING, \"(emacs-eval-region)\", 1).inspect \
rescue message(\"(emacs-eval-region): %%s (%%s)\\n%%s\", \
$!.message, $!.class, $!.backtrace.join(\"\\n\"))\n"
 		 snd-send-eval-file)))
    (comint-send-region (snd-proc) start end)
    (comint-send-string (snd-proc) "\n")))

(defun snd-send-region-and-go (start end)
  "Send the current region to the inferior Snd process.
Switch to the process buffer.  START and END define the region."
  (interactive "r")
  (snd-send-region start end)
  (snd-switch-to-snd t))

(defun snd-send-definition (&optional cnt)
  "Send the current or CNT definition to the inferior Snd process."
  (interactive "p")
  (save-excursion
    (if (eq 'ruby snd-inf-kind)
	(ruby-beginning-of-defun cnt)
      (beginning-of-defun cnt))
   (let ((beg (point)))
     (if (eq 'ruby snd-inf-kind)
	 (ruby-end-of-defun)
       (end-of-defun))
     (snd-send-region beg (point)))))

(defun snd-send-definition-and-go (&optional cnt)
  "Send the current or CNT definition to the inferior Snd process.
Switch to the process buffer."
  (interactive "p")
  (snd-send-definition cnt)
  (snd-switch-to-snd t))

(defun snd-send-last-sexp (&optional cnt)
  "Send the previous or CNT sexp to the inferior Snd process."
  (interactive "p")
  (snd-send-region (save-excursion
		     (if (eq 'ruby snd-inf-kind)
			 (ruby-backward-sexp cnt)
		       (backward-sexp cnt))
		     (point))
		   (point)))

(defun snd-send-block (&optional cnt)
  "Send the current or CNT block to the inferior Snd-Ruby process.
Works only in `snd-ruby-mode'."
  (interactive "p")
  (save-excursion
    (ruby-beginning-of-block cnt)
    (let ((beg (point)))
      (ruby-end-of-block)
      (end-of-line)
      (snd-send-region beg (point)))))

(defun snd-send-block-and-go (&optional cnt)
  "Send the current or CNT block to the inferior Snd-Ruby process.
Switch to the process buffer.  Works only in `snd-ruby-mode'."
  (interactive "p")
  (snd-send-block cnt)
  (snd-switch-to-snd t))

(defun snd-send-buffer ()
  "Send the current buffer to the inferior Snd process."
  (interactive)
  (snd-send-region (point-min) (point-max)))

(defun snd-send-buffer-and-go ()
  "Send the current buffer to the inferior Snd process.
Switch to the process buffer."
  (interactive)
  (snd-send-buffer)
  (snd-switch-to-snd t))

(defun snd-switch-to-snd (&optional eob-p)
  "If inferior Snd process exists, switch to process buffer, else start Snd.
Non-nil EOB-P positions cursor at end of buffer."
  (interactive "P")
  (let ((buf (snd-proc-buffer)))
    (if (get-buffer buf)
	(pop-to-buffer buf)
      (snd-run-snd))
    (if eob-p
	(push-mark)
      (goto-char (point-max)))))

(defun snd-run-snd ()
  "If inferior Snd process exists, switch to process buffer, else start Snd.
Started from `snd-ruby-mode', `snd-forth-mode' or `snd-scheme-mode'."
  (interactive)
  (if (snd-proc-p)
      (progn
	(pop-to-buffer (snd-proc-buffer))
	(push-mark)
	(goto-char (point-max)))
    (cond ((eq 'ruby snd-inf-kind)
	   (run-snd-ruby inf-snd-ruby-program-name))
	  ((eq 'forth snd-inf-kind)
	   (run-snd-forth inf-snd-forth-program-name))
	  (t
	   (run-snd-scheme inf-snd-scheme-program-name)))))

(defun snd-load-file-protected (filename)
  "Load a Ruby script FILENAME as an anonymous module into the inferior Snd process."
  (interactive (comint-get-source "Load Snd script file: "
				  snd-prev-l/c-dir/file snd-source-modes t))
  (comint-check-source filename)
  (setq snd-prev-l/c-dir/file (cons (file-name-directory filename)
				     (file-name-nondirectory filename)))
  (comint-send-string (snd-proc) (concat "load(\"" filename "\", true)\n")))

(defun snd-load-file (filename)
  "Load a Snd script FILENAME into the inferior Snd process."
  (interactive (comint-get-source "Load Snd script file: "
				  snd-prev-l/c-dir/file snd-source-modes t))
  (comint-check-source filename)
  (setq snd-prev-l/c-dir/file (cons (file-name-directory filename)
				     (file-name-nondirectory filename)))
  (if (eq 'forth snd-inf-kind)
      (comint-send-string (snd-proc) (concat "include " filename "\n"))
    (comint-send-string (snd-proc) (concat "(load \"" filename"\"\)\n"))))

;;; this from Orm Finnendahl 20-Mar-17
(defun snd-scheme-open-file (filename)
  "Open file in a running inferior Snd-Scheme process. Start the process if necessary."
  (interactive "FOpen Soundfile:")
  (if (comint-check-proc inf-snd-scheme-buffer)
      (inf-snd-send-string (format "(open-sound \"%s\")" filename))
    (progn
      (set-buffer (apply 'make-comint inf-snd-scheme-buffer-name inf-snd-scheme-program-name nil (list filename)))
      (inf-snd-scheme-mode)
      (snd-send-invisible "#f"))))

(defun snd-save-state ()
  "Synchronize the inferior Snd process with the edit buffer."
  (and (snd-proc)
       (setq inf-snd-kind snd-inf-kind)))
  
(defun snd-file ()
  "Open Snd's view-files-dialog widget."
  (interactive)
  (snd-save-state)
  (inf-snd-file))

(defun snd-play ()
  "Play current sound."
  (interactive)
  (snd-save-state)
  (inf-snd-play))

(defun snd-stop ()
  "Stop playing of all sounds."
  (interactive)
  (snd-save-state)
  (inf-snd-stop))

(defun snd-help (&optional html-help)
  "Receive a string in minibuffer and show corresponding help.
\\<inf-snd-ruby-mode-map>\\<inf-snd-forth-mode-map>\\<inf-snd-scheme-mode-map>
This is done via Snd's function snd_help() or html() if HTML-HELP
is non-nil, i.e. it's called by \\[universal-argument]
\\[snd-help], putting result at the end of the inferior Snd
process buffer.  If point is near a function name in inferior Snd
process buffer, that function will be used as default value in
minibuffer; tab-completion is activated.  `inf-snd-ruby-keywords'
and `inf-snd-scheme-keywords' hold the help strings, the user
variable `inf-snd-index-path' should point to the correct path of
snd-xref.c."
  (interactive "P")
  (snd-save-state)
  (inf-snd-help html-help))

(defun snd-help-html ()
  "Start html help."
  (interactive)
  (snd-help t))

(defun snd-quit ()
  "Send exit to current inferior Snd process."
  (interactive)
  (snd-save-state)
  (save-excursion
    (snd-switch-to-snd t)
    (inf-snd-quit)))

(defun snd-kill ()
  "Kill current inferior Snd process buffer."
  (interactive)
  (snd-save-state)
  (save-excursion
    (snd-switch-to-snd t)
    (inf-snd-kill)))

(defun snd-proc-buffer ()
  "Return the current process buffer."
  (cond ((eq 'ruby snd-inf-kind)
	 inf-snd-ruby-buffer)
	((eq 'forth snd-inf-kind)
	 inf-snd-forth-buffer)
	(t
	 inf-snd-scheme-buffer)))

(defun snd-proc ()
  "Return the process buffer."
  (let* ((buf (snd-proc-buffer))
	 (proc (get-buffer-process (if (eq major-mode
					   (cond ((eq 'ruby snd-inf-kind)
						  'inf-snd-ruby-mode)
						 ((eq 'forth snd-inf-kind)
						  'inf-snd-forth-mode)
						 ('
						  'inf-snd-scheme-mode)))
				       (current-buffer)
				     buf))))
    (or proc
	(error "No current process.  See variable inf-snd-ruby|inf-snd-forth|scheme-buffer"))))

(defun snd-proc-p ()
  "Return non-nil if no process buffer available."
  (save-current-buffer
    (comint-check-proc (snd-proc-buffer))))

(defun snd-set-keys (mode name)
  "Set the key bindings and menu entries for MODE.
Menu name is NAME.  You can extend the key bindings and menu entries
here or via hook variables in .emacs file."
  (define-key (current-local-map) "\M-\C-x"  'snd-send-definition)
  (define-key (current-local-map) "\C-x\C-e" 'snd-send-last-sexp)
  (define-key (current-local-map) "\C-c\M-e" 'snd-send-definition)
  (define-key (current-local-map) "\C-c\C-e" 'snd-send-definition-and-go)
  (define-key (current-local-map) "\C-c\M-r" 'snd-send-region)
  (define-key (current-local-map) "\C-c\C-r" 'snd-send-region-and-go)
  (define-key (current-local-map) "\C-c\M-o" 'snd-send-buffer)
  (define-key (current-local-map) "\C-c\C-o" 'snd-send-buffer-and-go)
  (define-key (current-local-map) "\C-c\C-z" 'snd-switch-to-snd)
  (define-key (current-local-map) "\C-c\C-s" 'snd-run-snd)
  (define-key (current-local-map) "\C-c\C-l" 'snd-load-file)
  (define-key (current-local-map) "\C-c\C-f" 'snd-file)
  (define-key (current-local-map) "\C-c\C-p" 'snd-play)
  (define-key (current-local-map) "\C-c\C-t" 'snd-stop)
  (define-key (current-local-map) "\C-c\C-i" 'snd-help)
  (define-key (current-local-map) "\C-c\C-k" 'snd-kill)
  (define-key (current-local-map) "\C-c\C-q" 'snd-quit)
  (define-key (current-local-map) "\e\C-i"   'snd-completion)
  (if (eq 'ruby snd-inf-kind)
      (progn
	(define-key (current-local-map) "\C-c\M-b" 'snd-send-block)
	(define-key (current-local-map) "\C-c\C-b" 'snd-send-block-and-go)
	(define-key (current-local-map) "\C-cb"    'undefined) ;overwrite inf-ruby-commands
	(define-key (current-local-map) "\C-cr"    'undefined) ;C-c + single letter key
	(define-key (current-local-map) "\C-ce"    'undefined) ;is reserved for user
	(define-key (current-local-map) "\C-c\C-x" 'undefined) ;key bindings
	(define-key (current-local-map) "\C-c\M-x" 'undefined))
    (define-key (current-local-map) "\C-c\C-c" 'undefined) ;no compile
    (define-key (current-local-map) "\C-c\M-c" 'undefined))
  (define-key (current-local-map) [menu-bar mode]
    (cons name (make-sparse-keymap name)))
  (define-key (current-local-map) [menu-bar mode kill]
    '(menu-item "Kill Snd Process and Buffer" snd-kill
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode quit]
    '(menu-item "Send exit to Snd Process" snd-quit
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-quit] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode desc]
    '(menu-item "Describe Mode" describe-mode))
  (define-key (current-local-map) [menu-bar mode help-html]
    '(menu-item "Describe Snd Function (html) ..." snd-help-html
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode help]
    '(menu-item "Describe Snd Function (snd-help) ..." snd-help
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-desc] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode stop]
    '(menu-item "Stop Playing" snd-stop
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode play]
    '(menu-item "Start Playing" snd-play
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode file]
    '(menu-item "Open Snd-File Dialog" snd-file
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-load] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode start-r]
    '(menu-item "Start Snd-Ruby Process" snd-run-snd
		:enable (not (snd-proc-p))
		:visible (eq 'ruby snd-inf-kind)))
  (define-key (current-local-map) [menu-bar mode start-f]
    '(menu-item "Start Snd-Forth Process" snd-run-snd
		:enable (not (snd-proc-p))
		:visible (eq 'forth snd-inf-kind)))
  (define-key (current-local-map) [menu-bar mode start-g]
    '(menu-item "Start Snd-Scheme Process" snd-run-snd
		:enable (not (snd-proc-p))
		:visible (eq 'scheme snd-inf-kind)))
  (define-key (current-local-map) [menu-bar mode switch]
    '(menu-item "Switch to Snd Process" snd-switch-to-snd
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-proc] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode block-go]
    '(menu-item "Send Block and Go" snd-send-block-and-go
		:enable (snd-proc-p)
		:visible (eq 'ruby snd-inf-kind)))
  (define-key (current-local-map) [menu-bar mode block]
    '(menu-item "Send Block" snd-send-block
		:enable (snd-proc-p)
		:visible (eq 'ruby snd-inf-kind)))
  (define-key (current-local-map) [menu-bar mode buffer-go]
    '(menu-item "Send Buffer and Go" snd-send-buffer-and-go
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode buffer]
    '(menu-item "Send Buffer" snd-send-buffer
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode region-go]
    '(menu-item "Send Region and Go" snd-send-region-and-go
		:enable (and (snd-proc-p) mark-active)))
  (define-key (current-local-map) [menu-bar mode region]
    '(menu-item "Send Region" snd-send-region
		:enable (and (snd-proc-p) mark-active)))
  (define-key (current-local-map) [menu-bar mode def-go]
    '(menu-item "Send Definition and Go" snd-send-definition-and-go
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode def]
    '(menu-item "Send Definition" snd-send-definition
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode last-sexp]
    '(menu-item "Send last Sexp" snd-send-last-sexp
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-load] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode load-sec]
    '(menu-item "Load Ruby Script (protected) ..." snd-load-file-protected
		:enable (snd-proc-p)
		:visible (eq 'ruby snd-inf-kind)))
  (define-key (current-local-map) [menu-bar mode load-r]
    '(menu-item "Load Ruby Script ..." snd-load-file
		:enable (snd-proc-p)
		:visible (eq 'ruby snd-inf-kind)))
  (define-key (current-local-map) [menu-bar mode load-f]
    '(menu-item "Load Forth Script ..." snd-load-file
		:enable (snd-proc-p)
		:visible (eq 'forth snd-inf-kind)))
  (define-key (current-local-map) [menu-bar mode load-g]
    '(menu-item "Load Scheme Script ..." snd-load-file
		:enable (snd-proc-p)
		:visible (eq 'scheme snd-inf-kind))))

(provide 'inf-snd)

;;; inf-snd.el ends here
