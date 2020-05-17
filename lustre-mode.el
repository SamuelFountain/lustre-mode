;;; lustre-mode.el --- Major mode for editing lustre source in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Samuel Fountain

;; Author: Samuel Fountain <16006711+SamuelFountain@users.noreply.github.com >
;; URL: https://github.com/SamuelFountain/lustre-mode
;; Version: 2.0.0
;; Package-Requires: ((emacs "25.2"))
;; Keywords: lustre

;; This file is not part of GNU Emacs.

;;; Commentary:



;;;; Installation

;;;;; MELPA

;; TODO add package to MELPA

;;;;; Manual

;; Put this file in your load-path
;; file: lustre-mode.el

;; and put this in your init
;; (require 'lustre-mode)

;; Alternativly if using use-package

;;(use-package lustre-mode)

;; Byte-compile lustre.el to speed-up
;; the loading of a lustre source file :

;; M-x byte-compile-file  -> lustre.el


;;;; Usage

;; This is a major moding for editing lustre files
;; activate it with `lustre-mode' but the mode should
;; activate by default in `.lus' files

;;;; Tips

;; + You can customize settings in the `lustre-mode' group.

;;;; Credits

;; Based off of work done by Chevallier Olivier
;; https://matthieu-moy.fr/emacs/lustre.el

;; Modified by Nicolas Berthier
;; http://www-verimag.imag.fr/DIST-TOOLS/SYNCHRONE/lustre-v6/doc/lustre.el

;; Their work has been modified and released under
;; the terms of their license.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defconst lustre-mode-version "2.0.0")

;;;; Requirements

(require 'font-lock)
(require 'cl-lib)

;;;; Customization

(defgroup lustre-mode nil
  "Settings for `lustre-mode'.")

(defcustom lustre-mode-comment-ind-level 2
  "*How many spaces to indent a comment."
  :type '(integer)
  :group 'lustre-mode)

(defcustom lustre-mode-executer-name "jkind"
  "Name of the lustre executor"
  :group 'lustre-mode )
;;;; Variables


(defvar lustre-mode-var nil
  "A variable.")

(defvar lustre-mode-hook nil
  "functions called when entering Lustre Mode.")

;; {{{ Font lock

;; NB: Note `labels' is a common-lisp feature.
(defconst lustre-mode-font-lock-keywords
  ;; note that we specify optional multi-line font lock here...
  (let ((sep "[\t\n ]*")
	(id "\\<\\(\\w*\\)\\>")
	(ids "\\(\\w\\|[,\t\n ]\\)*")
	(var font-lock-variable-name-face)
	(fun font-lock-function-name-face)
	(cte font-lock-constant-face)
	(cmt font-lock-comment-face)
	(str font-lock-string-face)
	(typ font-lock-type-face)
	(kw  font-lock-keyword-face))
    (cl-labels ((id-before (suf) (concat id sep suf))
	     (id-after (pref) (concat pref sep id))
	     (id-in-btw (pref suf) (concat pref sep (id-before suf)))
	     (ids-before (suf) (concat ids sep suf))
	     (ids-after (pref) (concat pref sep ids))
	     (ids-in-btw (pref suf) (concat pref sep (ids-before suf))))
      `(
					; identifiers
	(,(ids-in-btw "\\([(;^]\\|\\<var\\|const\\|package\\|model\\|provides\\|body\\|uses\\>\\)" ":")	. ,var)
	;; NB: needed in some cases, such as long lists of formal arguments...:
	(,(ids-before ":")					. ,var)

	(,(concat "[:;,/().]\\|"	; keywords (override since we
					; match commas and other keywords
					; in lists of identifiers)
		  (regexp-opt '("/" "*" "#" "=" "+" "-" "*" "<" ">")) "\\|"
		  (regexp-opt '("node" "const" "function" "include" "let"
				"returns" "tel" "type" "var" "if" "with"
            "package" "model" "body" "uses" "provides"
				"then" "else" "and" "or" "xor" "assert" "with" "struct"
				"pre" "not" "when"
				"current") 'words))		0 ,kw t)

	;; NB: we need to do this 'cause keywords may be highlighted in comments
	;; and strings otherwise (in some circumstances).
	;;
	;; XXX: I have no clue for multi-line comments yet... (except maybe a
	;; call to some function).
	("--.*$"						0 ,cmt t)
	("\".*\""						0 ,str t)

					; type names
	(,(id-after (regexp-opt '("type")))			1 ,typ)
	(,(id-in-btw ":" (regexp-opt '(")" ";")))		1 ,typ)

					; call
	(,(id-after (regexp-opt '("node" "function") 'words))	1 ,fun)
	(,(id-before "(")					1 ,fun)

					; constants
	(,(regexp-opt '("true" "false") 'words)			. ,cte)
	("\\<[0-9]*\\>"						. ,cte))))

  "Regular expressions used by Font-lock mode.")

(defun lustre-mode-font-lock-mode ()
  "Initialisation of font-lock for Lustre mode."
  (setq font-lock-multiline 'undecided
	font-lock-defaults '(lustre-mode-font-lock-keywords))
  (font-lock-mode 1))

(add-hook 'lustre-mode-mode-hook 'lustre-mode-font-lock-mode)

;; }}}

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps.

(defvar lustre-mode-map nil
  "Keymap for lustre major mode.")

(unless lustre-mode-map
  (setq lustre-mode-map (make-sparse-keymap))
  (define-key lustre-mode-map "\C-c\C-r" 'lustre-mode-run)
  (define-key lustre-mode-map (quote [f10]) 'lustre-mode-run)
  )

;;;;; Syntax table
;; {{{ Syntax table

(defvar lustre-mode-syntax-table (make-syntax-table)
  "Syntax table used in the Lustre mode.")

					; `_' is also in words.
(modify-syntax-entry ?\_	"w"	lustre-mode-syntax-table)

					; comment line finisher:
(modify-syntax-entry ?\n	">"	lustre-mode-syntax-table)

					; comment start if double,
					; punct. otherwise.
(modify-syntax-entry ?\-	". 12" 	lustre-mode-syntax-table)

;; multiline comments (is this the right syntax?):
					; beg of multiline comment starter
(modify-syntax-entry ?\(	"()1b"	lustre-mode-syntax-table)
					; beg of multiline comment finisher
(modify-syntax-entry ?\)	")(4b"	lustre-mode-syntax-table)
					; end & beg of multiline comment starter
					; & finisher.
(modify-syntax-entry ?\*	". 23b"	lustre-mode-syntax-table)

;; }}}

;;;; Functions

;;; indentation code ----------------------------------------------
(defun lustre-mode--indent-decl ()
  "Returns the indentation of a declaration line. "
  (let ((result 2))
    (save-excursion
      (if (re-search-backward "^\\<\\(const\\|tel\\|type\\|var\\)\\>" 0 t)
	  (cond
	   ((looking-at "^\\<tel\\>") (setq result 2))
	   ((looking-at "^\\<\\(const\\|type\\|var\\)\\>[ \t]*$")
	    (setq result 2))
	   ((looking-at
	     "^\\<const\\>[ \t]*.+") (setq result 6))
	   ((looking-at "^\\<type\\>[ \t]*.+") (setq result 5))
	   ((looking-at "^\\<var\\>[ \t]*.+") (setq result 4)))))
    result))




(defun lustre-mode--get-beginning-of-line (&optional arg)
  "Returns position of the first non-space char of the current line,
   or line (arg - 1) if optional argument is given."
  (save-excursion
    (beginning-of-line arg)
    (let ((deb (point)))
      (skip-chars-forward " \t")
      (let ((fin (point)))
        (- fin deb)))))

(defun lustre-mode--get-point-of-line ()
  "Returns position of the first char of the current line."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun lustre-mode--skip-comments ()
  "set point before the commentary of the current line (if any)."
  (beginning-of-line)
  (while (not (or (looking-at "$")
		  (looking-at "--")))
    (forward-char 1)))

(defun lustre-mode--line-is-comment (&optional arg)
  "non-nil means line is only a commentary."
  (save-excursion
    (beginning-of-line arg)
    (skip-chars-forward " \t")
    (looking-at "--")))

(defun lustre-mode--line-is-decl ()
  "non-nil means current line is a declaration. "
  (save-excursion
    (let ((res nil)
	  (continue t))
      (while continue
	(if (= (point) 1)
	    (setq continue nil))
	(re-search-backward
	 "\\<\\(const\\|let\\|node\\|var\\|type\\|function\\)\\>" 1 t)
	(if (not (lustre-mode--line-is-comment))
	    (setq continue nil)))
      (if (looking-at "\\<\\(const\\|type\\|var\\)\\>")
	  (setq res t))
      (if (looking-at "\\<\\(let\\|node\\|function\\)\\>")
	  (setq res nil))
      res)))


(defun lustre-mode--in-comment ()
  "non-nil means point is inside a comment."
  (save-excursion
    (re-search-backward "--" (lustre-mode--get-point-of-line) t)))

(defun lustre-mode--skip-commentary-lines ()
  "set point to the beginnig of the first non-commemtary line before
   the current line."
  (forward-line -1)
  (while (and (lustre-mode--line-is-comment) (> (point) 1))
    (forward-line -1)))

(defun lustre-mode--indent (level)
  "Indents current line ."
  (beginning-of-line)
  (delete-char (lustre-mode--get-beginning-of-line))
  (let ((ind level))
    (while (> ind 0)
      (insert " ")
      (setq ind (- ind 1)))))


(defun lustre-mode--find-noindent-reg ()
  "non-nil means current line begins with:
   const, function, include, let, var, tel, node, returns, type."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (and
     (looking-at

"\\<\\(const\\|function\\|include\\|let\\|node\\|returns\\|tel\\|type\\|var\\)\\>")
     (not (lustre-mode--in-comment)))))


(defun lustre-mode--find-unmatching-parent ()
  "Looks for an unmatched parenthese, and returns its position.
   (or nil if there isn't any). "
  (let ((continue t)
        (result nil)
        (beg nil)
        (count-parent 0))
    (save-excursion
      (beginning-of-line)
      (if (= (point) 1)
	  (setq continue nil))
      (while continue
        (forward-line -1)
        (setq beg (point))
	(end-of-line)
        (while (and (not (looking-at "^")) continue)
          (if (and (looking-at ")") (not (lustre-mode--in-comment)))
              (setq count-parent (- count-parent 1))
            (if (and (looking-at "(") (not (lustre-mode--in-comment)))
                (progn
                  (setq count-parent (+ count-parent 1))
                  (if (= count-parent 1)
		      (progn
			(setq result (- (point) beg))
			(setq continue nil))))))
	  (forward-char -1))
	(skip-chars-forward " \t")
	(if (and (looking-at "\\<const\\|var\\|type\\|node\\|function\\>")
		 (not (lustre-mode--in-comment)))
	    (setq continue nil))
	(beginning-of-line)
	(if (= (point) 1)
	    (setq continue nil))))
    result))


(defun lustre-mode--indent-normal ()
  "non-nil means indent normally."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (looking-at "[]a-zA-Z0-9^[()]+")))

(defun lustre-mode--empty-line ()
  "non-nil means line is empty."
  (save-excursion
    (skip-chars-forward " \t")
    (looking-at "$")))



(defun lustre-mode--compute-indent ()
  "Returns indentation of current line."
  (cond
   ; if line is comment
   ((lustre-mode--line-is-comment) lustre-mode-comment-ind-level)
   ; if line begins with node,include...
   ((lustre-mode--find-noindent-reg) 0)
   ; if line begins with 'then' or 'else'
   ((lustre-mode--find-then-else-beg) (lustre-mode--indent-then-else-beg))
   ; if previous line ends with 'then' or 'else'
   ((lustre-mode--find-then-else-end) (+ (lustre-mode--indent-then-else-end) 2))
   ; looks for an unmatched parenthese
   ((lustre-mode--find-unmatching-parent) (+ (lustre-mode--find-unmatching-parent) 1))
   ; if line is a declaration
   ((lustre-mode--line-is-decl) (lustre-mode--indent-decl))
   ; if previous line ends with '->'
   ((lustre-mode--find-arrow) (lustre-mode--indent-arrow-equal-bool))
   ; if previous line ends with '='
   ((lustre-mode--find-equal-end) (lustre-mode--indent-arrow-equal-bool))
   ; if previous line ends with a boolean operator
   ((lustre-mode--find-bool-expr-end) (lustre-mode--indent-arrow-equal-bool))
   ; if line is a 'normal line'
   ((lustre-mode--indent-normal) 2)
   ; line is empty
   ((lustre-mode--empty-line) 2)
   ; else ...
   (t 0)))


(defun lustre-mode--find-arrow ()
  "non-nil means previous line ends with '->' ."
  (save-excursion
    (lustre-mode--skip-commentary-lines)
    (lustre-mode--skip-comments)
    (skip-chars-backward " \t")
    (forward-char -2)
    (and (looking-at "->") (not (lustre-mode--in-comment)))))


(defun lustre-mode--indent-arrow-equal-bool ()
  "Find the level of indentation when previous line ends with '->',
'='
   or a boolean (or, xor, and)."
  (save-excursion
    (lustre-mode--skip-commentary-lines)
    (+ (lustre-mode--get-beginning-of-line) 2)))


(defun lustre-mode--find-bool-expr-end ()
  "non-nil means last line ends with 'and', 'xor' or 'or'."
  (let ((result nil))
    (save-excursion
      (lustre-mode--skip-commentary-lines)
      (lustre-mode--skip-comments)
      (skip-chars-backward " \t")
      (forward-char -2)
      (setq result (and (looking-at "\\<or\\>")
			(not (lustre-mode--in-comment))))
      (forward-char -1)
      (or (and (looking-at "\\<\\(and\\|not\\|xor\\)\\>")
	       (not (lustre-mode--in-comment)))

	  result))))


(defun lustre-mode--find-then-else-beg ()
  "non-nil means current line begins with 'then' or 'else' ."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (and (looking-at "\\<\\(else\\|then\\)\\>")
	 (not (lustre-mode--in-comment)))))


(defun lustre-mode--find-then-else-end ()
  "non-nil means last line ends with 'then' or 'else'."
  (save-excursion
    (lustre-mode--skip-commentary-lines)
    (lustre-mode--skip-comments)
    (skip-chars-backward " \t")
    (forward-char -4)
    (and (looking-at "\\<\\(else\\|then\\)\\>")
	 (not (lustre-mode--in-comment)))))



(defun lustre-mode--find-equal-end ()
  "non-nil means last line ends with '=' ."
  (save-excursion
    (lustre-mode--skip-commentary-lines)
    (lustre-mode--skip-comments)
    (skip-chars-backward " \t")
    (forward-char -1)
    (and (looking-at "=")
	 (not (lustre-mode--in-comment)))))



(defun lustre-mode--indent-then-else-beg ()
  "Returns the level of indentation of a line beginning with
   'then' or 'else'."
  (let ((beg nil)
        (result nil)
        (count-expr 0)
        (continue t))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (and (looking-at "\\<then\\>") (not (lustre-mode--in-comment)))
          (while continue
            (lustre-mode--skip-commentary-lines)
	    (setq beg (point))
	    (lustre-mode--skip-comments)
            (skip-chars-forward " \t")
	    (if (and (looking-at "\\<node\\|function\\>")
		     (not (lustre-mode--in-comment)))
		(setq continue nil))
	    (end-of-line)
            (while (and (not (looking-at "^")) continue)
              (if (and (looking-at "\\<then\\>")
		       (not (lustre-mode--in-comment)))
                  (setq count-expr (- count-expr 1))
                (if (and (looking-at "\\<\\(if\\|with\\)\\>")
			 (not (lustre-mode--in-comment)))
		    (progn
                      (setq count-expr (+ count-expr 1))
                      (if (and (= count-expr 1) continue)
                          (progn
                            (setq continue nil)
                            (setq result (- (point) beg)))))))
              (forward-char -1)))
	(if (looking-at "\\<else\\>")
            (while continue
	      (lustre-mode--skip-commentary-lines)
	      (setq beg (point))
	      (lustre-mode--skip-comments)
	      (skip-chars-forward " \t")
	      (if (and (looking-at "\\<node\\|function\\>")
		       (not (lustre-mode--in-comment)))
		  (setq continue nil))
	      (end-of-line)
	      (while (and (not (looking-at "^")) continue)
		(if (and (looking-at "\\<else\\>")
			 (not (lustre-mode--in-comment)))
		    (setq count-expr (- count-expr 1))
		  (if (and (looking-at "\\<\\(if\\|with\\)\\>")
			   (not (lustre-mode--in-comment)))
		      (progn
			(setq count-expr (+ count-expr 1))
			(if (and (= count-expr 1) continue)
			    (progn
			      (setq continue nil)
			      (setq result (- (point) beg)))))))
		(forward-char -1))))))
    result))


(defun lustre-mode--indent-then-else-end ()
  "Returns the level of indentation of a line ending with 'then' or
'else'."
  (let ((beg nil)
        (result nil)
        (count-expr 1)
        (continue t))
    (save-excursion
      (lustre-mode--skip-commentary-lines)
      (lustre-mode--skip-comments)
      (skip-chars-backward " \t")
      (forward-char -4)
      (if (and (looking-at "\\<then\\>") (not (lustre-mode--in-comment)))
          (progn
            (forward-line 1)
            (while continue
              (forward-line -1)
              (setq beg (point))
              (skip-chars-forward " \t")
	      (if (and (looking-at "\\<node\\|function\\>")
		       (not (lustre-mode--in-comment)))
		  (setq continue nil))
	      (end-of-line)
              (while (and (not (looking-at "^")) continue)
                (if (and (looking-at "\\<then\\>")
			 (not (lustre-mode--in-comment)))
		    (setq count-expr (- count-expr 1))
                  (if (and (looking-at "\\<\\(if\\|with\\)\\>")
			   (not (lustre-mode--in-comment)))
		      (progn
                        (setq count-expr (+ count-expr 1))
                        (if (and (= count-expr 1) continue)
                            (progn
                              (setq continue nil)
                              (setq result (- (point) beg)))))))
                (forward-char -1))))
        (if (and (looking-at "\\<else\\>") (not (lustre-mode--in-comment)))
            (progn
              (forward-line 1)
              (while continue
		(forward-line -1)
		(setq beg (point))
		(skip-chars-forward " \t")
		(if (and (looking-at "\\<node\\|function\\>")
			 (not (lustre-mode--in-comment)))
		    (setq continue nil))
		(end-of-line)
		(while (and (not (looking-at "^")) continue)
		  (if (and (looking-at "\\<else\\>")
			   (not (lustre-mode--in-comment)))
		      (setq count-expr (- count-expr 1))
		    (if (and (looking-at "\\<\\(if\\|with\\)\\>")
			     (not (lustre-mode--in-comment)))
			(progn
			  (setq count-expr (+ count-expr 1))
			  (if (and (= count-expr 1) continue)
			      (progn
				(setq continue nil)
				(setq result (- (point) beg)))))))
		  (forward-char -1)))))))
    result))

;; -----------------------------------------------------------------------------


;;;;; Commands
(defun lustre-mode-run()
  "Saves the file and calls the lustre executure Prompt for saving if
`compilation-ask-about-save' is non nil"
  (interactive)
  (if (buffer-modified-p)
      (if (or (not compilation-ask-about-save)
	      (y-or-n-p "Save file? "))
	  (save-buffer)))
  (let ((fichier (buffer-file-name)))
    (progn
      (delete-other-windows)
      ;; TODO let split horozontally
      (split-window-vertically)
      (get-buffer-create "*run-lustre-mode*")
      (select-window (next-window))
      (switch-to-buffer "*run-lustre-mode*")
      (start-process "lustre-mode-run"
		     "*run-lustre-mode*"
		     lustre-mode-executer-name
		     fichier)
      (end-of-buffer)
      (select-window (next-window)))))

;;; indentation code ----------------------------------------------
(defun lustre-mode-electric-end-of-line ()
  "Insert a newline."
  (interactive)
  (newline))

(defun lustre-mode-electric-tab ()
  "Indent current line ."
  (interactive)
  (let ((mark (make-marker)))
    (set-marker mark (point))
    (beginning-of-line)
    (lustre-mode--indent (lustre-mode--compute-indent))
    (goto-char (marker-position mark))
    (set-marker mark nil)
    )
  (if (looking-at "^")
      (skip-chars-forward " \t")))






;;;###autoload
(define-derived-mode lustre-mode prog-mode "Lustre"
    "Major mode for editing Lustre files.

   Special Commands :
     C-c C-r : execute a lustre file
     f10 : execute a lustre file

   Customisable variables :

     - lustre-comment-ind-level:
         How many spaces to indent a comment. (default: 2)

     - lustre-executer-name:
         Name of the lustre model checker to call. (default: 'jkind')

   File bug reports on github https://github.com/SamuelFountain/lustre-mode/


\\{lustre-mode-map}"
    (setq-local comment-start "--")
    (setq-local comment-end "")
    (setq font-lock-defaults '(lustre-mode-font-lock-keywords nil t))
    (set (make-local-variable 'indent-line-function) 'lustre-mode-electric-tab))


;;;;; Support
(add-to-list 'auto-mode-alist '("\\.lus\\'" . lustre-mode))

;;;; Footer

(provide 'lustre-mode)

;;; lustre-mode.el ends here
