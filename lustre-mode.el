;;; lustre-mode.el --- Package description (don't include the word "Emacs")  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 First Last

;; Author: First Last <name@example.com>
;; URL: http://example.com/lustre-mode.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: something

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my package.  It is nice.  You should try it.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + foo
;; + bar

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'lustre-mode)

;;;; Usage

;; Run one of these commands:

;; `lustre-mode-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `lustre-mode' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

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

;;;; Requirements

(require 'foo)
(require 'bar)

;;;; Customization

(defgroup lustre-mode nil
  "Settings for `lustre-mode'."
  :link '(url-link "http://example.com/lustre-mode.el"))

(defcustom lustre-mode-something nil
  "This setting does something."
  :type 'something)

;;;; Variables

(defvar lustre-mode-var nil
  "A variable.")

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps.

(defvar lustre-mode-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "lustre-mode map"))
        (maps (list
               ;; Mappings go here, e.g.:
               "RET" #'lustre-mode-RET-command
               [remap search-forward] #'lustre-mode-search-forward
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))

;;;; Functions

;;;;; Commands

;;;###autoload
(defun lustre-mode-command (args)
  "Frobnicate the flange."
  (interactive)
  (lustre-mode--something)
  (bar))

;;;;; Support

(defun lustre-mode--something (args)
  "This function helps frobnicate the flange."
  (foo))

;;;; Footer

(provide 'lustre-mode)

;;; lustre-mode.el ends here
