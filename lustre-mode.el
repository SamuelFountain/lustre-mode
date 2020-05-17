;;; lustre-mode.el --- Major mode for editing lustre source in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Samuel Fountain

;; Author: Samuel Fountain <16006711+SamuelFountain@users.noreply.github.com >
;; URL: https://github.com/SamuelFountain/lustre-mode
;; Version: 2.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: lustre

;; This file is not part of GNU Emacs.

;;; Commentary:



;;;; Installation

;;;;; MELPA

;; TODO add package to MELPA

;;;;; Manual

;; Install these required packages:

;; + font-lock


;; Then put this file in your load-path, and put this in your init
;; file: lustre-mode.el

;; (require 'lustre-mode)

;; Alternativly if using use-package

;;(use-package lustre-mode)

;;;; Usage

;; Run one of these commands:

;; `lustre-mode-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `lustre-mode' group.

;;;; Credits

;; Based off of work done by Chevallier Olivier
;; https://matthieu-moy.fr/emacs/lustre.el

;; Modified by Nicolas Berthier
;; http://www-verimag.imag.fr/DIST-TOOLS/SYNCHRONE/lustre-v6/doc/lustre.el

;; Their work has been modified and released under
;; the terms of their GNU GPL V2 and the option has been taken
;; to use a later version of the license.

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

(defconst lustre-mode-version "2.1")

;;;; Requirements

(require 'font-lock)

;;;; Customization

(defgroup lustre-mode nil
  "Settings for `lustre-mode'.")

(defcustom lustre-comment-ind-level 2
  "*How many spaces to indent a comment."
  :type '(integer)
  :group 'lustre-mode)

;;;; Variables


(defvar lustre-mode-var nil
  "A variable.")

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps.

(defvar lustre-mode-map nil
  "Keymap for lustre major mode.")
(unless lustre-mode-map
  (setq lustre-mode-map (make-sparse-keymap))
  ()
  )

;;;; Functions

;;;;; Commands


;;;;; Support


;;;; Footer

(provide 'lustre-mode)

;;; lustre-mode.el ends here
