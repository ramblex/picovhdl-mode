;;; picovhdl-mode.el --- mode for editing picoVHDL code

;; Copyright (C) 2009 Alex Duller

;; Author: Alex Duller
;; Created: 2009-12-17

;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Usage

;; Put this file in your Emacs lisp path and add the following to your .emacs
;; file:
;;
;; (require 'picovhdl-mode)
;; (add-to-list 'auto-mode-alist '("\\.vhd$" . picovhdl-vhdl-mode))
;;
;; Optionally, it is possible to alter how much C++ code is indented with 
;; respect to the CODE ENDCODE tags. For example, the following will set the
;; indent to be 2 (it is set to 0 by default)...
;;
;; (setq picovhdl-c++-indent-offset 2)
;;
;; ...and will result in code like:
;;
;; SIM_DATA CODE
;;   void test()
;;   {
;;   }
;; ENDCODE;

;;; Commentary:

;; This mode tries to address the problem of having C++ code embedded within
;; VHDL. There are in fact two modes used within picoVHDL: `vhdl-mode' and
;; `c++-mode'. A number of the default options have been configured in order to
;; make implementing this mode a bit easier. For example, c-electric-flag has
;; been set to nil to stop indentation that is controlled by `picovhdl-mode'.
;; Features such as `vhdl-electric-mode' and `vhdl-stutter-mode' have also been
;; turned off by default.

;;; Code:

(require 'cc-mode)
(require 'vhdl-mode)

(defgroup picovhdl nil
  "picoVHDL mode"
  :prefix "picovhdl-"
  :group 'languages)

(defcustom picovhdl-c++-indent-offset 0
  "The number of spaces to indent C++ with respect to CODE"
  :type 'integer
  :group 'picovhdl)

(defcustom picovhdl-c++-mode-hook nil
  "List of functions to be executed on entry to `picovhdl-c++-mode'."
  :type 'hook
  :group 'picovhdl)

(defcustom picovhdl-vhdl-mode-hook nil
  "List of functions to be executed on entry to `picovhdl-vhdl-mode'."
  :type 'hook
  :group 'picovhdl)

;; Local variables
(defvar picovhdl-update 0)
(defvar picovhdl-idle-timer nil)
(defvar picovhdl-delay (/ (float 1) (float 16)))
(defvar picovhdl-mode-bool nil) ;; Whether the current buffer is using picovhdl
(defvar c++-open-delim "\\(SIM\\|HFA\\)_[ 0-9@A-Za-z]+CODE")
(defvar c++-close-delim "ENDCODE")

(defun picovhdl-setup ()
  "Setup the timer for the picovhdl mode"
  (interactive)
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'picovhdl-set-update nil t)
  (make-local-variable 'picovhdl-mode-bool)
  (setq picovhdl-mode-bool t)
  (when picovhdl-idle-timer
    (cancel-timer picovhdl-idle-timer))
  (setq picovhdl-idle-timer
        (run-with-idle-timer picovhdl-delay t
                             'picovhdl-update)))

(defun picovhdl-set-update ()
  (setq picovhdl-need-update 1))

(defun picovhdl-in-c++-block ()
  "Check whether the point is within a C++ block"
  (interactive)
  (save-excursion
    (let ((starts (count-matches c++-open-delim (point-min) (point) nil))
          (ends (count-matches c++-close-delim (point-min) (point) nil)))
      (if (> starts ends)
          t
        nil))))

(defun picovhdl-switch-to (mode)
  "Switch between vhdl and c++ major modes if necessary"
  (if (string= mode major-mode)
      (progn (setq picovhdl-need-update 0)
             t)
    (funcall mode)
    (hack-local-variables)
    (picovhdl-setup)))

(defun picovhdl-update ()
  "Check whether the mode should be changed"
  (when (and picovhdl-mode-bool picovhdl-need-update)
    (if (picovhdl-in-c++-block)
        (picovhdl-switch-to 'picovhdl-c++-mode)
      (picovhdl-switch-to 'picovhdl-vhdl-mode))))

(defun set-brace-strs ()

(setq open-brace-str 
  (concat "\n" 
   (make-string picovhdl-c++-indent-offset (string-to-char " ")) 
   "{"))

 (setq close-brace-str
  (concat (make-string picovhdl-c++-indent-offset (string-to-char " "))
   "}\n")))

(defun picovhdl-add-c-braces ()
  "Helper function used in picovhdl indent functions"
  (set-brace-strs)
  (save-excursion
    (when (re-search-backward c++-open-delim (point-min) t)
      (end-of-line)
      (insert open-brace-str))
    (when (re-search-forward c++-close-delim (point-max) t)
      (beginning-of-line)
      (insert close-brace-str))))

(defun picovhdl-cleanup-braces ()
  "Helper function used in picovhdl to clean up braces added by
  picovhdl-add-c-braces"
  (interactive)
  (save-excursion
    (when (re-search-backward c++-open-delim (point-min) t)
      (re-search-forward open-brace-str)
      (delete-backward-char (length open-brace-str)))
    (when (re-search-forward c++-close-delim (point-max) t)
      (previous-line)
      (beginning-of-line)
      (delete-char (length close-brace-str)))))

(defun picovhdl-indent-region (start end &optional quiet)
  (interactive)
  (message "picovhdl-indent-region")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (while (< (point) end)
      (or (and (bolp) (eolp))
          (picovhdl-indent-line))
      (forward-line 1))
    (move-marker end nil)))

(defun picovhdl-contains (code)
  "Returns t if the current line contains code, otherwise returns nil."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward code (point-at-eol) t) 
        t
      nil)))

(defun picovhdl-indent-line ()
  (interactive)
  (if (or (eq (picovhdl-contains c++-close-delim) t)
          (eq (picovhdl-contains c++-open-delim) t))
      (save-excursion
        (indent-line-to vhdl-basic-offset))
    (if (picovhdl-in-c++-block)
        (progn (picovhdl-add-c-braces)
               (ignore-errors (c-indent-line))
               (picovhdl-cleanup-braces))
      (vhdl-indent-line))))

(define-derived-mode picovhdl-c++-mode c++-mode "picoVHDL (C++)"
  "C++ mode for picoVHDL"
  (c-add-language 'picovhdl-c++-mode 'c++-mode)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'picovhdl-indent-line)
  (setq indent-region-function 'picovhdl-indent-region)
  (setq c-electric-flag nil)
  (run-hooks 'picovhdl-c++-mode-hook)
  (picovhdl-setup))

(define-derived-mode picovhdl-vhdl-mode vhdl-mode "picoVHDL (VHDL)"
  "VHDL mode for picoVHDL"
  (setq vhdl-electric-mode nil)
  (setq vhdl-stutter-mode nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'picovhdl-indent-line)
  (setq indent-region-function 'picovhdl-indent-region)
  (run-hooks 'picovhdl-vhdl-mode-hook)
  (picovhdl-setup))

(defvar picovhdl-kwds
  '("complex"
    "complex8"
    "complex8pair"
    "complex16"
    "complex32"
    "complex64"
    "integer8"
    "integer8pair_complex8"
    "integer8quad"
    "integer16"
    "integer16_complex8"
    "integer16_integer8pair"
    "integer16pair"
    "integer32"
    "integer64"))

;(regexp-opt picovhdl-kwds 'words)

;; Common keywords
(setq common-kwds
  '(("\\(SIM_[A-Z]+\\)" 1 font-lock-preprocessor-face prepend)
    ("\\<\\(CODE\\)" 1 font-lock-preprocessor-face prepend)
    ("\\<\\(ENDCODE\\)" 1 font-lock-preprocessor-face prepend)
    ("\\<\\(complex\\(?:16\\|32\\|64\\|8\\(?:pair\\)?\\)?\\|integer\\(?:16\\(?:_\\(?:complex8\\|integer8pair\\)\\|pair\\)?\\|32\\|64\\|8\\(?:pair_complex8\\|quad\\)?\\)\\)\\>" . font-lock-type-face)
    ))
(font-lock-add-keywords 'picovhdl-c++-mode common-kwds)
(font-lock-add-keywords 'picovhdl-vhdl-mode common-kwds)

;(add-hook 'vhdl-mode-hook 'picovhdl-setup)

(provide 'picovhdl-mode)
