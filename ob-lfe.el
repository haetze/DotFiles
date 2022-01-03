;;; ob-lfe.el --- org-babel functions for lisp flavored erlang (lfe) code evaluation

;; Copyright (C) 2021 Richard Stewing

;; Author: Richard Stewing
;; Keywords: literate programming, reproducible research
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating lfe code

;;; Code:
(require 'ob)

(defun org-babel-execute:lfe (body params)
  "Execute a block of lfe code with org-babel.
   This function is called by `org-babel-execute-src-block'."
  (message "executing lfe code block...")
  (let ((term (concat
	       "(progn "
	       body
	       "\n)")))
    (org-babel:lfe-send-term term))
  '())

(defun org-babel:lfe-send-term (term)
  (comint-send-string (inferior-lfe-proc) term)
  (comint-send-string (inferior-lfe-proc) "\n"))

(provide 'ob-lfe)
;;; ob-lfe.el ends here
