;;; vtex.el --- A lightweight LaTeX mode for emacs.

;; Copyright (C) 2014 David Wells

;; Author: David Wells <drwells @ virginia tech email>
;; Version: 0.2
;; Keywords: LaTeX

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

;;; Commentary:

;; A modern (and incomplete) replacement for AUCTeX.
;;

;;; Code:
(defconst vtex-version 0.2
  "alpha copy of vtex.")

;; syntax highlighting
(defun vtex--font-lock-opt (string-list font-lock-input &optional paren)
  "Utility functon to apply regexp-opt to `string-list' and form a list of a cons
   cell with `font-lock-input'."
  (progn
    (if (eq paren 'word-right)
        (setq regex (concat (regexp-opt string-list) "\\>"))
      (setq regex (regexp-opt string-list paren)))
    (list `(,regex . ,font-lock-input))))

(defconst vtex-font-lock-sectioning
  (list
   '("section{\\(.*\\)}$" 1 font-lock-builtin-face)
   '("begin{\\([^}]*\\)}" 1 font-lock-builtin-face)
   '("end{\\([^}]*\\)}" 1 font-lock-builtin-face)
   (car (vtex--font-lock-opt
    '("\\section" "\\subsection" "\\subsubsection" "\\begin" "\\end")
    font-lock-keyword-face 'word-right))
   )
  "Additional keywords to highlight in VTEX mode.")

(defconst vtex-font-lock-math-delimiters
  (vtex--font-lock-opt '("\\(" "\\)" "\\[" "\\]" "$") font-lock-string-face)
  "Math delimiters.")

(defconst vtex-font-lock-catch-backslash
  (list '("\\(\\\\\\w+\\)" . font-lock-builtin-face))
  "catch additional backslashed terms.")

(defconst vtex-font-lock-catch-backslash-special
  (vtex--font-lock-opt '("\\&" "\\%" "\\#" "\\\\") font-lock-negation-char-face)
  "Catch special backslashed terms.")

(defconst vtex-font-lock-math-align
  (vtex--font-lock-opt '("&" "&=" "&\\leq" "&\\geq" "&<" "&>")
                       font-lock-negation-char-face)
  "Math alignment terms.")

(defconst vtex-font-lock-misc-keywords
  (vtex--font-lock-opt
   '("\\label" "\\eqref" "\\cite" "\\newcommand" "\\input" "\\include"
     "\\RequirePackage" "\\usepackage" "\\documentclass" "\\setcounter")
   font-lock-function-name-face)
  "Miscellaneous keywords.")

(defconst vtex-font-lock-spacing-commands
  (vtex--font-lock-opt
   '("\\quad" "\\," "\\thinmuskip" "\\!" "\\>" "\\;" "\\:" "\\enspace" "\\qquad"
     "\\hspace" "\\hphantom" "\\hfill")
   font-lock-function-name-face)
  "Common spacing commands.")

(defvar vtex-font-lock-keywords
  (append vtex-font-lock-sectioning
          vtex-font-lock-math-delimiters
          vtex-font-lock-math-align
          vtex-font-lock-misc-keywords
          vtex-font-lock-spacing-commands
          vtex-font-lock-catch-backslash
          vtex-font-lock-catch-backslash-special)
  "Default highlighting expressions for VTEX mode.")

(defvar vtex-syntax-table
  (let ((vtex-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?% "< b" vtex-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" vtex-mode-syntax-table)
    vtex-mode-syntax-table)
  "Syntax table for VTEX mode.")

;; utility regexps
(defconst vtex-block-terminator-regexp
  (concat "^ *$\\|" "[^\\]%\\|" (regexp-opt
           '("\\[" "\\]" "\\section{" "\\subsection{" "\\subsubsection{"
             "\\begin{" "\\end{" "\\label{")))
  "regexp to match at the end of a LaTeX block.")

;; indentation
(defun vtex-matching-begin-indent ()
  "Assuming that the cursor is currently at an 'end' statement, find the
   indentation level of the matching begin statement."
  (interactive)
  (save-excursion
    (setq stack-count 1)
    (while (and (not (bobp)) (not (eq stack-count 0)))
      (forward-line -1)
      (beginning-of-line)
      (if (search-forward "\\begin" (line-end-position) t)
          (setq stack-count (- stack-count 1))
        (if (search-forward "\\end" (line-end-position) t)
            (setq stack-count (+ stack-count 1)))))
    (current-indentation)))

(defun vtex-fix-end-block ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^ *\\\\end")
      (progn (message "found end block")
             (indent-line-to (vtex-matching-begin-indent))))))

;; TODO implement this.
(defun vtex--in-math-align-block ()
  "Determine if we are currently in some sort of math align environment."
  ; Assume that alignment sections cannot be nested. A new section or a begin
  ; block will terminate an align block.
  nil
  )

(defun vtex-indent-line ()
  (interactive)
  (defvar vtex-next-indent 0)
  (setq vtex-max-look-count 2)
  (setq vtex-look-count 1)
  (save-excursion
    (progn
      (forward-line -1)
      (beginning-of-line)
      (cond
       ((bobp)
        (progn
          (message "using (bobp) to indent")
          (indent-line-to 0)))
       ((looking-at "^ *\\\\section")
        (progn
          ;; check sectioning.
          (message "using \\section to indent")
          (indent-line-to 0)
          (setq vtex-next-indent 4)))
       ((looking-at "^ *\\\\subsection")
        (progn
          (message "using \\subsection to indent")
          (indent-line-to 4)
          (setq vtex-next-indent 8)))
       ((looking-at "^ *\\\\subsubsection")
        (progn
          (message "using \\subsubsection to indent")
          (indent-line-to 8)
          (setq vtex-next-indent 12)))
       ;; check for "\item".
       ((looking-at "^ *\\\\item ")
        (progn
          (message "using \\item to indent")
          (setq vtex-next-indent (+ 6 (current-indentation)))))
       ;; check begin/end blocks.
       ((and (looking-at "^.*\\\\begin")
             (not (looking-at "^.*\\\\begin{document}")))
        (progn
          (message "using \\begin to indent")
          (setq vtex-next-indent
                (+ 4 (current-indentation)))))
       ((looking-at "^.*\\\\end")
        (progn
          (message "using \\end to indent")
          (indent-line-to
           (vtex-matching-begin-indent))
          (setq vtex-next-indent (current-indentation))))
       ; Add code for checking to see if inside a math alignment block here.  If
       ; the previous line ended with "//", then indent to the "&=". Otherwise
       ; indent 3 more than last line (to align with previous "&=").
;;       ((vtex--in-math-align-block)
;;        (progn ))
       ((looking-at "^ *$")
        (progn
          (message "using empty line to indent")
          (while (and (eq (current-indentation) 0)
                      (< vtex-look-count
                         vtex-max-look-count)
                      (not (bobp)))
            (forward-line -1)
            (setq vtex-look-count
                  (1+ vtex-look-count)))
          (setq vtex-next-indent
                (current-indentation))))
       (t
        (progn
          (message "using previous line to indent")
          (setq vtex-next-indent (current-indentation)))))))
  (indent-line-to vtex-next-indent)
  ; end blocks are badly behaved. Check it again.
  (vtex-fix-end-block))

(defun vtex-indent-region (start end)
  "Based on my preferences for how indentation works (i.e. fix
   section locations while indenting), override the usual
   indent-region command."
  (message "running vtex-indent-region")
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char 0)
      (while (not (eobp))
        (vtex-indent-line)
        (forward-line 1)
        (end-of-line)))))

;; alignment of "\\"s
(defun vtex--align-continuation-right (line)
  "Align the \\\\ on the end of a string."
  (let ((index (string-match "\\\\\\\\$"
                             (replace-regexp-in-string " *\\\\\\\\ *$"
                                                       "\\\\\\\\" line))))
    (cond
     ((eq index nil) line)
     (t (concat (substring line 0 index)
                ; add appropriate spaces (or none if line too long)
                (make-string (max 0 (- fill-column index 2)) ? )
                "\\\\")))))

(defun vtex-align-continuation-right-buffer ()
  "Align the terminating \\\\s to be at the 78th and 79th columns."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\\\\\\\\\\s-*$" (point-max) t)
      ; don't edit the line if it is already the correct length.
      (if (= (- (line-end-position) (line-beginning-position)) 80)
          nil
        (let ((new-content
             (vtex--align-continuation-right
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position)))))
        (progn
          (delete-region (line-beginning-position) (line-end-position))
          (goto-char (line-beginning-position))
          (insert new-content)
          nil))))))

;; paragraph filling
(defun vtex--find-paragraph-border (direction)
  "Find the first or last character of a LaTeX block by
`vtex-block-terminator-regexp'. If the current line matches the
paragraph terminator, then return nil."
  (save-excursion
    (progn
      (goto-char (+ (line-beginning-position) (current-indentation)))
      (cond
       ((looking-at vtex-block-terminator-regexp) nil)
       ((eq direction 'up)
        (progn
          (search-backward-regexp vtex-block-terminator-regexp)
          (forward-line 1)
          (line-beginning-position)))
       ((eq direction 'down)
        (progn (search-forward-regexp vtex-block-terminator-regexp)
               (forward-line -1)
               (line-end-position)))
       (t (error (concat "unrecognized direction;"
                         "should be `up' or `down'")))))))

(defun vtex-fill-paragraph ()
  "Call fill-region based on a narrowed range."
  (interactive)
  ;; TODO just call fill-paragraph if inside a comment. Make sure that this is
  ;; not a recursive call.
  (let ((first-char (vtex--find-paragraph-border 'up))
        (last-char (vtex--find-paragraph-border 'down)))
    (if (or (eq first-char nil) (eq last-char nil))
        (message "At recognized keyword; no need to reformat")
      (save-restriction
        (narrow-to-region first-char last-char)
        (fill-individual-paragraphs (point-min) (point-max))))))

;; misc. functions
(defun vtex-insert-backquote ()
  "Insert ``|'' at point, and move the cursor to |."
  (interactive)
  (progn
    (insert "``")
    (insert "''")
    (goto-char (- (point) 2))))

;; mode configuration
(defvar vtex-mode-hook nil)

(defvar vtex-mode-map
  (let ((vtm (make-keymap)))
    (define-key vtm "`" 'vtex-insert-backquote)
    (define-key vtm "\M-q" 'vtex-fill-paragraph)
    (define-key vtm "\C-j" 'newline-and-indent)
    vtm)
  "Keymap for VTEX major mode")

(define-derived-mode vtex-mode prog-mode
  "Major mode for editing LaTeX files."
  :syntax-table vtex-syntax-table
  (progn
    (setq font-lock-defaults vtex-font-lock-keywords)
    (setq comment-start "%")
    (setq comment-end "")
    (use-local-map vtex-mode-map)
    (set (make-local-variable 'font-lock-defaults) '(vtex-font-lock-keywords))
    (set (make-local-variable 'indent-line-function) 'vtex-indent-line)
    (set (make-local-variable 'indent-region) 'vtex-indent-region)
    (set (make-local-variable 'adaptive-fill-regexp) nil)
    ; should this be set as local?
    (set (make-local-variable 'fill-paragraph-function) 'vtex-fill-paragraph)
    (setq major-mode 'vtex-mode)
    (setq mode-name "VTEX")
    (add-hook 'before-save-hook
              (lambda () (progn (font-lock-fontify-buffer))) nil t)
    (add-hook 'before-save-hook
              (lambda () (vtex-align-continuation-right-buffer)) nil t)))

(provide 'vtex-mode)
;;; vtex-mode.el ends here
