;;; highlight-html.el --- Syntax highlight HTML code blocks -*- lexical-binding: t; -*-
;;
;; This file is part of highlight-hugo.
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Takes source code and converts it into syntax highlighted HTML.
;;
;; Right now highlight-html.el uses htmlize.el internally. Ideally we should
;; probably use engrave-faces.el and get rid of this file entirely.
;;
;;; Code:

(require 'htmlize)

(defgroup highlight-html nil
  "Convert source code into syntax highlighted HTML."
  :group 'hypermedia)

(defcustom highlight-html-code-has-html-entities t
  "Whether code blocks contain HTML entities.
Non-nil means the HTML entities in the original code will be
converted to UTF-8 characters before syntax highlighting is
applied."
  :group 'highlight-html
  :type 'boolean)

(defcustom highlight-html-lang-modes-map
  '(("C" . c)
    ("C++" . c++)
    ("cpp" . c++)
    ("shell" . sh)
    ("xml" . sgml))
  "Alist mapping languages to their major mode.
The key is the language name. The value is the mode name, as a
string or a symbol, without the \"-mode\" suffix."
  :group 'highlight-html
  :type '(repeat
          (cons
           (string :tag "Language")
           (symbol :tag "Major mode"))))

(defvar highlight-html-last-lang-input nil
  "Last used language in highlight-html-replace-in-region.")

(defun highlight-html-major-mode-for-lang (lang)
  "Return the major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol. The
function first tries to find the language in the language to
major mode map. If no suitable major mode has been found, the
returned major mode name is the language name concatenated with
-mode."
  (intern
   (concat
    (let ((l (or (cdr (assoc lang highlight-html-lang-modes-map)) lang)))
      (if (symbolp l) (symbol-name l) l))
    "-mode")))

(defun highlight-html-entities-to-utf8 (str)
  "Convert HTML entities in STR to UTF-8 characters."
  (replace-regexp-in-string
   "&amp;" "&"
   (replace-regexp-in-string
    "&lt;" "<"
    (replace-regexp-in-string
     "&gt;" ">"
     (replace-regexp-in-string
      "&quot;" "\""
      (replace-regexp-in-string
       "&apos;" "'"
       str))))))

(defun highlight-html-prettify-string-htmlize (str major)
  "Return the highlighted HTML of STR using MAJOR mode.
This function requires htmlize-buffer from htmlize.el by Hrvoje Niksic.

Original code and idea to use htmlize mostly from Xah Lee:
http://xahlee.info/emacs/emacs/elisp_htmlize.html"
  (let (output-buf result-str)
    (with-temp-buffer
      (insert (if highlight-html-code-has-html-entities
                  (highlight-html-entities-to-utf8 str)
                str))
      ;; Use fundamental-mode if the requested major mode doesn't exist
      (if (fboundp major) (funcall major) (fundamental-mode))
      (font-lock-ensure)
      (setq output-buf (htmlize-buffer)))
    (with-current-buffer output-buf
      (let (p1 p2)
        (setq p1 (search-forward "<pre>"))
        (setq p2 (search-forward "</pre>"))
        (setq result-str (buffer-substring-no-properties (+ p1 1) (- p2 6)))))
    (kill-buffer output-buf)
    result-str))

;;;###autoload
(defun highlight-html-replace-in-region (beg end lang)
  "Apply syntax highlighting to a region between BEG and END using LANG."
  (interactive
   (let ((str (read-string "Language: " highlight-html-last-lang-input)))
     (list (region-beginning) (region-end) str)))
  (setq highlight-html-last-lang-input lang)
  (let* ((major (highlight-html-major-mode-for-lang lang))
         (input-str (buffer-substring-no-properties beg end))
         (output-str (highlight-html-prettify-string-htmlize input-str major)))
    (delete-region beg end)
    (insert output-str)))

(provide 'highlight-html)

;;; highlight-html.el ends here
