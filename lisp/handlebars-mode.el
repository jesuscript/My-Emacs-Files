;;; handlebars-mode.el --- A major mode for editing Handlebars files. -*- lexical-binding: t; -*-

;; Author: Tony Gentilcore, Chris Wanstrath, Daniel Hackney, Daniel Evans
;; Version: 1.4

;; This file is not part of Emacs.
;; This file is free software under the GPL v2+.

;;; Commentary:
;; Provides syntax highlighting and indentation for Handlebars templates.

(eval-when-compile
  (require 'font-lock))

(defgroup handlebars nil
  "Handlebars template mode."
  :group 'languages)

(defface handlebars-mode-section-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for Handlebars section tags."
  :group 'handlebars)

(defface handlebars-mode-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for Handlebars comments."
  :group 'handlebars)

(defface handlebars-mode-include-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for Handlebars include/partial tags."
  :group 'handlebars)

(defface handlebars-mode-builtins-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for Handlebars builtins."
  :group 'handlebars)

(defface handlebars-mode-variable-face
  '((t (:inherit font-lock-constant-face)))
  "Face for Handlebars variables."
  :group 'handlebars)

(defvar handlebars-mode-version "1.4")

(defvar handlebars-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'reindent-then-newline-and-indent)
    (define-key map "\C-ct" 'handlebars-insert-tag)
    (define-key map "\C-cv" 'handlebars-insert-variable)
    (define-key map "\C-cs" 'handlebars-insert-section)
    map)
  "Keymap for handlebars-mode.")

(defvar handlebars-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?<  "(>  " st)
    (modify-syntax-entry ?>  ")<  " st)
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\\ ".   " st)
    (modify-syntax-entry ?'  "w   " st)
    st))

(defvar handlebars-basic-offset 2
  "The basic indentation offset.")

(defconst handlebars-mode-handlebars-token
  "\\([a-zA-Z_.][a-zA-Z0-9_:=\?!.-]*\s+\\)*[a-zA-Z_.][a-zA-Z0-9_:=\?!.-]*")

(defconst handlebars-mode-section
  (concat "\\({{[#^/]\s*" handlebars-mode-handlebars-token "\s*}}\\)"))
(defconst handlebars-mode-open-section
  (concat "\\({{#\s*" handlebars-mode-handlebars-token "\s*}}\\)"))
(defconst handlebars-mode-close-section
  (concat "{{/\\(\s*" handlebars-mode-handlebars-token "\s*\\)}}"))
(defconst handlebars-mode-comment "\\({{!.*?}}\\)")
(defconst handlebars-mode-include
  (concat "\\({{[><]\s*" handlebars-mode-handlebars-token "\s*}}\\)"))
(defconst handlebars-mode-variable
  (concat "\\({{{?\s*" handlebars-mode-handlebars-token "\s*}}}?\\)"))
(defconst handlebars-mode-builtins
  (concat "\\({{\\<\s*" (regexp-opt '("BI_NEWLINE" "BI_SPACE") t) "\s*\\>}}\\)"))
(defconst handlebars-mode-close-section-at-start
  (concat "^[ \t]*?" handlebars-mode-close-section))

(defconst handlebars-mode-html-constant "\\(&#?[a-z0-9]\\{2,5\\};\\)")
(defconst handlebars-mode-pair-tag
  (concat "\\<"
          (regexp-opt
           '("a" "abbr" "acronym" "address" "applet" "area" "b" "bdo"
             "big" "blockquote" "body" "button" "caption" "center" "cite"
             "code" "col" "colgroup" "dd" "del" "dfn" "dif" "div" "dl"
             "dt" "em" "fieldset" "font" "form" "frame" "frameset" "h1"
             "header" "nav" "footer" "section"
             "h2" "h3" "h4" "h5" "h6" "head" "html" "i" "iframe" "ins"
             "kbd" "label" "legend" "li" "link" "map" "menu" "noframes"
             "noscript" "object" "ol" "optgroup" "option" "p" "pre" "q"
             "s" "samp" "script" "select" "small" "span" "strike"
             "strong" "style" "sub" "sup" "table" "tbody" "td" "textarea"
             "tfoot" "th" "thead" "title" "tr" "tt" "u" "ul" "var")
           t)
          "\\>"))
(defconst handlebars-mode-standalone-tag
  (concat "\\<"
          (regexp-opt '("base" "br" "hr" "img" "input" "meta" "param") t)
          "\\>"))
(defconst handlebars-mode-open-tag
  (concat "<\\(" handlebars-mode-pair-tag "\\)"))
(defconst handlebars-mode-close-tag
  (concat "</\\(" handlebars-mode-pair-tag "\\)>"))
(defconst handlebars-mode-close-tag-at-start
  (concat "^[ \t]*?" handlebars-mode-close-tag))
(defconst handlebars-mode-blank-line "^[ \t]*?$")
(defconst handlebars-mode-else-line "^[ \t]*?{{[ \t]*?else[ \t]*?}}")
(defconst handlebars-mode-dangling-open
  (concat "\\(" handlebars-mode-open-section "\\)\\|\\("
          handlebars-mode-open-tag "\\)[^/]*$"))

(defun handlebars-insert-tag (tag)
  "Insert an HTML TAG pair."
  (interactive "sTag: ")
  (handlebars-indent)
  (insert (concat "<" tag ">\n\n</" tag ">"))
  (handlebars-indent)
  (forward-line -1)
  (handlebars-indent))

(defun handlebars-insert-variable (variable)
  "Insert a Handlebars VARIABLE."
  (interactive "sVariable: ")
  (insert (concat "{{" variable "}}")))

(defun handlebars-insert-section (section)
  "Insert a Handlebars SECTION block."
  (interactive "sSection: ")
  (handlebars-indent)
  (insert (concat "{{#" section "}}\n\n{{/" section "}}"))
  (handlebars-indent)
  (forward-line -1)
  (handlebars-indent))

(defun handlebars-indent ()
  "Indent current line for Handlebars mode."
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((tag-stack 1) (close-tag "") (cur-indent 0) (old-pnt (point-marker))
          (close-at-start) (open-token))
      (if (looking-at "^[ \t]*?{{")
          (setq close-at-start handlebars-mode-close-section-at-start
                open-token "{{#")
        (setq close-at-start handlebars-mode-close-tag-at-start
              open-token "<"))
      (if (looking-at close-at-start)
          (progn
            (save-excursion
              (setq close-tag (match-string 1))
              (while (and (not (bobp))
                          (> tag-stack 0)
                          (re-search-backward
                           (concat (replace-regexp-in-string "{{#" "{{#?" open-token)
                                   "\\(/?\\)" close-tag)
                           nil t))
                (if (string-equal (match-string 1) "/")
                    (setq tag-stack (1+ tag-stack))
                  (setq tag-stack (1- tag-stack)))
                (setq cur-indent (current-indentation))))
            (when (> tag-stack 0)
              (save-excursion
                (forward-line -1)
                (setq cur-indent (current-indentation)))))
        (save-excursion
          (while (progn
                   (forward-line -1)
                   (and (not (bobp)) (looking-at handlebars-mode-blank-line))))
          (setq cur-indent (current-indentation))
          (when (or (re-search-forward handlebars-mode-dangling-open old-pnt t)
                    (looking-at handlebars-mode-else-line))
            (setq cur-indent (+ cur-indent handlebars-basic-offset)))))
      (when (looking-at handlebars-mode-else-line)
        (setq cur-indent (- cur-indent handlebars-basic-offset)))
      (indent-line-to (max 0 cur-indent)))))

(defconst handlebars-mode-font-lock-keywords
  `((,handlebars-mode-section (1 'handlebars-mode-section-face))
    (,handlebars-mode-comment (1 'handlebars-mode-comment-face))
    (,handlebars-mode-include (1 'handlebars-mode-include-face))
    (,handlebars-mode-builtins (1 'handlebars-mode-builtins-face))
    (,handlebars-mode-variable (1 'handlebars-mode-variable-face))
    (,(concat "</?\\(" handlebars-mode-pair-tag "\\)") (1 font-lock-function-name-face))
    (,(concat "<\\(" handlebars-mode-standalone-tag "\\)") (1 font-lock-function-name-face))
    (,handlebars-mode-html-constant (1 font-lock-variable-name-face))))

;;;###autoload
(define-derived-mode handlebars-mode fundamental-mode "Handlebars"
  "Major mode for editing Handlebars templates."
  (setq-local indent-line-function #'handlebars-indent)
  (setq-local indent-tabs-mode nil)
  (setq-local font-lock-defaults '(handlebars-mode-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . handlebars-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . handlebars-mode))

(provide 'handlebars-mode)
;;; handlebars-mode.el ends here
