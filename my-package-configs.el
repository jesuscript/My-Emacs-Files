;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))


(push '(:name yasnippet
              :website "https://github.com/capitaomorte/yasnippet.git"
              :description "YASnippet is a template system for Emacs."
              :type github
              :pkgname "capitaomorte/yasnippet"
              :features "yasnippet"
              :compile "yasnippet.el")
      el-get-sources)


(setq my-packages (append my-el-get-packages
                          (mapcar 'el-get-source-name el-get-sources)))

(el-get-cleanup my-packages)
(el-get 'sync my-packages)

;;package.el
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

                                        ; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

                                        ; install the missing packages
(dolist (package my-package-packages)
  (when (not (package-installed-p package))
    (package-install package)))



;; Wanderlust
(add-to-list 'load-path "~/.emacs.d/emacs-w3m")
(require 'w3m)
(setq mm-text-html-renderer 'w3m)
                                        ;(require 'mime-w3m)
(autoload 'wl "wl" "Wanderlust" t)


;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))


;; Helm
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)

;; autopair
(electric-pair-mode t)

;; coffeescript
(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)
(custom-set-variables '(coffee-tab-width 2))
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup)) 
;; only show bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) 

;;ZenCoding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

(add-hook 'ecmascript-mode-hook
          (lambda ()
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-close 0)))

;;rsense
(setq rsense-home "~/.emacs.d/rsense")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

;;project grep
(require 'project-grep)

;;handlebars
(require 'handlebars-mode)

;;csharp-mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;;ack
(add-to-list 'load-path "~/.emacs.d/ack-el")
(require 'ack)
(autoload 'pcomplete/ack "pcmpl-ack")
(autoload 'pcomplete/ack-grep "pcmpl-ack")

;;project-root
(require 'project-root)
(setq project-roots
      `(
        ("ASP.NET project"
         :root-contains-files ("Web.config")
         :filename-regex ,(regexify-ext-list '(html css js cs aspx ascs ashx resx))
         )
        ("Any project"
         :root-contains-files (".project-root")
         )
        ("Meteor Project"
         :root-contains-files (".meteor")
         )
        ("Emacs project"
         :root-contains-files ("init.el")
         )
        )
      )

;;js2-mode
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

;; handlebars indentation for html/sgml
(require 'handlebars-sgml-hacks)


;; yasnippets
(yas-global-mode 1)

(add-hook 'js2-mode-hook
          '(lambda ()
             (yas-minor-mode)))

(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))

;; Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
(define-key ac-complete-mode-map "\r" nil)
(setq ac-auto-show-menu 0.1)
(setq ac-quick-help-delay 0.3)


;; PHP Mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

;; CSS Mode
(add-hook 'css-mode-hook 'rainbow-mode)

;; SCSS Mode
(add-hook 'scss-mode-hook 'flymake-mode)
(add-hook 'scss-mode-hook 'auto-complete-mode) ; that's a weird one...
(add-hook 'scss-mode-hook 'rainbow-mode)
(require 'my-aj-compilation) ;add-on (hides compilation buffer); added hide-exceptions (e.g. "ack")

;;tags
(require 'ctags)
(setq tags-case-fold-search nil)

;;rainbow-delimiter
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;ace-jump-mode
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))




(provide 'my-package-configs)
