;;;;;;;;;;;;;;;;;;;; PACKAGES ;;;;;;;;;;;;;;;;;;;

(setq my-el-get-packages '(el-get js2-mode dash s multiple-cursors org-pomodoro
                                  auto-complete js2-refactor emacs-http-server scss-mode
                                  ctags rainbow-delimiters rainbow-mode ace-jump-mode emacs-w3m))

(setq my-package-packages '(csharp-mode magit yaml-mode csv-mode))
(add-to-list 'load-path "~/.emacs.d")

;;;;;;;;;;;;;;;;;; GLOBAL SETTINGS ;;;;;;;;;;;;;;;

(global-font-lock-mode 1)
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq tramp-default-method "ssh")
(global-auto-revert-mode)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;;;;;;;;;;;;;; MODE-EXTENSION MAPPINGS ;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.less" . css-mode))
(add-to-list 'auto-mode-alist '("\\.srml" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . sgml-mode))

;;;;;;;;;;;;;;;;;;;;;; HOOKS ;;;;;;;;;;;;;;;;;;;;;

(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(add-hook 'html-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'indentation-config nil :noerror)
;; (require 'my-package-configs)
;; (require 'my-handy-functions)
;; (require 'my-keybindings)
;; (require 'my-macros)
;; (require 'my-local-macros nil :noerror)
;; (require 'my-faces)

(load "indentation-config" t)
(load "my-package-configs")
(load "my-handy-functions")
(load "my-keybindings")
(load "my-macros")
(load "my-local-macros" t)
(load "my-faces")


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
