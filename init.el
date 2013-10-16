;;;;;;;;;;;;;;;;;;;; PACKAGES ;;;;;;;;;;;;;;;;;;;

(setq my-el-get-packages '(el-get js2-mode dash s multiple-cursors
                                  auto-complete js2-refactor emacs-http-server scss-mode
                                  ctags rainbow-delimiters rainbow-mode ace-jump-mode emacs-w3m
                                  ))
(setq my-package-packages '(csharp-mode jss))
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

;;;;;;;;;;;;;;;;;;;;;; HOOKS ;;;;;;;;;;;;;;;;;;;;;

(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(add-hook 'html-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'indentation-config)
(require 'my-package-configs)
(require 'my-handy-functions)
(require 'indentation-config)
(require 'my-keybindings)
(require 'my-macros)
(require 'my-local-macros nil :noerror)
(require 'my-faces)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-info ((t (:foreground "#cccccc"))))
 '(org-table ((t (:foreground "#cccccc"))))
 '(w3m-anchor ((t (:foreground "#cccccc"))))
 '(w3m-arrived-anchor ((t (:foreground "#ffffff"))))
 '(w3m-header-line-location-content ((t (:background "Gray90" :foreground "#000077"))))
 '(w3m-image-anchor ((t (:background "#550055"))))
 '(w3m-session-select ((t (:foreground "#999999"))))
 '(w3m-tab-unselected ((t (:background "blue" :foreground "#ffffff")))))
