;;package.el
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(package-initialize)

                                        ; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

                                        ; install the missing packages
(dolist (package my-package-packages)
  (when (not (package-installed-p package))
    (package-install package)))



;; Helm
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)

;; autopair
(electric-pair-mode t)

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


;; handlebars indentation for html/sgml
(require 'handlebars-sgml-hacks)


;; yasnippets
(yas-global-mode 1)

;; (add-hook 'js2-mode-hook
;;           '(lambda ()
;;              (yas-minor-mode)))

(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
(setq yas-prompt-functions '(yas-ido-prompt))


;; Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
(define-key ac-complete-mode-map "\r" nil)
(setq ac-auto-show-menu 0.1)
(setq ac-quick-help-delay 0.3)

;; CSS Mode
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

;; SCSS Mode
                                        ;(add-hook 'scss-mode-hook 'flymake-mode)
(add-hook 'scss-mode-hook 'auto-complete-mode) ; that's a weird one...
(add-hook 'scss-mode-hook 'rainbow-mode)
(require 'my-aj-compilation) ;add-on (hides compilation buffer); added hide-exceptions (e.g. "ack")
(setq scss-compile-at-save nil)

;;rainbow-delimiter
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;ace-jump-mode
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

;;python
(when (executable-find "ipython")
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

;;erc
(setq erc-track-enable-keybindings nil)

;; org-mode
(setq org-log-done t)
(setq org-pomodoro-play-sounds nil)



(defun my-c++-mode-hook ()
  (setq indent-tabs-mode t
        c-basic-offset 2)
  (auto-fill-mode)
  (c-set-offset 'substatement-open 0))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)


;;projectile
(projectile-global-mode)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)



