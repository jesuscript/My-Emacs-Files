;;; init.el --- Main config (Emacs 30+) -*- lexical-binding: t; -*-

;;; ──────────────────────────────────────────────────────────────────
;;; Startup performance
;;; ──────────────────────────────────────────────────────────────────
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)
            (message "Emacs ready in %.2fs with %d GCs."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(setq create-lockfiles nil
      read-process-output-max (* 1024 1024))

;;; ──────────────────────────────────────────────────────────────────
;;; Load path
;;; ──────────────────────────────────────────────────────────────────
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; ──────────────────────────────────────────────────────────────────
;;; Package system
;;; ──────────────────────────────────────────────────────────────────
(require 'package)
(require 'seq)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(setq package-install-upgrade-built-in t)

(package-initialize)

;; Purge corrupted auto-mode-alist entries from prior broken sessions
(setq auto-mode-alist
      (seq-filter (lambda (entry)
                    (and (consp entry)
                         (stringp (car entry))))
                  auto-mode-alist))

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;;; ──────────────────────────────────────────────────────────────────
;;; Custom file
;;; ──────────────────────────────────────────────────────────────────
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;; ──────────────────────────────────────────────────────────────────
;;; Global defaults
;;; ──────────────────────────────────────────────────────────────────
(global-font-lock-mode 1)
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)
(column-number-mode 1)

(setq-default indent-tabs-mode nil
              tab-width 2)

(setq tramp-default-method "ssh"
      tramp-auto-save-directory "/tmp"
      uniquify-buffer-name-style 'forward)

(let ((backup-dir (locate-user-emacs-file "backup/")))
  (make-directory backup-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))
        backup-by-copying t
        version-control t
        delete-old-versions t
        kept-new-versions 20
        kept-old-versions 5))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;;; ──────────────────────────────────────────────────────────────────
;;; Completion (Vertico + Orderless + Marginalia + Consult + Embark)
;;; ──────────────────────────────────────────────────────────────────
(use-package vertico
  :init (vertico-mode 1)
  :config (setq vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :bind (("C-c s"   . consult-ripgrep)
         ("C-c b"   . consult-buffer)
         ("C-c C-s" . consult-line)))

(use-package embark
  :bind (("C-."  . embark-act)
         ("C-;"  . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; ──────────────────────────────────────────────────────────────────
;;; In-buffer completion (Corfu + Cape)
;;; ──────────────────────────────────────────────────────────────────
(use-package corfu
  :init (global-corfu-mode 1)
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0.1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package kind-icon
  :after corfu
  :if (display-graphic-p)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; ──────────────────────────────────────────────────────────────────
;;; Discoverability
;;; ──────────────────────────────────────────────────────────────────
(use-package which-key
  :init (which-key-mode 1)
  :config (setq which-key-idle-delay 0.4))

;;; ──────────────────────────────────────────────────────────────────
;;; LSP — Eglot (built-in)
;;; ──────────────────────────────────────────────────────────────────
(use-package eglot
  :ensure nil
  :hook ((rustic-mode   . eglot-ensure)
         (gdscript-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0))

;;; ──────────────────────────────────────────────────────────────────
;;; Rust
;;; ──────────────────────────────────────────────────────────────────
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot
        rustic-format-on-save nil))

;;; ──────────────────────────────────────────────────────────────────
;;; Godot — GDScript
;;; ──────────────────────────────────────────────────────────────────
(use-package gdscript-mode
  :mode ("\\.gd\\'" . gdscript-mode)
  :config
  (setq gdscript-godot-executable "/Applications/Godot.app/Contents/MacOS/Godot")
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(gdscript-mode . ("localhost" 6005)))))

;;; ──────────────────────────────────────────────────────────────────
;;; Git
;;; ──────────────────────────────────────────────────────────────────
(use-package magit
  :bind ("C-c g" . magit-status))

(use-package diff-hl
  :hook ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (global-diff-hl-mode 1))

;;; ──────────────────────────────────────────────────────────────────
;;; Search & navigation
;;; ──────────────────────────────────────────────────────────────────
(use-package rg
  :commands (rg rg-project)
  :config (rg-enable-default-bindings))

(use-package avy
  :bind (("C-c SPC"   . avy-goto-word-1)
         ("C-c C-SPC" . avy-goto-word-1)
         ("C-c h SPC" . avy-goto-char)
         ("C-c l SPC" . avy-goto-line)))

;;; ──────────────────────────────────────────────────────────────────
;;; Formatting
;;; ──────────────────────────────────────────────────────────────────
(use-package apheleia
  :init (apheleia-global-mode +1))

;;; ──────────────────────────────────────────────────────────────────
;;; Terminal
;;; ──────────────────────────────────────────────────────────────────
(use-package vterm
  :commands vterm
  :config
  (add-to-list 'vterm-keymap-exceptions "M-[")
  (add-to-list 'vterm-keymap-exceptions "M-]"))

;;; ──────────────────────────────────────────────────────────────────
;;; AI — Claude Code (agentic coding)
;;; ──────────────────────────────────────────────────────────────────

;; Claude Code — agentic assistant (runs CLI in vterm)
(use-package claude-code
  :ensure nil
  :load-path "~/.emacs.d/elpa/claude-code"
  :bind-keymap ("C-c q" . claude-code-command-map)
  :config
  (setq claude-code-terminal-backend 'vterm)
  (claude-code-mode 1))

;;; ──────────────────────────────────────────────────────────────────
;;; Language modes
;;; ──────────────────────────────────────────────────────────────────
(use-package yaml-mode     :mode "\\.ya?ml\\'")
(use-package markdown-mode :mode ("\\.md\\'" "\\.markdown\\'"))
(use-package web-mode      :mode ("\\.html?\\'" "\\.ejs\\'" "\\.cshtml\\'"))
(use-package js2-mode      :mode "\\.js\\'")
(use-package toml-mode     :mode "\\.toml\\'")

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
  (yas-reload-all))

(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode))

;;; ──────────────────────────────────────────────────────────────────
;;; Mode associations
;;; ──────────────────────────────────────────────────────────────────
(add-to-list 'auto-mode-alist '("\\.srml\\'"    . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'"    . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

;;; ──────────────────────────────────────────────────────────────────
;;; Personal modules (lisp/)
;;; ──────────────────────────────────────────────────────────────────
(load "indentation-config" t)
(load "my-aj-compilation"  t)
(load "my-handy-functions" t)
(load "my-macros"          t)
(load "my-keybindings"     t)
(load "my-faces"           t)
(load "my-local-macros"    t)

;;; ──────────────────────────────────────────────────────────────────
;;; Terminal fix — swallow focus events arriving after M-[
;;; ──────────────────────────────────────────────────────────────────
(unless (display-graphic-p)
  (global-set-key (kbd "M-[")
    (lambda ()
      "Run `select-previous-window', but swallow trailing I/O from focus events."
      (interactive)
      (let ((next (read-event nil nil 0.01)))
        (cond
         ((eq next ?I) nil)
         ((eq next ?O) nil)
         (next (push next unread-command-events)
               (select-previous-window))
         (t (select-previous-window))))))
  (global-set-key (kbd "M-]") #'select-next-window))

;;; init.el ends here
