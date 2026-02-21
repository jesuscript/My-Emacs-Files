;;; my-keybindings.el --- Personal keybindings -*- lexical-binding: t; -*-

;; ── Eval ──
(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'eval-buffer)

;; ── Motion & editing ──
(global-set-key (kbd "C-j") #'newline-and-indent)
(global-set-key (kbd "C-a") #'back-to-indentation)
(global-set-key (kbd "M-m") #'move-beginning-of-line)

(global-set-key (kbd "C-w") #'backward-kill-word)
(global-set-key (kbd "C-h") #'backward-delete-char)

(global-set-key (kbd "C-x C-k") #'kill-region)
(global-set-key (kbd "C-x C-o") #'goto-line)

(global-set-key (kbd "C-z") #'undo)
(global-unset-key (kbd "C-t"))

;; ── Window navigation ──
;; In terminal mode, M-[ and M-] are set up in init.el via
;; input-decode-map to coexist with CSI escape sequences.
(when (display-graphic-p)
  (global-set-key (kbd "M-]") #'select-next-window)
  (global-set-key (kbd "M-[") #'select-previous-window))

;; ── Registers ──
(global-set-key (kbd "C-x g g") #'point-to-register)
(global-set-key (kbd "C-x g j") #'jump-to-register)

;; ── Minibuffer ──
(define-key minibuffer-local-map (kbd "C-x C-k") #'kill-region)
(define-key minibuffer-local-map (kbd "C-x C-o") #'goto-line)

;; ── Isearch ──
(define-key isearch-mode-map (kbd "C-h") #'isearch-delete-char)
(add-hook 'isearch-mode-end-hook #'my-goto-match-beginning)

;; ── File / buffer ops ──
(global-set-key (kbd "C-x r")   #'rename-file-and-buffer)
(global-set-key (kbd "C-c r")   #'replace-string)
(global-set-key (kbd "C-c c")   #'pbcopy)
(global-set-key (kbd "C-c v")   #'pbpaste)
(global-set-key (kbd "C-c w r") #'remove-windows-new-line-chars)
(global-set-key (kbd "C-c [")   #'untabify-and-indent-buffer)

;; ── Password generator ──
(global-set-key (kbd "C-x p") #'password-generator-phonetic)

;; ── xref / tags ──
(global-set-key (kbd "M-.")     #'xref-find-definitions-other-window)
(global-set-key (kbd "C-c M-.") #'xref-find-definitions)

;; ── Org mode ──
(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o b") #'org-switchb)

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-j")     #'org-return)
            (define-key org-mode-map (kbd "M-j")     #'org-insert-todo-heading)
            (define-key org-mode-map (kbd "C-c u p") #'org-clock-update-time-maybe)))

;; ── Mode switching ──
(global-set-key (kbd "C-c m j") #'js2-mode)
(global-set-key (kbd "C-c m s") #'js2-jsx-mode)
(global-set-key (kbd "C-c m w") #'web-jsx-mode)

;; ── YASnippet ──
(with-eval-after-load 'yasnippet
  (when (boundp 'yas-minor-mode-map)
    (define-key yas-minor-mode-map (kbd "M-s") #'yas-completing-expand)
    (define-key yas-minor-mode-map (kbd "M-S") #'yas-insert-snippet))
  (add-hook 'yas-before-expand-snippet-hook #'my-yas-key-mapping-hook)
  (add-hook 'yas-after-exit-snippet-hook #'my-yas-key-unmapping-hook))

(defun my-yas-key-mapping-hook ()
  (local-set-key (kbd "M-f") #'yas-next-field)
  (local-set-key (kbd "M-b") #'yas-prev-field))

(defun my-yas-key-unmapping-hook ()
  (local-unset-key (kbd "M-f"))
  (local-unset-key (kbd "M-b")))

;; ── JS2 / JS2-Refactor ──
(with-eval-after-load 'js2-refactor
  (when (fboundp 'js2r-add-keybindings-with-prefix)
    (js2r-add-keybindings-with-prefix "C-c C-j")
    (js2r-add-keybindings-with-prefix "C-c j")))

(with-eval-after-load 'js2-mode
  (when (boundp 'js2-mode-map)
    (define-key js2-mode-map (kbd "C-c e")   #'js2-display-error-list)
    (define-key js2-mode-map (kbd "C-j")     #'js2-line-break)
    (define-key js2-mode-map (kbd "C-c m w") #'web-jsx-mode)))

;; ── Snippet editing ──
(with-eval-after-load 'snippet-mode
  (when (boundp 'snippet-mode-map)
    (define-key snippet-mode-map (kbd "C-c C-r") #'yas-reload-all)))

;; ── Ruby ──
(add-hook 'ruby-mode-hook
          (lambda () (local-set-key (kbd "RET") #'newline-and-indent)))
(add-hook 'html-mode-hook
          (lambda () (local-set-key (kbd "RET") #'newline-and-indent)))

(provide 'my-keybindings)
;;; my-keybindings.el ends here
