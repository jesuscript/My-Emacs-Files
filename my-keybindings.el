(global-set-key (kbd "M-]") 'select-next-window)
(global-set-key (kbd "M-[")  'select-previous-window)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-h" 'backward-delete-char)

(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-x\C-o" 'goto-line)

(define-key minibuffer-local-map "\C-x\C-k" 'kill-region)
(define-key minibuffer-local-map "\C-x\C-o" 'goto-line)

(define-key isearch-mode-map [(control h)] 'isearch-delete-char)

(global-set-key (kbd "C-c ; C-w") 'rinari-web-server-restart)

(global-set-key (kbd "C-c . g") 'project-grep)

(global-set-key (kbd "C-c p a") 'project-root-ack)
(global-set-key (kbd "C-c p f") 'project-root-find-file)
(global-set-key (kbd "C-c p l") 'project-root-browse-seen-projects)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-z") 'undo)

(define-key zencoding-mode-keymap (kbd "C-j") nil)
(define-key zencoding-mode-keymap (kbd "M-j") 'zencoding-expand-line)

(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)

(global-unset-key (kbd "C-t"))

(js2r-add-keybindings-with-prefix "C-c C-j")
(js2r-add-keybindings-with-prefix "C-c j")

(define-key js2-mode-map (kbd "C-c e") 'js2-display-error-list)
(define-key js2-mode-map (kbd "C-j") 'js2-line-break)

(define-key yas-minor-mode-map (kbd "M-s") 'yas-ido-expand)

(local-set-key (kbd "M-f") 'yas-next-field)

(global-set-key (kbd "C-c C-c") 'ctags-create-or-update-tags-table)

(global-set-key (kbd "C-x r") 'rename-file-and-buffer)

(global-set-key (kbd "C-c r") 'replace-string)

(defun my-yas-key-mapping-hook ()
  (local-set-key (kbd "M-f") 'yas-next-field)
  (local-set-key (kbd "M-b") 'yas-prev-field)
  )

(defun my-yas-key-unmapping-hook ()
  (local-unset-key (kbd "M-f"))
  (local-unset-key (kbd "M-b")))

;;yasnippet
(add-hook 'yas-before-expand-snippet-hook 'my-yas-key-mapping-hook)
(add-hook 'yas-after-exit-snippet-hook 'my-yas-key-unmapping-hook)

;; ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-word-mode)
(define-key global-map (kbd "C-c h SPC") 'ace-jump-char-mode)
(define-key global-map (kbd "C-c l SPC") 'ace-jump-line-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; get rid of the fucking windows new line characters in the buffer ()
(define-key global-map (kbd "C-c w r") 'remove-windows-new-line-chars)

;;etags
(global-unset-key (kbd "M-."))
(global-set-key (kbd "M-.") 'find-tag-other-window)
(global-set-key (kbd "C-c M-.") 'find-tag)

(provide 'my-keybindings)
