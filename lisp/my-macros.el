;;; my-macros.el --- Keyboard macros & buffer helpers -*- lexical-binding: t; -*-

(defun untabify-and-indent-buffer ()
  "Untabify and re-indent the entire buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defun tabify-and-indent-buffer ()
  "Tabify and re-indent the entire buffer."
  (interactive)
  (tabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defun open-line-and-indent ()
  "Open a new line above point and indent."
  (interactive)
  (newline-and-indent)
  (forward-line -1)
  (end-of-line)
  (indent-according-to-mode))

(global-set-key (kbd "C-o") #'open-line-and-indent)

;; macOS clipboard helpers
(defun pbcopy ()
  "Copy active region to macOS clipboard (pbcopy)."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "pbcopy is macOS-only"))
  (unless (use-region-p)
    (user-error "No active region"))
  (call-process-region (region-beginning) (region-end) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  "Paste from macOS clipboard (pbpaste) at point.
If region is active, replace it."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "pbpaste is macOS-only"))
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (call-process-region (point) (point) "pbpaste" t t))

(provide 'my-macros)
;;; my-macros.el ends here
