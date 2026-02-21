;;; my-faces.el --- Face customisations -*- lexical-binding: t; -*-

;; Core UI faces — white on dark theme
(set-face-foreground 'button "#ffffff")
(set-face-foreground 'link "#ffffff")
(set-face-foreground 'minibuffer-prompt "#ffffff")

;; Vertico current-selection highlight
(with-eval-after-load 'vertico
  (set-face-attribute 'vertico-current nil :background "dark gray"))

;; Avy (replaces ace-jump-mode)
(with-eval-after-load 'avy
  (setq avy-background t))

(provide 'my-faces)
;;; my-faces.el ends here
