;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LilyPond-pdf-command "open")
 '(js2-strict-inconsistent-return-warning nil)
 '(package-selected-packages
   '(apheleia avy cape corfu diff-hl embark-consult gdscript-mode gptel
              inheritenv js2-refactor kind-icon magit marginalia
              orderless rg rustic toml-mode vertico vterm web-mode
              yaml-mode))
 '(safe-local-variable-values
   '((eval pyvenv-activate "./.venv")
     (python-shell-interpreter-args . "-i --simple-prompt")
     (python-shell-interpreter . "ipython"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :background "#ffffff" :foreground "#000000"))))
 '(diff-file-header ((t (:background "grey70" :foreground "black" :weight bold))))
 '(fringe ((t (:background "black" :foreground "brightwhite"))))
 '(js2-external-variable ((t (:foreground "#aaa"))))
 '(org-document-info ((t (:foreground "#cccccc"))))
 '(org-table ((t (:foreground "#cccccc"))))
 '(region ((t (:extend t :background "dark gray"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#00ff77"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#ff2299"))))
 '(web-mode-html-tag-face ((t (:foreground "#0077ff")))))
