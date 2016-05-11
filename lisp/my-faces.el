(defface erb-face
  `((t (:background "no")))
  "Default inherited face for ERB tag body"
  :group 'rhtml-faces)

(defface erb-delim-face
  `((t (:background "no")))
  "Default inherited face for ERB tag delimeters"
  :group 'rhtml-faces)


(set-face-foreground 'button "#ffffff")
(set-face-foreground 'link "#ffffff")
(set-face-foreground 'minibuffer-prompt "#ffffff")
(set-face-foreground 'tool-bar "#ffffff")

(setq ace-jump-mode-gray-background t)
;(set-face-foreground 'ace-jump-face-foreground "#ffffff")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-html-attr-value-face ((t (:foreground "#ff2299"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#00ff77"))))
 '(web-mode-html-tag-face ((t (:foreground "#0077ff"))))
 '(bg:erc-color-face10 ((t (:background "color-18"))))
 '(bg:erc-color-face14 ((t (:background "black"))))
 '(diff-added ((t (:inherit diff-changed :background "#ffffff" :foreground "#000000"))))
 '(diff-file-header ((t (:background "grey70" :foreground "black" :weight bold))))
 '(diff-refine-change ((t (:background "color-45"))))
 '(erb-face ((t (:background "color-240"))))
 '(fg:erc-color-face1 ((t (:background "#ffffff" :foreground "black"))))
 '(fringe ((t (:background "black" :foreground "brightwhite"))))
 '(magit-diff-add ((t (:background "#ffffff" :foreground "green"))))
 '(magit-diff-del ((t (:background "#ffffff" :foreground "red"))))
 '(magit-item-highlight ((t (:background "black"))))
 '(magit-key-mode-button-face ((t (:foreground "#00ffff"))))
 '(org-document-info ((t (:foreground "#cccccc"))))
 '(org-pomodoro-mode-line ((t (:foreground "#cc0000"))))
 '(org-pomodoro-mode-line-break ((t (:foreground "#007700"))))
 '(org-table ((t (:foreground "#cccccc"))))
 '(w3m-anchor ((t (:foreground "#cccccc"))))
 '(w3m-arrived-anchor ((t (:foreground "#ffffff"))))
 '(w3m-header-line-location-content ((t (:background "Gray90" :foreground "#000077"))))
 '(w3m-image-anchor ((t (:background "#550055"))))
 '(w3m-session-select ((t (:foreground "#999999"))))
 '(w3m-tab-unselected ((t (:background "blue" :foreground "#ffffff")))))
