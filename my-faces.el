(defface erb-face
  `((t (:background "no")))
  "Default inherited face for ERB tag body"
  :group 'rhtml-faces)

(defface erb-delim-face
  `((t (:background "no")))
  "Default inherited face for ERB tag delimeters"
  :group 'rhtml-faces)


(set-face-foreground 'button "#ffffff")
(set-face-foreground 'comint-highlight-prompt "#ffffff")
(set-face-foreground 'link "#ffffff")
(set-face-foreground 'minibuffer-prompt "#ffffff")
(set-face-foreground 'tool-bar "#ffffff")

(setq ace-jump-mode-gray-background t)
(set-face-foreground 'ace-jump-face-foreground "#fff")

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


(provide 'my-faces)
