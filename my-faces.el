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

(provide 'my-faces)
