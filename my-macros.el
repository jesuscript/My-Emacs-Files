;;;;;;;;;;;;;;;;;;;;; MACROS ;;;;;;;;;;;;;;;;;;;;;

(fset 'erb-echo-tag
      "<%= %>\C-b\C-b\C-b ")
(global-set-key (kbd "C-c r i") 'erb-echo-tag)

(fset 'erb-tag
      "<% %>\C-b\C-b\C-b ")
(global-set-key (kbd "C-c r u") 'erb-tag)

(fset 'ack-js-regex
      "--type=js ")
(define-key minibuffer-local-map (kbd "C-c j") 'ack-js-regex)

(fset 'erb-end-tag
      "<% end %>")
(global-set-key (kbd "C-c r e") 'erb-end-tag)


(defun untabify-and-indent-buffer ()
  (interactive)
  (progn
    (untabify (point-min) (point-max))
    (indent-region (point-min) (point-max))
    )
  )

(global-set-key (kbd "C-c [") 'untabify-and-indent-buffer)

(provide 'my-macros)
