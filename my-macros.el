;;;;;;;;;;;;;;;;;;;;; MACROS ;;;;;;;;;;;;;;;;;;;;;

(fset 'ack-js-regex
      "--type=js ")
(define-key minibuffer-local-map (kbd "C-c j") 'ack-js-regex)


(defun untabify-and-indent-buffer ()
  (interactive)
  (progn
    (untabify (point-min) (point-max))
    (indent-region (point-min) (point-max))
    )
  )

(global-set-key (kbd "C-c [") 'untabify-and-indent-buffer)

(provide 'my-macros)
