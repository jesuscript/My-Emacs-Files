;;; my-aj-compilation.el --- Compilation helpers with window restore -*- lexical-binding: t; -*-

(require 'seq)

(setq compilation-scroll-output t
      compilation-window-height 17
      compilation-ask-about-save nil
      compilation-save-buffers-predicate (lambda () nil))

(defvar aj-exceptions-list '("find" "perl" "ack")
  "Commands for which we do not restore window configuration on success.")

(defvar aj-compilation-saved-window-configuration nil
  "Window configuration saved before starting compilation.")

(defvar aj-compile-command ""
  "The compilation command used by `compilation-start'.")

(defun aj--compilation-start-before (&rest args)
  "Save window configuration and compilation command before compilation."
  (setq aj-compile-command (car args))
  (setq aj-compilation-saved-window-configuration (current-window-configuration)))

(defun aj--is-exception-p (command)
  "Return non-nil if COMMAND matches one of `aj-exceptions-list' at position 0."
  (seq-some (lambda (ex)
              (let ((pos (and (stringp command) (string-match ex command))))
                (and (integerp pos) (zerop pos))))
            aj-exceptions-list))

(defun aj--compilation-handle-exit-after (process-status exit-status _msg)
  "Restore window configuration when compilation succeeds."
  (when (and (eq process-status 'exit)
             (zerop exit-status)
             (not (aj--is-exception-p aj-compile-command))
             (window-configuration-p aj-compilation-saved-window-configuration))
    (set-window-configuration aj-compilation-saved-window-configuration)))

(advice-add 'compilation-start :before #'aj--compilation-start-before)
(advice-add 'compilation-handle-exit :after #'aj--compilation-handle-exit-after)

(provide 'my-aj-compilation)
;;; my-aj-compilation.el ends here
