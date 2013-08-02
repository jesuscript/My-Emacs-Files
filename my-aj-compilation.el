;; Compilation mode
;; Compilation ;;;;;;;;;;;;;;;;;;;;;;;;;
(setq compilation-scroll-output t)
(setq compilation-window-height 17)

(setq compilation-ask-about-save nil)
(setq compilation-save-buffers-predicate '(lambda () nil))

(defvar aj-exceptions-list (list "find" "perl" "ack")
  "Exception commands for don't hide compilation window.")


(defvar aj-compilation-saved-window-configuration nil
  "Previous window conf from before a compilation")

(defvar aj-compile-command ""
  "The compile command used by compilation-start since
  `compile-command' is only saved by `compile' command.")

;; Hide *compilation* buffer if compile didn't give errors
(defadvice compilation-start (before aj-compilation-save-window-configuration(command comint))
  "Save window configuration before compilation in
`aj-compilation-saved-window-configuration'"

  ;; compile command is not saved in compilation-start function only in
  ;; compile function (rgrep only uses compilation-start)
  (setq aj-compile-command command)
  ;; Save window configuration
  (setq aj-compilation-saved-window-configuration
        (current-window-configuration)))

(ad-activate 'compilation-start)


(defun aj-is-exception (ex-list aj-compile-command)
  "Search through exceptions list `ex-list'.
Used to decide whether or not hide compilation window."
  (if (> (length ex-list) 0)
      (let* ((ex-position (string-match (car ex-list) aj-compile-command))
             ;; Not nil and not 0 means that command was "find" at
             ;; pos 0 which means that I don't want to restore the layout
             (is-exception (and (integerp ex-position) (zerop ex-position))))
        (if is-exception 
            t
          (aj-is-exception (cdr ex-list) aj-compile-command)))
    nil))

;; compilation-handle-exit returns (run-hook-with-args
;; 'compilation-finish-functions cur-buffer msg) Could use but it only
;; got a string describing status
(defadvice compilation-handle-exit
  (after aj-compilation-exit-function(process-status exit-status msg))
  "Hack to restore window conf"
  (when (and (eq process-status 'exit)
             (zerop exit-status)
             (not (aj-is-exception aj-exceptions-list aj-compile-command)))
    (set-window-configuration aj-compilation-saved-window-configuration)))

(ad-activate 'compilation-handle-exit)

(provide 'my-aj-compilation)
