;;; early-init.el --- Pre-init config (Emacs 30+) -*- lexical-binding: t; -*-
;;; Runs before package.el and frame rendering — ideal for startup perf + UI.

;; Maximise GC threshold during startup (restored in init.el)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent package.el from auto-loading before init.el takes over
(setq package-enable-at-startup nil)

;; Suppress UI chrome before the frame is drawn (avoids flicker)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Native compilation settings (Emacs 30 has this built in)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-jit-compilation t))

;; Don't resize the frame to preserve exact column/row counts
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
