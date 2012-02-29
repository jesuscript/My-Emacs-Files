(setq load-path
      (append (list nil "/home/sedition2/.emacs.d")
	      load-path))

(global-font-lock-mode 1)

(setq js-indent-level 2)

(setq ruby-indent-level 2)
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

(add-hook 'html-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))


(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.less" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js.erb" . js-mode))


(setq auto-mode-alist
      (append '(("\\.php$" . php-mode)
		("\\.module$" . php-mode))
              auto-mode-alist))

(setq-default indent-tabs-mode nil)

(setq c-basic-offset 4)

(setq tramp-default-method "ssh")

(menu-bar-mode -1)

(setq x-select-enable-clipboard t)

(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)

(defun select-next-window ()
  "Switch to the next window" 
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window" 
  (interactive)
  (select-window (previous-window)))

(defface erb-face
  `((t (:background "no")))
  "Default inherited face for ERB tag body"
  :group 'rhtml-faces)

(defface erb-delim-face
  `((t (:background "no")))
  "Default inherited face for ERB tag delimeters"
  :group 'rhtml-faces)


;; Rinari
(add-to-list 'load-path "~/.emacs.d/rinari")
(require 'rinari)


;;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))


(global-set-key (kbd "M-]") 'select-next-window)
(global-set-key (kbd "M-[")  'select-previous-window)


(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

(add-hook 'ecmascript-mode-hook
          (lambda ()
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-close 0)))

(setq ruby-deep-indent-paren nil)


(setq rsense-home "/opt/rsense-0.3")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

;;;;;;;;;;;;;;;;;;;;; MACROS ;;;;;;;;;;;;;;;;;;;;;

(fset 'erb-echo-tag
      "<%= %>\C-b\C-b\C-b ")
(global-set-key (kbd "C-c i") 'erb-echo-tag)

(fset 'erb-tag
      "<% %>\C-b\C-b\C-b ")
(global-set-key (kbd "C-c u") 'erb-tag)

(fset 'erb-end-tag
      "<% end %>")
(global-set-key (kbd "C-c e") 'erb-end-tag)

(fset 'indent-all-file
      "\C-[<\C-@\C-[>\C-[\C-\\\C-u\C-@\C-u\C-@")
(global-set-key (kbd "C-c ]") 'indent-all-file)