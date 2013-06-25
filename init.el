;;;;;;;;;;;;;;;;;;;;; PACKAGES ;;;;;;;;;;;;;;;;;;;
(setq my-el-get-packages '(el-get wanderlust apel flim auto-complete js2-mode dash s multiple-cursors
                                  js2-refactor emacs-http-server))

(setq my-package-packages '(skewer-mode))





(setq load-path
      (append (list nil "$HOME/.emacs.d")
              load-path))
(add-to-list 'load-path "~/.emacs.d")


(global-font-lock-mode 1)


(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

(add-hook 'html-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))


(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.less" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js.erb" . js-mode))
(add-to-list 'auto-mode-alist '("\\.srml" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq auto-mode-alist
      (append '(("\\.php$" . php-mode)
                ("\\.module$" . php-mode))
              auto-mode-alist))

(setq-default indent-tabs-mode nil)


(setq tramp-default-method "ssh")

(menu-bar-mode -1)

(setq x-select-enable-clipboard t)

(global-auto-revert-mode)




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


(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when isearch-forward (goto-char isearch-other-end)))



;; indentation config
(require 'indentation-config)


;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))


(push '(:name yasnippet
              :website "https://github.com/capitaomorte/yasnippet.git"
              :description "YASnippet is a template system for Emacs."
              :type github
              :pkgname "capitaomorte/yasnippet"
              :features "yasnippet"
              :compile "yasnippet.el")
      el-get-sources)

(setq my-packages (append my-el-get-packages
                          (mapcar 'el-get-source-name el-get-sources)))

(el-get-cleanup my-packages)
(el-get 'sync my-packages)


;;package.el
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

; install the missing packages
(dolist (package my-package-packages)
  (when (not (package-installed-p package))
    (package-install package)))

;; Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

                                        ;(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)



;; Wanderlust
(add-to-list 'load-path "~/.emacs.d/emacs-w3m")
(require 'w3m)
(setq mm-text-html-renderer 'w3m)
                                        ;(require 'mime-w3m)
(autoload 'wl "wl" "Wanderlust" t)

;; Rinari
(add-to-list 'load-path "~/.emacs.d/rinari")
(require 'rinari)


;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))


;; Helm
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)

;; autopair
(electric-pair-mode t)

;; coffeescript
(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)
(custom-set-variables '(coffee-tab-width 2))
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup)) 
;; only show bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) 


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

;;ZenCoding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

(add-hook 'ecmascript-mode-hook
          (lambda ()
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-close 0)))

;;rsense
(setq rsense-home "~/.emacs.d/rsense")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

;;project grep
(require 'project-grep)

;;handlebars
(require 'handlebars-mode)

;;csharp-mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;;ack
(add-to-list 'load-path "~/.emacs.d/ack-el")
(require 'ack)
(autoload 'pcomplete/ack "pcmpl-ack")
(autoload 'pcomplete/ack-grep "pcmpl-ack")

;;project-root
(require 'project-root)
(setq project-roots
      `(
        ("ASP.NET project"
         :root-contains-files ("Web.config")
         :filename-regex ,(regexify-ext-list '(html css js cs aspx ascs ashx resx))
         )
        ("Any project"
         :root-contains-files (".project-root")
         )
        ("Meteor Project"
         :root-contains-files (".meteor")
         )
        ("Emacs project"
         :root-contains-files ("init.el")
         )
        )
      )

;;skewer-mode
(skewer-setup)

;;js2-mode
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

;; handlebars indentation for html/sgml
(require 'handlebars-sgml-hacks)

;; yasnippets
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))

(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-ido-expand)

 ;;;;;;;;;;;;;;;;;;;; BINDINGS ;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-]") 'select-next-window)
(global-set-key (kbd "M-[")  'select-previous-window)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-h" 'backward-delete-char)

(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-x\C-o" 'goto-line)

(define-key minibuffer-local-map "\C-x\C-k" 'kill-region)
(define-key minibuffer-local-map "\C-x\C-o" 'goto-line)

(define-key isearch-mode-map [(control h)] 'isearch-delete-char)

(global-set-key (kbd "C-c ; C-w") 'rinari-web-server-restart)

(global-set-key (kbd "C-c . g") 'project-grep)

(global-set-key (kbd "C-c p a") 'project-root-ack)
(global-set-key (kbd "C-c p f") 'project-root-find-file)
(global-set-key (kbd "C-c p l") 'project-root-browse-seen-projects)

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

(define-key zencoding-mode-keymap (kbd "C-j") nil)
(define-key zencoding-mode-keymap (kbd "M-j") 'zencoding-expand-line)

(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)

(global-unset-key (kbd "C-t"))

(js2r-add-keybindings-with-prefix "C-c C-j r")
(js2r-add-keybindings-with-prefix "C-c j r")

(define-key js2-mode-map (kbd "C-c C-j e") 'js2-display-error-list)
(define-key js2-mode-map (kbd "C-c j e") 'js2-display-error-list)
;;;;;;;;;;;;;;;;;;;;; MACROS ;;;;;;;;;;;;;;;;;;;;;


(fset 'erb-echo-tag
      "<%= %>\C-b\C-b\C-b ")
(global-set-key (kbd "C-c r i") 'erb-echo-tag)

(fset 'erb-tag
      "<% %>\C-b\C-b\C-b ")
(global-set-key (kbd "C-c r u") 'erb-tag)

(fset 'js-log
      "console.log();\C-b\C-b")
(global-set-key (kbd "C-c j c") 'js-log)
(global-set-key (kbd "C-c C-j c") 'js-log)

(fset 'js-debugger
      "debugger;")
(global-set-key (kbd "C-c j d") 'js-debugger)
(global-set-key (kbd "C-c C-j d") 'js-debugger)


(fset 'js-func
      "\C-bfunction(){\C-j\C-j\C-p\C-i\C-p\C-e\C-b\C-b")
(global-set-key (kbd "C-c j f") 'js-func)
(global-set-key (kbd "C-c C-j f") 'js-func)

(fset 'js-object-literal
      "\C-b{\C-j\C-j\C-p\C-i")
(global-set-key (kbd "C-c j o") 'js-object-literal)
(global-set-key (kbd "C-c C-j o") 'js-object-literal)


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

;;;;;;;;;;;;;;;;;;;;; FACES  ;;;;;;;;;;;;;;;;;;;;;

(set-face-foreground 'button "#ffffff")
(set-face-foreground 'comint-highlight-prompt "#ffffff")
(set-face-foreground 'link "#ffffff")
(set-face-foreground 'minibuffer-prompt "#ffffff")
(set-face-foreground 'tool-bar "#ffffff")

;;;;;;;;;;;;;;;;; EMACS BACKUPS  ;;;;;;;;;;;;;;;;;

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

