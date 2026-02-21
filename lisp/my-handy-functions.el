;;; my-handy-functions.el --- Utility functions -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun select-next-window ()
  "Switch to the next window."
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window."
  (interactive)
  (select-window (previous-window)))

(defun my-goto-match-beginning ()
  "After isearch ends, move point to the beginning of the match."
  (when (and (boundp 'isearch-other-end) isearch-other-end)
    (goto-char isearch-other-end)))

(defun rename-file-and-buffer (new-name)
  "Rename both current buffer and the file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((buf-name (buffer-name))
        (filename (buffer-file-name)))
    (cond
     ((not filename)
      (message "Buffer '%s' is not visiting a file!" buf-name))
     ((get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name))
     (t
      (rename-file filename new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil)))))

(defun yas-completing-expand ()
  "Select and expand a yasnippet key using completing-read.
Works with Vertico/Orderless out of the box."
  (interactive)
  (unless (fboundp 'yas-expand)
    (user-error "yasnippet not available"))
  (let ((original-point (point)))
    (while (and (not (= (point) (point-min)))
                (not (string-match-p "[[:space:]\n]" (char-to-string (char-before)))))
      (if (string-match-p "[_${}\\.]" (char-to-string (char-before)))
          (backward-char 1)
        (backward-word 1)))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (keys (yas-active-keys)))
      (goto-char original-point)
      (let* ((candidates (cl-remove-if-not
                          (lambda (s) (string-match-p (concat "^" (regexp-quote word)) s))
                          keys))
             (key (cond
                   ((= (length candidates) 1) (car candidates))
                   (t (completing-read "Snippet: " keys nil nil word)))))
        (delete-region init-word original-point)
        (insert key)
        (yas-expand)))))

(defun remove-windows-new-line-chars ()
  "Remove CR characters (^M) from Windows-style line endings."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "" nil t))))

(defun org-set-line-checkbox (arg)
  "Prefix lines in region (or ARG lines) with `- [ ] '."
  (interactive "P")
  (let ((n (or arg 1)))
    (when (use-region-p)
      (setq n (count-lines (region-beginning) (region-end)))
      (goto-char (region-beginning)))
    (dotimes (_ n)
      (beginning-of-line)
      (insert "- [ ] ")
      (forward-line 1))
    (beginning-of-line)))

(defun web-jsx-mode ()
  "Switch to web-mode with JSX engine."
  (interactive)
  (when (fboundp 'web-mode)
    (web-mode))
  (when (fboundp 'web-mode-set-engine)
    (web-mode-set-engine "jsx")))

(provide 'my-handy-functions)
;;; my-handy-functions.el ends here
