;;; autoload/oga-shell.el -*- lexical-binding: t; -*-


;;;###autoload
(defun oga/shell-buffers-consult ()
  "Select a shell-mode buffer using consult and switch to it in the current window.
   Displays the selected buffer name in the minibuffer for debugging."
  (interactive)
  (let* ((shell-buffers (seq-filter (lambda (buf)
                                      (with-current-buffer buf
                                        (eq major-mode 'shell-mode)))
                                    (buffer-list))))
    (if shell-buffers
        (let ((selected-buffer-name (consult--read
                                     (mapcar #'buffer-name shell-buffers)
                                     :prompt "Switch to shell buffer: "
                                     :require-match t
                                     :category 'buffer)))
          (message "DEBUG: Selected buffer: %s (Type: %s)" selected-buffer-name (type-of selected-buffer-name))
          ;;(sit-for 1)  ;; ミニバッファメッセージを1秒間表示
          (when (stringp selected-buffer-name)  ;; 選択結果が文字列であることを確認
            (let ((selected-buffer (get-buffer selected-buffer-name)))
              (if (bufferp selected-buffer)
                  (switch-to-buffer selected-buffer)
                (message "Error: No buffer found with name '%s'" selected-buffer-name)))))
      (message "No shell buffers found."))))


;;;###autoload
(defun oga/shell-buffers-count ()
  "Display the number of open shell-mode buffers in the minibuffer."
  (interactive)
  (let ((count (length (seq-filter (lambda (buf)
                                     (with-current-buffer buf
                                       (eq major-mode 'shell-mode)))
                                   (buffer-list)))))
    (message "Shell buffers: %d" count)))



(defun oga/get-last-directory (path)
  "Return the name of the last directory in the given path."
  (unless (file-directory-p path)
    (error "Invalid directory path: %s" path))
  (file-name-nondirectory (directory-file-name (file-name-directory (expand-file-name path)))))



;; Start shell-mode with specifying a current directory.
;;;###autoload
(defun oga/shell-mode-in-dir (dir)
  "Starts a new shell in directory DIR."
  (interactive "DSet shell directory: ")
  (let* ((default-directory (expand-file-name dir))
         (main-dir-name (oga/get-last-directory default-directory)))
    (shell)
    (rename-buffer (concat "*shell(" main-dir-name ")*") t))
)


(defun oga/shell-display-buffer ()
  "Display the buffer starting with '*shell' in a window."
  (interactive)
  (let ((shell-buffer (cl-find-if (lambda (buf) (string-prefix-p "*shell" (buffer-name buf))) (buffer-list))))
    (when shell-buffer
      (let* ((num-lines (* 2 (/ (window-body-height) 3))) ; height of window is calculated based on the number of rows.
             (new-window (split-window-vertically num-lines)))
        (set-window-buffer new-window shell-buffer)))))




(defun oga/shell-buffer-name-list ()
  (seq-filter
   (lambda (s) (string-prefix-p "*shell" s))
   (seq-sort
    #'string-lessp
    (seq-map 'buffer-name (buffer-list)))))


(defun oga/insert-last-five-lines-from-buffers (buffer-list)
  "Insert the last five lines from each buffer in BUFFER-LIST into a new buffer."
  (let ((result-buffer (generate-new-buffer "*shell-dashboard.md*")))
    (dolist (buf-name buffer-list)
      (with-current-buffer buf-name
        (save-excursion
          (goto-char (point-max))
          (forward-line -5)
          (let ((start-pos (point)))
            (goto-char (point-max))
            (copy-region-as-kill start-pos (point)))
          (with-current-buffer result-buffer
                (insert (concat "\n\n## " buf-name "\n\n"))
                (insert "```\n")
                (yank)
                (insert "\n```\n")
            )
          )))
    (switch-to-buffer result-buffer)))

;;;###autoload
(defun oga/shell-dashboard ()
  (interactive)
  (when (get-buffer "*shell-dashboard.md*")
    (kill-buffer "*shell-dashboard.md*"))
  (oga/insert-last-five-lines-from-buffers (oga/shell-buffer-name-list)))



;; === general buffer operations ===


(defun oga/trunc (str)
  "Remove leading and trailing whitespace characters from a string."
  (replace-regexp-in-string "\\`[[:space:]]+\\|[[:space:]]+\\'" "" str))

;;;###autoload
(defun oga/buffer-open-with-word-at-point ()
  "Open a buffer with the word at point as the buffer name."
  (interactive)
  (save-excursion
    (let ((start (re-search-backward "[ \n]" nil t))
          (end (progn
                 (forward-char)
                 (re-search-forward "[ \n]" nil t))))
      (when (and start end)
        (let ((word (oga/trunc (buffer-substring-no-properties start end))))
          (print word)
          (switch-to-buffer word)
          )))))




