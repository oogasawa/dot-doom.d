;;; autoload/oga-docusaurus.el --- Custom Docusaurus helpers -*- lexical-binding: t; -*-

;;;###autoload
(defun oga/docusaurus-convert-from-chatgpt (start end)
  "Clean up ChatGPT-generated Markdown content in the selected region.
This includes symbol replacements and removal of emojis from headings and list items."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))

    ;; Replace ** with an empty string
    (while (search-forward "**" nil t)
      (replace-match "" nil t))

    ;; Replace \( with $
    (goto-char (point-min))
    (while (search-forward "\\(" nil t)
      (replace-match "$" nil t))

    ;; Replace \) with $
    (goto-char (point-min))
    (while (search-forward "\\)" nil t)
      (replace-match "$" nil t))

    ;; Replace \[ with $$
    (goto-char (point-min))
    (while (search-forward "\\[" nil t)
      (replace-match "$$" nil t))

    ;; Replace \] with $$
    (goto-char (point-min))
    (while (search-forward "\\]" nil t)
      (replace-match "$$" nil t))

    ;; Replace ^--- with an empty string using regex
    (goto-char (point-min))
    (while (re-search-forward "^---" nil t)
      (replace-match "" nil nil))

    ;; Remove unwanted glyphs
    (goto-char (point-min))
    (while (re-search-forward "" nil t)
      (replace-match "" nil nil))

    (goto-char (point-min))
    (while (re-search-forward "" nil t)
      (replace-match "" nil nil))

    ;; Remove emoji-like characters from headings and list items
    ;; Match lines starting with #, -, *, + followed by emoji symbols
    (goto-char (point-min))
    (while (re-search-forward
            "^\\([ \t]*\\(?:[#]+\\|[-*+]\\)[ \t]+\\)\\(?:[\u2190-\u21FF\u2300-\u23FF\u2600-\u27BF\U0001F000-\U0001FAFF][[:space:]]*\\)+"
            nil t)
      (replace-match "\\1" nil nil))
    ))


;;;###autoload
(defun oga/docusaurus-open-i18n-file ()
  "Find and open the English file corresponding to the current file."
  (interactive)
  ;; Get the current file path
  (let ((current-file (buffer-file-name)))
    (if current-file
        (progn
          ;; Determine the path of the corresponding English file
          (let ((english-file (if (string-match "/docs/" current-file)
                                  (replace-regexp-in-string "/docs/" "/i18n/en/docusaurus-plugin-content-docs/" current-file)
                                (if (string-match "/i18n/en/docusaurus-plugin-content-docs/" current-file)
                                    current-file))))
            ;; Split the frame horizontally and open the English file
            (when english-file
              (split-window-horizontally)
              (other-window 1)
              (find-file english-file)
              (other-window 1))))
      (message "No file is currently being edited."))))



