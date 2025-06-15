;;; autoload/oga-docusaurus.el --- Custom Docusaurus helpers -*- lexical-binding: t; -*-


;;;###autoload
(defun oga/docusaurus-convert-from-chatgpt (start end)
  "Perform a series of replacements in the selected region.
START and END specify the region boundaries."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)

    ;; Replace ** with an empty string
    (goto-char (point-min))
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

    ;; Replace line-starting '*' used for bullet points in Markdown with '-'
    (goto-char (point-min))
    (while (re-search-forward "^\\([ \t]*\\)\\*\\([ \t]+\\)" nil t)
      (replace-match "\\1-\\2"))))


(defun oga/docusaurus-replace-markdown-bullets-asterisk-to-dash ()
  "Replace line-starting '*' used for bullet points in Markdown with '-'."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\([ \t]*\\)\\*\\([ \t]+\\)" nil t)
      (replace-match "\\1-\\2"))))



;;;###autoload
(defun oga/docusaurus-remove-unwanted-chars-in-region ()
  "Remove all occurrences of known unwanted characters from the selected region.
Characters are defined in the `known-unwanted-chars` list."
  (interactive)
  (when (use-region-p)
    (let ((known-unwanted-chars '("" "" "" "" "" ""
                                  "✳️" "🔁" "📄" "🔍" "✅" "🎯" "🧪" "⚙️" "📝" "🚀"))
          (start (region-beginning))
          (end (copy-marker (region-end))))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (dolist (ch known-unwanted-chars)
              (setq line (replace-regexp-in-string (regexp-quote ch) "" line)))
            (delete-region (line-beginning-position) (line-end-position))
            (insert line))
          (forward-line 1))))))




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



