;;; autoload/oga-docusaurus.el --- Custom Docusaurus helpers -*- lexical-binding: t; -*-

;;;###autoload
(defun oga/docusaurus-convert-from-chatgpt (start end)
  "Perform a series of replacements in the selected region.
START and END specify the region boundaries."
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
    (goto-char (point-min))

    ))


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



