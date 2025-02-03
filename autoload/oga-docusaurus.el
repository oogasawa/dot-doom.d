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
<<<<<<< HEAD
    ;; Replace ^# with ##
;;    (goto-char (point-min))
;;    (while (re-search-forward "^#" nil t)
;;      (replace-match "##" nil nil))
=======
>>>>>>> a7cda4723f581f5da127483a4d968aa6f6705843
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



