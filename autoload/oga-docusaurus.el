;;; autoload/oga-docusaurus.el --- Custom Docusaurus helpers -*- lexical-binding: t; -*-


;;; autoload/new-doc.el --- Create a new document per ProjectStandards layout  -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst new-doc--doc-id-regexp
  "\\`[A-Za-z][A-Za-z0-9]*_[0-9]\\{6\\}_[a-z]+[0-9]+\\'"
  "Pattern matching DocumentID: <Description>_<YYMMDD>_<author>.")

(defconst new-doc--order-regexp
  "\\`[0-9]\\{3\\}\\'"
  "Pattern matching a 3-digit order prefix.")

(defun new-doc--suggest-order (dir)
  "Suggest next 10-step order prefix for DIR. Returns a 3-digit string."
  (let* ((entries (directory-files dir nil "\\`[0-9]\\{3\\}_"))
         (numbers (cl-loop for e in entries
                           when (string-match "\\`\\([0-9]\\{3\\}\\)_" e)
                           collect (string-to-number (match-string 1 e))))
         (next (if numbers
                   (* 10 (1+ (/ (apply #'max numbers) 10)))
                 10)))
    (format "%03d" next)))

;;;###autoload
(defun oga/docusaurus-new-doc-create (doc-id title order)
  "Create a new document directory and Markdown file in `default-directory'.
DOC-ID is the document identifier <Description>_<YYMMDD>_<author>.
TITLE is the document title used in front matter and H1.
ORDER is a 3-digit order prefix; auto-suggested as the next 10-step number."
  (interactive
   (let* ((doc-id (read-string "Document ID (e.g., MyDoc_260527_oo01): "))
          (title (read-string "Title: " doc-id))
          (suggested (new-doc--suggest-order default-directory))
          (order (read-string
                  (format "Order [default %s]: " suggested)
                  nil nil suggested)))
     (list doc-id title order)))
  (unless (string-match-p new-doc--doc-id-regexp doc-id)
    (user-error "Invalid Document ID: %s (expected <Description>_<YYMMDD>_<author>)" doc-id))
  (unless (string-match-p new-doc--order-regexp order)
    (user-error "Invalid order: %s (need 3 digits)" order))
  (let* ((basename (format "%s_%s" order doc-id))
         (dir-path (expand-file-name basename default-directory))
         (file-path (expand-file-name (concat basename ".md") dir-path)))
    (when (file-exists-p dir-path)
      (user-error "Directory already exists: %s" dir-path))
    (make-directory dir-path)
    (with-temp-file file-path
      (insert (format "---\nid: %s\ntitle: %s\n---\n\n# %s\n\n"
                      doc-id title title)))
    (find-file file-path)
    (goto-char (point-max))
    (message "Created %s" file-path)))


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



