;;; autoload/oga-java.el --- Custom Java helpers -*- lexical-binding: t; -*-
;;;###autoload
(defun oga/java-stringjoiner-add-region (start end)
  "Convert each line in a region to a Java StringJoiner add() method call."
  (interactive "r")
  (let ((lines (split-string (buffer-substring-no-properties start end) "\n" t))
        (insertion-point (if (use-region-p) end (point))))
    (goto-char insertion-point)
    (insert "\n")
    (dolist (line lines)
      (setq line (replace-regexp-in-string "\"" "\\\\\"" line))
      (insert (format "stringJoiner.add(\"%s\");\n" line)))))

;;;###autoload
(defun oga/java-show-error ()
  "Show the error message in the current line. (flycheck-list-errors)"
  (interactive)
  (flycheck-list-errors))
