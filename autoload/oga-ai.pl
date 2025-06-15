;;; autoload/oga-calc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun oga/ai-translate-region-to-English ()
  "Translate selected Japanese text to English using jbang and insert result before the region."
  (interactive)
  (if (use-region-p)
      (let* ((start (region-beginning))
             (insertion-marker (copy-marker start))
             (text (buffer-substring-no-properties (region-beginning) (region-end)))
             (translated
              (with-temp-buffer
                (insert text)
                (call-process-region
                 (point-min) (point-max)
                 "jbang" t t nil
                 "/home/oogasawa/scripts_java/sau3.java" "gemini:toEnglish")
                (buffer-string))))
        (goto-char insertion-marker)
        (insert translated "\n\n"))
    (user-error "No region selected")))



;;;###autoload
(defun oga/ai-translate-region-to-Japanese ()
  "Translate selected English to Japanese text using jbang and insert result before the region."
  (interactive)
  (if (use-region-p)
      (let* ((start (region-beginning))
             (insertion-marker (copy-marker start))
             (text (buffer-substring-no-properties (region-beginning) (region-end)))
             (translated
              (with-temp-buffer
                (insert text)
                (call-process-region
                 (point-min) (point-max)
                 "jbang" t t nil
                 "/home/oogasawa/scripts_java/sau3.java" "gemini:toJapanese")
                (buffer-string))))
        (goto-char insertion-marker)
        (insert translated "\n\n"))
    (user-error "No region selected")))

    

;;;###autoload
(defun oga/ai-paraphrase-region ()
  "Paraphrase selected English text using jbang, show result in minibuffer, and add to kill-ring."
  (interactive)
  (if (use-region-p)
      (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
             (paraphrased
              (with-temp-buffer
                (insert text)
                (call-process-region
                 (point-min) (point-max)
                 "jbang" t t nil
                 "/home/oogasawa/scripts_java/sau3.java" "gemini:paraphrase")
                (buffer-string))))
        (kill-new paraphrased)
        (message "%s" (string-trim paraphrased)))
    (user-error "No region selected")))

    
;;;###autoload
(defun oga/ai-define-region ()
  "Define the selected English word or phrase using jbang, show result in minibuffer, and add to kill-ring."
  (interactive)
  (if (use-region-p)
      (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
             (definition
              (with-temp-buffer
                (insert text)
                (call-process-region
                 (point-min) (point-max)
                 "jbang" t t nil
                 "/home/oogasawa/scripts_java/sau3.java" "gemini:define")
                (buffer-string))))
        (kill-new definition)
        (message "%s" (string-trim definition)))
    (user-error "No region selected")))

