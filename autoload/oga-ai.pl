;;; autoload/oga-calc.el -*- lexical-binding: t; -*-


(defconst gemini-model "gemini-1.5-flash"
  "The Gemini model name to use for translation.")

(defconst gemini-apikey "AIzaSyDlWxjq4Wo0jWiO3vFmKklrn_ItHmB5HVY"
  "API for authenticating with the Gemini service.")


;;;###autoload
(defvar oga/gptel-available-models
  '("gemini-1.5-flash"
    "gemini-1.5-pro"
    "gemini-1.5-pro-latest"
    "gemini-2.0-flash"
    "gemini-2.0-flash-lite"
    "gemini-2.0-pro-exp"
    "gemini-2.5-flash-preview-05-20"
    "gemini-2.5-pro-preview-03-25") ;'
  "List of available models for GPTel.")


;;;###autoload
(defun oga/gptel-switch-model ()
  "Interactively select and set the model for GPTel."
  (interactive)
  (let ((model (completing-read "Select GPT model: " oga/gptel-available-models nil t)))
    (setq gptel-model model)
    (message "gptel-model set to %s" model)))



;;;###autoload
(defun oga/ai-insert-response ()
  "Send the selected region as a prompt to Gemini via jbang and insert the result after the region.

The selected text is passed as input to the Java script which uses Gemini,
and the response is inserted immediately after the region."
  (interactive)
  (if (use-region-p)
      (let* ((end (region-end))
             (insertion-marker (copy-marker end))
             (text (buffer-substring-no-properties (region-beginning) (region-end)))
             (response
              (with-temp-buffer
                (insert text)
                (apply #'call-process-region
                       (point-min) (point-max)
                       "jbang" t t nil
                       "/home/oogasawa/scripts_java/sau3.java"
                       "gemini:run"
                       "-m" gemini-model
                       "-k" gemini-apikey)
                (buffer-string))))
        (goto-char insertion-marker)
        (insert "\n\n" response))
    (user-error "No region selected")))


    
;;;###autoload
(defun oga/ai-translate-region-to-English ()
  "Translate selected Japanese text to English using jbang and insert result before the region.

The function calls a Java program via `jbang` using `gemini-model` and `gemini-apikey`
to obtain the English translation of the selected region."
  (interactive)
  (if (use-region-p)
      (let* ((start (region-beginning))
             (insertion-marker (copy-marker start))
             (text (buffer-substring-no-properties (region-beginning) (region-end)))
             (translated
              (with-temp-buffer
                (insert text)
                (apply #'call-process-region
                       (point-min) (point-max)
                       "jbang" t t nil
                       "/home/oogasawa/scripts_java/sau3.java"
                       "gemini:toEnglish"
                       "-m" gemini-model
                       "-k" gemini-apikey)
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
                 "/home/oogasawa/scripts_java/sau3.java"
                 "gemini:toJapanese"
                 "-m" gemini-model
                 "-k" gemini-apikey)
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
                 "/home/oogasawa/scripts_java/sau3.java"
                 "gemini:paraphrase"
                 "-m" gemini-model
                 "-k" gemini-apikey)
                (buffer-string))))
        (kill-new paraphrased)
        (message "%s" (string-trim paraphrased)))
    (user-error "No region selected")))

    
;;;###autoload
(defun oga/ai-define-region ()
  "Give a definition of the selected English word or phrase using jbang, show result in minibuffer, and add to kill-ring."
  (interactive)
  (if (use-region-p)
      (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
             (definition
              (with-temp-buffer
                (insert text)
                (call-process-region
                 (point-min) (point-max)
                 "jbang" t t nil
                 "/home/oogasawa/scripts_java/sau3.java"
                 "gemini:define"
                 "-m" gemini-model
                 "-k" gemini-apikey)
                (buffer-string))))
        (kill-new definition)
        (message "%s" (string-trim definition)))
    (user-error "No region selected")))

