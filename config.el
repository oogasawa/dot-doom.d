 ;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'dichromacy)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; flycheck for YAML
;; Prerequisite: yamllint is installed.
;;     pip install yamllint
(after! yaml-mode
  (add-hook 'yaml-mode-hook
            (lambda ()
              (flycheck-mode 1)
              (setq-local yaml-indent-offset 4))))



;;; =================================================


;; Disabling doom/delete-trailing-newlines in Doom Emacs when saving a buffer
;; https://www.reddit.com/r/emacs/comments/smo79c/disabling_doomdeletetrailingnewlines_in_doom/
(remove-hook 'doom-first-buffer-hook #'ws-butler-global-mode)


;; === fundamental settings ===

(setq doom/set-indent-width 4)

(setq initial-frame-alist
        '((top . 10) (left . 60)
          (width . 150)
        (height . 60)))

;; Main font
(setq doom-font (font-spec :family "Noto Sans Mono" :size 12))

;; Japanese font
(setq doom-unicode-font (font-spec :family "Noto Sans CJK JP" :size 12))


(bind-key "C-]" 'set-mark-command)

;; When the option dired-reuse-buffers is enabled,
;; Dired will reuse existing buffers to display new directories.
(setq dired-reuse-buffers t)


;; === fast scrolling ===
(defun oga/scroll-up-half ()
  (interactive)
  (let ((window-half-height
         (max 1 (/ (1- (window-height (selected-window))) 2))))
    (scroll-up window-half-height)
    ))
  

(defun oga/scroll-down-half ()
  (interactive)
    (let ((window-half-height (max 1 (/ (1- (window-height (selected-window))) 2))))
    (scroll-down window-half-height)
    ))

(bind-key* "C-<down>" 'oga/scroll-up-half)
(bind-key* "C-<up>" 'oga/scroll-down-half)


;; === switching windows ===

(defun oga/window-previous()
  (interactive)
  (other-window -1))

(defun oga/window-next()
  (interactive)
  (other-window 1))

(bind-key* "C-x p" 'oga/window-previous)
(bind-key* "C-x o" 'oga/window-next)


;; === switching tabs ===

(bind-key* "C-x t p" 'tab-previous)


;; === frame operations ===

(defun oga/frame-split-into-three-windows ()
  "Split the current frame into three horizontal windows."
  (interactive)                        
  (delete-other-windows)  ; Delete all other windows except the current one
  (split-window-below)    ; Split the window below
  (other-window 1)        ; Move to the next window
  (split-window-below)    ; Split the window below again
  (balance-windows))      ; Adjust the size of the windows evenly



;; Configure golden-ratio using use-package
(use-package golden-ratio
;;  :ensure t  ; Automatically install from package archives like MELPA
  :config
  (golden-ratio-mode 1)  ; Enable golden-ratio-mode
  ;; Exclude specific modes where window resizing is not desired
  :custom
  (golden-ratio-exclude-modes '("ediff-mode" "helm-mode" "dired-mode"))
  ;; Disable golden-ratio resizing during certain functions
  (golden-ratio-exclude-functions '(my-special-function))
  ;; Add additional commands that trigger resizing
  (golden-ratio-extra-commands '(windmove-up windmove-down windmove-left windmove-right))
  )





;; === neo-tree operations ===

(defun oga/set-neo-window-width ()
  "Prompt user for NeoTree window width and set neo-window-width."
  (interactive)
  (setq neo-window-width (read-number "Enter NeoTree window width: "))
  (message "NeoTree window width set to %d" neo-window-width))


(use-package! neotree
  :config
  (setq neo-window-width 32))




;; === shell buffer operations ===


(defun oga/get-last-directory (path)
  "Return the name of the last directory in the given path."
  (unless (file-directory-p path)
    (error "Invalid directory path: %s" path))
  (file-name-nondirectory (directory-file-name (file-name-directory (expand-file-name path)))))



;; Start shell-mode with specifying a current directory.
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


(defun oga/shell-dashboard ()
  (interactive)
  (when (get-buffer "*shell-dashboard.md*")
    (kill-buffer "*shell-dashboard.md*"))
  (oga/insert-last-five-lines-from-buffers (oga/shell-buffer-name-list)))



;; === general buffer operations ===


(defun oga/trunc (str)
  "Remove leading and trailing whitespace characters from a string."
  (replace-regexp-in-string "\\`[[:space:]]+\\|[[:space:]]+\\'" "" str))


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



;; === instant calculator ===
(defun oga/calc (expr)
    (insert (concat "\n" (number-to-string expr)))
)

(defun oga/power (base exponent)
  "Calculate the power of BASE raised to EXPONENT."
  (if (= exponent 0)
      1
    (* base (oga/power base (- exponent 1)))))

;; Example
;; (oga/power 2 3)  ;; calculates two to the third power -> 8



;; === java ===

(defun oga/java-string-joiner-add-region (start end)
  "Convert each line in the region to a Java StringJoiner add() method call with escaped double quotes and insert after the region."
  (interactive "r") ; Get the start and end positions of the region as arguments
  (let ((lines (split-string (buffer-substring-no-properties start end) "\n" t))
        (insertion-point (if (use-region-p) end (point)))) ; Set the insertion point
    (goto-char insertion-point) ; Move to the insertion point
    (insert "\n") ; Insert a blank line between the original text and the new text
    (dolist (line lines)
      (setq line (replace-regexp-in-string "\"" "\\\\\"" line)) ; Escape double quotes
      (insert (format "stringJoiner.add(\"%s\");\n" line)))))


(defun oga/java-show-error ()
  "Show the error message in the current line. (flycheck-list-errors)"
  (interactive)
  (flycheck-list-errors))





;; === docusaurus ===


;; Function to find and open the corresponding English file in a right split
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









;; ;; 候補となるテーマのリストを定義
;; ;;(setq doom-theme 'doom-one)
;; ;;(setq doom-theme 'wombat)
;; ;;(setq doom-theme 'tango)
;; ;;(setq doom-theme 'doom-opera)
;; ;;(setq doom-theme 'doom-city-lights)
;; ;;(setq doom-theme 'deeper-blue)
;; ;;(setq doom-theme 'wheatgrass)

;; (defvar oga/themes-favorite
;;   '(doom-1337
;;     doom-acario-light
;;     doom-badger
;;     doom-city-lights
;;     doom-dark+
;;     doom-dracula
;;     doom-ephemeral
;;     doom-fairy-floss
;;     doom-feather-dark
;;     doom-one
;;     doom-opera
;;     deeper-blue
;;     wombat))


;; (defun oga/theme-load ()
;;   "Load one of my favorite themes."
;;   (interactive)
;;   ;; テーマの選択プロンプトを表示
;;   (let ((theme (completing-read "Choose a theme: " oga/themes-favorite nil t)))
;;     ;; 文字列をシンボルに変換してテーマをロード
;;     (load-theme (intern theme) t)))

;; ;; キーバインドなどで `my-load-theme` を呼び出せるようにする
;; ;;(global-set-key (kbd "C-c t") 'my-load-theme)


;; (defun load-string-mappings (file-path)
;;   "Load string mappings from a file."
;;   (let (mappings)
;;     (with-temp-buffer
;;       (insert-file-contents file-path)
;;       (goto-char (point-min))
;;       ;; 各行を読み取りながらマッピングを生成
;;       (while (not (eobp))
;;         (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
;;                (parts (split-string line "\t" t)))
;;           (when (= (length parts) 2)
;;             (push (cons (nth 0 parts) (nth 1 parts)) mappings)))
;;         (forward-line)))
;;     mappings))

;; (defun oga/abbrev ()
;;   "Replace the string at point with a corresponding mapping."
;;   (interactive)
;;   ;; 文字列のマッピングをファイルから読み込む
;;   (let* ((string-mappings (load-string-mappings (concat (getenv "HOME") "/.oga-abbrev.txt")))
;;          (begin (save-excursion
;;                   (skip-chars-backward "^ \t\n")
;;                   (point)))
;;          (end (save-excursion
;;                 (skip-chars-forward "^ \t\n")
;;                 (point)))
;;          (current-string (buffer-substring-no-properties begin end))
;;          (replacement (cdr (assoc current-string string-mappings))))

;;     (when replacement
;;       (delete-region begin end)
;;       (insert replacement))))




;; ;; === ispell-mode ===

;; (setq-default ispell-program-name "aspell")
;; (with-eval-after-load "ispell"
;;   (setq ispell-local-dictionary "en_GB")
;;   (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))



;;; === github copilot ===
;;; accept completion from copilot and fallback to company
;(use-package! copilot
;  :hook (prog-mode . copilot-mode)
;  :bind (:map copilot-completion-map
;              ("<tab>" . 'copilot-accept-completion)
;              ("TAB" . 'copilot-accept-completion)
;              ("C-TAB" . 'copilot-accept-completion-by-word)
;              ("C-<tab>" . 'copilot-accept-completion-by-word)))


;; === god-mode ===
;; https://emacs.stackexchange.com/questions/33660/making-it-clearer-im-in-god-mode

;; (defun me//god-mode-indicator ()
;;   (cond (god-local-mode
;;          (progn
;;            (set-face-background 'mode-line "red4")
;;            (set-face-foreground 'mode-line "gray")
;;            (set-face-background 'mode-line-inactive "gray30")
;;            (set-face-foreground 'mode-line-inactive "red")))
;;         (t
;;          (progn
;;            (set-face-background 'mode-line-inactive "blue")
;;            (set-face-foreground 'mode-line-inactive "gray30")
;;            (set-face-background 'mode-line "gray75")
;;            (set-face-foreground 'mode-line "black")))))

;; (add-hook 'god-mode-enabled-hook #'me//god-mode-indicator)
;; (add-hook 'god-mode-disabled-hook #'me//god-mode-indicator)


;; ;; Update cursor
;; (defun my-god-mode-update-cursor ()
;;     (setq cursor-type (if (or god-local-mode buffer-read-only)
;;                         'box
;;                         'bar)))
;; (add-hook 'god-mode-enabled-hook #'my-god-mode-update-cursor)
;; (add-hook 'god-mode-disabled-hook #'my-god-mode-update-cursor)


;; ;; ;; Update mode-line
;; (defun my-god-mode-enabled-modeline ()
;;     (set-face-background 'mode-line "red4")
;; )
;; (defun my-god-mode-disabled-modeline ()
;;     (set-face-background 'mode-line "gray30")
;; )
;; (add-hook 'god-mode-enabled-hook #'my-god-mode-enabled-modeline)
;; (add-hook 'god-mode-disabled-hook #'my-god-mode-disabled-modeline)


;; ;;(bind-key* "M-]" 'god-mode)
;; ;;(bind-key* "s-]" 'god-mode)
;; (bind-key* "C-]" 'god-mode)
;; (bind-key* "C-t" 'god-mode)



;; === markdown ===

;; (after! markdown-mode
;;   (map! :map markdown-mode-map
;;         "DEL" nil  ;; 既存のバインディングを無効化
;;         "DEL" #'backward-delete-char))


;; (after! markdown-mode
;;   ;; markdown-modeが有効になるときに実行される関数を定義
;;   (add-hook 'markdown-mode-hook
;;             (lambda ()
;;               (remove-hook 'before-save-hook 'polymode-before-save t)  ; buffer-localに削除
;;               (remove-hook 'after-save-hook 'polymode-after-save t)    ; buffer-localに削除
;;               (remove-hook 'after-save-hook 'markdown-live-preview-if-markdown t)  ; buffer-localに削除
;;             ))
;; )


;; (after! markdown-mode
;;   (add-hook 'markdown-mode-hook
;;             (lambda ()
;;               (message "Current before-save-hook: %S" before-save-hook)
;;               (remove-hook 'before-save-hook 'polymode-before-save t)
;;               (message "Updated before-save-hook: %S" before-save-hook)
;;               (message "Current after-save-hook: %S" after-save-hook)
;;               (remove-hook 'after-save-hook 'polymode-after-save t)
;;               (remove-hook 'after-save-hook 'markdown-live-preview-if-markdown t)
;;               (message "Updated after-save-hook: %S" after-save-hook))))


(defun my-markdown-setup-function ()
  (interactive)
  (message "markdown-mode is active")
  ;; Remove hooks
  (remove-hook 'before-save-hook 'polymode-before-save t)
  (remove-hook 'after-save-hook 'polymode-after-save t)
  (remove-hook 'after-save-hook 'markdown-live-preview-if-markdown t))


(use-package markdown-mode
  :hook ((markdown-mode . my-markdown-setup-function)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (run-with-idle-timer 0.1 nil #'my-markdown-setup-function)))

;; === spell check ===

(setq ispell-dictionary "english") ; Set the default dictionary to English.

