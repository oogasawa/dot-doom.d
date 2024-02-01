;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Disabling doom/delete-trailing-newlines in Doom Emacs when saving a buffer
;; https://www.reddit.com/r/emacs/comments/smo79c/disabling_doomdeletetrailingnewlines_in_doom/
(remove-hook 'doom-first-buffer-hook #'ws-butler-global-mode)


(setq doom/set-indent-width 4)


(defun oga/window-previous()
  (interactive)
  (other-window -1))

(defun oga/window-next()
  (interactive)
  (other-window 1))

(bind-key "C-x p" 'oga/window-previous)
(bind-key "C-x o" 'oga/window-next)

(bind-key* "C-x t p" 'tab-previous)

(bind-key "C-]" 'set-mark-command)


;; When the option dired-reuse-buffers is enabled,
;; Dired will reuse existing buffers to display new directories.
(setq dired-reuse-buffers t)


(defun oga/frame-split-into-three ()
  "Split the current frame into three horizontal windows."
  (interactive)                        
  + (delete-other-windows)  ; Delete all other windows except the current one
  + (split-window-below)    ; Split the window below
  + (other-window 1)        ; Move to the next window
  + (split-window-below)    ; Split the window below again
  + (balance-windows))      ; Adjust the size of the windows evenly


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





;; === docusaurus ===


;; Function to find and open the corresponding English file in a right split
(defun oga/docusaurus-open-i18n-file ()
  "Find and open the corresponding English file for the current file in a Docusaurus project."
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



;; 候補となるテーマのリストを定義
;;(setq doom-theme 'doom-one)
;;(setq doom-theme 'wombat)
;;(setq doom-theme 'tango)
;;(setq doom-theme 'doom-opera)
;;(setq doom-theme 'doom-city-lights)
;;(setq doom-theme 'deeper-blue)
;;(setq doom-theme 'wheatgrass)

(defvar oga/themes-favorite
  '(doom-1337
    doom-acario-light
    doom-badger
    doom-city-lights
    doom-dark+
    doom-dracula
    doom-ephemeral
    doom-fairy-floss
    doom-feather-dark
    wombat))

(defun oga/theme-load ()
  "Load one of my favorite themes."
  (interactive)
  ;; テーマの選択プロンプトを表示
  (let ((theme (completing-read "Choose a theme: " oga/themes-favorite nil t)))
    ;; 文字列をシンボルに変換してテーマをロード
    (load-theme (intern theme) t)))

;; キーバインドなどで `my-load-theme` を呼び出せるようにする
;;(global-set-key (kbd "C-c t") 'my-load-theme)


(defun load-string-mappings (file-path)
  "Load string mappings from a file."
  (let (mappings)
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      ;; 各行を読み取りながらマッピングを生成
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (parts (split-string line "\t" t)))
          (when (= (length parts) 2)
            (push (cons (nth 0 parts) (nth 1 parts)) mappings)))
        (forward-line)))
    mappings))

(defun oga/abbrev ()
  "Replace the string at point with a corresponding mapping."
  (interactive)
  ;; 文字列のマッピングをファイルから読み込む
  (let* ((string-mappings (load-string-mappings (concat (getenv "HOME") "/.oga-abbrev.txt")))
         (begin (save-excursion
                  (skip-chars-backward "^ \t\n")
                  (point)))
         (end (save-excursion
                (skip-chars-forward "^ \t\n")
                (point)))
         (current-string (buffer-substring-no-properties begin end))
         (replacement (cdr (assoc current-string string-mappings))))

    (when replacement
      (delete-region begin end)
      (insert replacement))))



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
  "Display the buffer starting with '*shell' in a window with a specified number of lines."
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


(defun oga/docusaurus-port ()
  "Searches for \"Docusaurus is running at \" in the selected region's buffer and returns the line if found."
  (interactive)
  (let ((buf (buffer-substring-no-properties (region-beginning) (region-end))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (when (search-backward "Docusaurus website is running at " nil t)
          (message (thing-at-point 'line)))))))


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




;; === neo-tree operations ===

(defun oga/set-neo-window-width ()
  "Prompt user for NeoTree window width and set neo-window-width."
  (interactive)
  (setq neo-window-width (read-number "Enter NeoTree window width: "))
  (message "NeoTree window width set to %d" neo-window-width))


(use-package! neotree
  :config
  (setq neo-window-width 32))


;; === calculator ===

;; instant calculator
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



;; ispell-mode

(setq-default ispell-program-name "aspell")
(with-eval-after-load "ispell"
  (setq ispell-local-dictionary "en_GB")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))



;; github copilot 
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


;; god-mode
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





;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Osamu Ogasawara"
      user-mail-address "osamu.ogasawara@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
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
;;(setq doom-theme 'doom-one)
(setq doom-theme 'wombat)
;;(setq doom-theme 'tango)

;;(setq doom-theme 'doom-opera)
;;(setq doom-theme 'doom-city-lights)
;;(setq doom-theme 'deeper-blue)
;;(setq doom-theme 'wheatgrass)




;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #character encode
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #mozc
;; Executes mozc settings and font settings only when Emacs is called from a GUI environment
;; (not from a text terminal).
(when (display-graphic-p)
  ;; mozc.el を読み込む
  (require 'mozc)
  ;; 入力メソッドとして mozc を設定
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc")
  (bind-key* "s-\\" 'toggle-input-method)

  ;; オプション: mozc-candidate-style を 'overlay に設定すると、候補の一覧がオーバーレイで表示されます。
  ;; (setq mozc-candidate-style 'overlay)
  ;; improve mozc conversion speed.
  ;; https://ut0s.netlify.app/2019/08/speedup-mozc-input/
  (setq mozc-candidate-style 'echo-area)

  ;; font settings

  ;; (setq doom-font (font-spec :family "NasuM" :size 14)
  ;;       doom-variable-pitch-font (font-spec :family "Nasu")
  ;;       doom-unicode-font (font-spec :family "NasuM")
  ;;       doom-big-font (font-spec :family "NasuM" :size 22))

  (setq doom-font (font-spec :family "sarasa mono j" :size 14)
	doom-variable-pitch-font (font-spec :family "sarasa gothic j")
	doom-unicode-font (font-spec :family "sarasa gothic j")
	doom-big-font (font-spec :family "sarasa gothic j" :size 18))
)

;;soft wrapping
;;(global-visual-line-mode t)

;;turn off auto-fill
(add-hook 'markdown-mode-hook (lambda () (auto-fill-mode -1)))

;; set defarult comment charqcter.
(setq-default comment-start "# ")

;; #company
(setq company-idle-delay 0.2)



;; doom-modeline

;; If non-nil, cause imenu to see `doom-modeline' declarations.
;; This is done by adjusting `lisp-imenu-generic-expression' to
;; include support for finding `doom-modeline-def-*' forms.
;; Must be set before loading doom-modeline.
;;(setq doom-modeline-support-imenu t)

;; ;; How tall the mode-line should be. It's only respected in GUI.
;; ;; If the actual char height is larger, it respects the actual height.
;; (setq doom-modeline-height 25)

;; ;; How wide the mode-line bar should be. It's only respected in GUI.
;; (setq doom-modeline-bar-width 4)

;; ;; Whether to use hud instead of default bar. It's only respected in GUI.
;; (setq doom-modeline-hud nil)

;; ;; The limit of the window width.
;; ;; If `window-width' is smaller than the limit, some information won't be
;; ;; displayed. It can be an integer or a float number. `nil' means no limit."
;; (setq doom-modeline-window-width-limit 85)

;; ;; How to detect the project root.
;; ;; nil means to use `default-directory'.
;; ;; The project management packages have some issues on detecting project root.
;; ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; ;; to hanle sub-projects.
;; ;; You can specify one if you encounter the issue.
(setq doom-modeline-project-detection 'auto)

;; ;; Determines the style used by `doom-modeline-buffer-file-name'.
;; ;;
;; ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;; ;;   auto => emacs/l/comint.el (in a project) or comint.el
;; ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;; ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;; ;;   truncate-with-project => emacs/l/comint.el
;; ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;; ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;; ;;   truncate-all => ~/P/F/e/l/comint.el
;; ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
;; ;;   relative-from-project => emacs/lisp/comint.el
;; ;;   relative-to-project => lisp/comint.el
;; ;;   file-name => comint.el
;; ;;   buffer-name => comint.el<2> (uniquify buffer name)
;; ;;
;; ;; If you are experiencing the laggy issue, especially while editing remote files
;; ;; with tramp, please try `file-name' style.
;; ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'auto)

;; ;; Whether display icons in the mode-line.
;; ;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon t)

;; ;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
 (setq doom-modeline-major-mode-icon t)

;; ;; Whether display the colorful icon for `major-mode'.
;; ;; It respects `nerdg-icons-color-icons'.
;; (setq doom-modeline-major-mode-color-icon t)

;; ;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
 (setq doom-modeline-buffer-state-icon t)

;; ;; Whether display the modification icon for the buffer.
;; ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
;; (setq doom-modeline-buffer-modification-icon t)

;; ;; Whether display the time icon. It respects variable `doom-modeline-icon'.
;; (setq doom-modeline-time-icon t)

;; ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
;; (setq doom-modeline-unicode-fallback nil)

;; ;; Whether display the buffer name.
 (setq doom-modeline-buffer-name t)

;; ;; Whether highlight the modified buffer name.
;; (setq doom-modeline-highlight-modified-buffer-name t)

;; ;; Whether display the minor modes in the mode-line.
;; (setq doom-modeline-minor-modes nil)

;; ;; If non-nil, a word count will be added to the selection-info modeline segment.
 (setq doom-modeline-enable-word-count nil)

;; ;; Major modes in which to display word count continuously.
;; ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;; ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;; ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
;; (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

;; ;; Whether display the buffer encoding.
 (setq doom-modeline-buffer-encoding t)

;; ;; Whether display the indentation information.
 (setq doom-modeline-indent-info nil)

;; ;; If non-nil, only display one number for checker information if applicable.
 (setq doom-modeline-checker-simple-format t)

;; ;; The maximum number displayed for notifications.
;; (setq doom-modeline-number-limit 99)

;; ;; The maximum displayed length of the branch name of version control.
;; (setq doom-modeline-vcs-max-length 12)

;; ;; Whether display the workspace name. Non-nil to display in the mode-line.
 (setq doom-modeline-workspace-name t)

;; ;; Whether display the perspective name. Non-nil to display in the mode-line.
 (setq doom-modeline-persp-name t)

;; ;; If non nil the default perspective name is displayed in the mode-line.
(setq doom-modeline-display-default-persp-name nil)

;; ;; If non nil the perspective name is displayed alongside a folder icon.
;; (setq doom-modeline-persp-icon t)

;; ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
 (setq doom-modeline-lsp t)

;; ;; Whether display the GitHub notifications. It requires `ghub' package.
 (setq doom-modeline-github nil)

;; ;; The interval of checking GitHub.
(setq doom-modeline-github-interval (* 30 60))

;; ;; Whether display the modal state.
;; ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
 (setq doom-modeline-modal t)

;; ;; Whether display the modal state icon.
;; ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
;; (setq doom-modeline-modal-icon t)

;; ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
;; (setq doom-modeline-mu4e nil)
;; ;; also enable the start of mu4e-alert
;; (mu4e-alert-enable-mode-line-display)

;; ;; Whether display the gnus notifications.
;; (setq doom-modeline-gnus t)

;; ;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
;; (setq doom-modeline-gnus-timer 2)

;; ;; Wheter groups should be excludede when gnus automatically being updated.
;; (setq doom-modeline-gnus-excluded-groups '("dummy.group"))

;; ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
;; (setq doom-modeline-irc t)

;; ;; Function to stylize the irc buffer names.
;; (setq doom-modeline-irc-stylize 'identity)

;; ;; Whether display the battery status. It respects `display-battery-mode'.
;; (setq doom-modeline-battery t)

;; ;; Whether display the time. It respects `display-time-mode'.
;;(setq doom-modeline-time t)

;; ;; Whether display the misc segment on all mode lines.
;; ;; If nil, display only if the mode line is active.
;; (setq doom-modeline-display-misc-in-all-mode-lines t)

;; ;; Whether display the environment version.
(setq doom-modeline-env-version t)
;; ;; Or for individual languages
;; (setq doom-modeline-env-enable-python t)
;; (setq doom-modeline-env-enable-ruby t)
;; (setq doom-modeline-env-enable-perl t)
;; (setq doom-modeline-env-enable-go t)
;; (setq doom-modeline-env-enable-elixir t)
;; (setq doom-modeline-env-enable-rust t)

;; ;; Change the executables to use for the language version string
;; (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
;; (setq doom-modeline-env-ruby-executable "ruby")
;; (setq doom-modeline-env-perl-executable "perl")
;; (setq doom-modeline-env-go-executable "go")
;; (setq doom-modeline-env-elixir-executable "iex")
;; (setq doom-modeline-env-rust-executable "rustc")

;; ;; What to display as the version while a new one is being loaded
;; (setq doom-modeline-env-load-string "...")

;; ;; By default, almost all segments are displayed only in the active window. To
;; ;; display such segments in all windows, specify e.g.
;; (setq doom-modeline-always-visible-segments '(mu4e irc))

;; ;; Hooks that run before/after the modeline version string is updated
;; (setq doom-modeline-before-update-env-hook nil)
;; (setq doom-modeline-after-update-env-hook nil)


