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
(setq doom-theme 'doom-xcode)
;;(setq doom-theme 'deeper-blue)

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


;; === cc-mode ===
(after! cc-mode
  (add-hook 'java-mode-hook
            (lambda ()
              (setq c-basic-offset 4
                    tab-width 4
                    lsp-enable-indentation nil))))


;; === company code completion ===
;; Do not use previously entered strings for completion. (By default, they are used.)
;;(after! company
;;  (setq company-disabled-backends '(company-dabbrev)))


;; === dired mode ===
(after! dired
  ;; When the option dired-reuse-buffers is enabled,
  ;; Dired will reuse existing buffers to display new directories.
  (setq dired-reuse-buffers t))


;; === flyspell spell checker ===
;; (after! flyspell
;;   (setq ispell-program-name "aspell")  ; 使用するスペルチェッカーツール
;;   (setq ispell-dictionary "en"))       ; デフォルトの辞書を英語に設定

;; (setq global-flyspell-mode nil)
;; (setq-default flyspell-mode nil)
;; (remove-hook 'text-mode-hook #'flyspell-mode)
;; (remove-hook 'prog-mode-hook #'flyspell-prog-mode)
;; (after! spell-fu
;;   (remove-hook 'text-mode-hook #'spell-fu-mode)
;;   (remove-hook 'prog-mode-hook #'spell-fu-mode))



;; ;; === github copilot ===
;; ;; accept completion from copilot and fallback to company
;; (use-package! copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . copilot-accept-completion)
;;               ("C-<tab>" . copilot-accept-completion-by-word)
;;               ("C-n" . copilot-next-completion))
;;   :config
;;   ;; Add a hook to set indentation defaults for Copilot
;;   (add-hook 'prog-mode-hook
;;             (lambda ()
;;               (setq-local tab-width 4)            ;; Default tab width
;;               (setq-local indent-tabs-mode nil)   ;; Use spaces instead of tabs
;;               )))




;; ;; === god-mode ===
;; ;; Load and initialize god-mode
;; (load! "autoload/oga-god")
;; (after! emacs
;;   (oga/setup-god-mode))

;; ;; Key binding
;; (map! :g "M-n" #'oga/god-toggle-global)




;; (defun god-mode-all-enable ()
;;   "Enable god-mode in all suitable buffers without toggling unexpectedly."
;;   (interactive)
;;   (dolist (buf (buffer-list))
;;     (with-current-buffer buf
;;       (when (and (derived-mode-p 'prog-mode 'text-mode)
;;                  (not (minibufferp))
;;                  (not god-local-mode))
;;         ;; 強制有効化
;;         (god-mode-all))))
;;   (message "god-mode ENABLED in all relevant buffers."))

;; (defun god-mode-all-disable ()
;;   "Disable god-mode in all suitable buffers without toggling unexpectedly."
;;   (interactive)
;;   (dolist (buf (buffer-list))
;;     (with-current-buffer buf
;;       (when god-local-mode
;;         ;; 強制無効化
;;         (god-mode-all))))
;;   (message "god-mode DISABLED in all relevant buffers."))

;; (map! :g "M-[" #'god-mode-all-enable
;;       :g "M-]" #'god-mode-all-disable)




;; === gptel (LLM) ===

(use-package! gptel
  :config
  (setq gptel-api-key "AIzaSyDlWxjq4Wo0jWiO3vFmKklrn_ItHmB5HVY"))


;; :key can be a function that returns the API key.
(gptel-make-gemini "Gemini" :key "AIzaSyDlWxjq4Wo0jWiO3vFmKklrn_ItHmB5HVY" :stream t)

;; OPTIONAL configuration
(setq
 gptel-model 'gemini-2.0-flash
 gptel-backend (gptel-make-gemini "Gemini"
                 :key "AIzaSyDlWxjq4Wo0jWiO3vFmKklrn_ItHmB5HVY"
                 :stream t))







;; === ispell ===
(setq ispell-program-name "aspell")  ; 使用するスペルチェッカーツール
(setq ispell-dictionary "english") ;; デフォルトの辞書を英語に設定


;; === lsp ===
(after! lsp-java
  (setq
        lsp-java-jdt-download-url
        "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.54.0/jdt-language-server-1.54.0-202511261751.tar.gz"))

(after! lsp-mode
  (add-to-list 'lsp-enabled-clients 'jdtls))

(after! lsp-java
  (setq c-basic-offset 4)            ;; 基本のインデント幅を4に設定
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  (setq lsp-java-format-settings-profile "GoogleStyle")
  )

(after! lsp-java
  (setq lsp-java-vmargs
        '("-noverify"
          "-Xmx8G" ;; 最大ヒープサイズを8GBに設定
          "-Xms1G" ;; 初期ヒープサイズを1GBに設定
          "-XX:+UseG1GC"
          "-XX:+UseStringDeduplication")))

(after! lsp-mode
  ;; 大きなプロジェクトでのメモリ使用を最適化
  (setq lsp-idle-delay 0.5 ;; 処理の遅延時間
        lsp-log-io nil ;; ログ出力を無効化
        lsp-file-watch-threshold 5000 ;; ファイル監視の閾値を増やす
        lsp-completion-provider :capf ;; Companyと競合を防ぐ
        read-process-output-max (* 1024 1024))) ;; プロセス出力を増加


;; ファイル監視対象を減らす
(after! lsp-mode
  (setq lsp-file-watch-ignored-directories
        '("[/\\\\]\\.git$"
          "[/\\\\]\\.hg$"
          "[/\\\\]\\.bzr$"
          "[/\\\\]_build$"
          "[/\\\\]\\.idea$"
          "[/\\\\]\\.vscode$"
          "[/\\\\]\\.gradle$"
          "[/\\\\]build$")))


(setq gc-cons-threshold (* 100 1024 1024))  ;; 100MB に調整
(setq gc-cons-percentage 0.2)  ;; GC の発生を適度に
(setq read-process-output-max (* 1024 1024))  ;; 1MB に戻す
(setq large-file-warning-threshold (* 512 1024 1024))  ;; 512MB に戻す


;; === neotree ===
(after! neotree
  ;; Define the function to set the NeoTree window width interactively
  (defun oga/neo-set-width ()
    "Prompt user for NeoTree window width and set neo-window-width."
    (interactive)
    (setq neo-window-width (read-number "Enter NeoTree window width: "))
    (message "NeoTree window width set to %d" neo-window-width))

  ;; Set the default NeoTree window width
  (setq neo-window-width 48))


;; === undo foo ===

;; undo-fu の設定
(use-package! undo-fu
  :defer t
  :init
  ;; undo と redo のキーバインドを設定
  (define-key global-map (kbd "C-z") 'undo-fu-only-undo)
  (define-key global-map (kbd "C-S-z") 'undo-fu-only-redo)
  (define-key global-map (kbd "C-/") 'undo-fu-only-undo)
  (define-key global-map (kbd "C-?") 'undo-fu-only-redo))

;; undo-fu-session の設定
(use-package! undo-fu-session
  :after undo-fu
  :init
  ;; セッションの保存先ディレクトリを設定
  (setq undo-fu-session-directory (expand-file-name ".undo-fu-session" user-emacs-directory))
  ;; 自動保存を有効にする
  (setq undo-fu-session-incompatible-files '())
  :config
  (global-undo-fu-session-mode))  ; グローバルに有効化


;;; === vundo ===

(use-package! vundo
  :config
  ;; キーバインドの設定
  ;; (define-key vundo-mode-map (kbd "h") 'vundo-backward)
  ;; (define-key vundo-mode-map (kbd "j") 'vundo-next)
  ;; (define-key vundo-mode-map (kbd "k") 'vundo-previous)
  ;; (define-key vundo-mode-map (kbd "l") 'vundo-forward)
  ;; カスタマイズオプションの設定
  (setq vundo-compact-display t)  ; コンパクトな表示にする
  )


;;; === yasnippet ===

(after! yasnippet
  (setq yas-snippet-dirs '("~/.doom.d/snippets"))  ; ユーザー定義のみ
  (yas-global-mode 1))

(after! company
  (add-to-list 'company-backends 'company-yasnippet))


;; ==========================================================================
;;    fundamental settings
;; ==========================================================================

;; Disabling doom/delete-trailing-newlines in Doom Emacs when saving a buffer
;; https://www.reddit.com/r/emacs/comments/smo79c/disabling_doomdeletetrailingnewlines_in_doom/
(remove-hook 'doom-first-buffer-hook #'ws-butler-global-mode)


(setq doom/set-indent-width 4)

(setq initial-frame-alist
        '((top . 10) (left . 60)
          (width . 150)
        (height . 60)))

;; Main font
(setq doom-font (font-spec :family "Noto Sans Mono" :size 14))

;; Japanese font
(setq doom-unicode-font (font-spec :family "Noto Sans CJK JP" :size 14))


;;(map! "C-[" #'set-mark-command)

;; Enable GTK native input method integration (fcitx5 + mozc).
;; When nil (Emacs default), Emacs ignores the GTK IM module and
;; consumes all key events itself — so C-SPC never reaches fcitx5
;; and fires set-mark-command instead.  Setting this to t lets GTK
;; forward C-SPC (the fcitx5 trigger key) to the IM module first,
;; so fcitx5 toggles normally without needing global-unset-key or
;; any Elisp wrapper like fcitx5-remote.
(setq x-gtk-use-native-input t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8
      default-buffer-file-coding-system 'utf-8-unix)


;; Ctrl-a/0 auto-selects region and typing replaces it (Vim-like)
;; ;; disable with (setq shift-select-mode nil).
(setq shift-select-mode nil)


;; === scrolling ===
(map! "C-<down>" #'oga/scroll-up-half)
(map! "C-<up>" #'oga/scroll-down-half)


;; === switching windows and tabs ===
(map! "C-x p" #'oga/window-previous)
(map! "C-x o" #'oga/window-next)
(map! "C-x t p" #'tab-previous) ;; switching tabs


;;(map! "C-k" #'oga/delete-line-no-kill)
(map! "C-k" #'kill-line)


;; === Always split the given WINDOW vertically (i.e., top and bottom) ===
(defun oga/split-window-vertically-always (window)
  "Always split the given WINDOW vertically (i.e., top and bottom),
regardless of the window's width. This overrides the default
behavior of Emacs, which chooses the split direction based on
the window size. This function ensures consistent vertical
splitting when new windows are created automatically."
  (when (and (window-splittable-p window t)
             ;; Avoid splitting the minibuffer or other non-splittable windows
             (not (window-minibuffer-p window)))
    (split-window window nil 'below)))

(setq split-window-preferred-function #'oga/split-window-vertically-always)


;; === Always import shell environment variables ===
(setq exec-path-from-shell-debug t)
(use-package! exec-path-from-shell
  :config
  ;; Explicitly specify environment variables to import
  (setq exec-path-from-shell-variables
        '("PATH" "JAVA_HOME"))
  ;; Always load them, regardless of GUI or terminal
  (exec-path-from-shell-initialize))


;; === LLM-IME: AI-powered text completion and kanji conversion ===
(load! "llm-ime")

