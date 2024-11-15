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


;; === Set your desired theme by uncommenting one of the lines below ===

;; Light themes
;; Uncomment one of the lines below to enable a light theme
;; (setq doom-theme 'doom-acario-light)
;; (setq doom-theme 'doom-fairy-floss)

;; Dark themes
;; Uncomment one of the lines below to enable a dark theme
;; (setq doom-theme 'doom-1337)
;; (setq doom-theme 'doom-badger)
;; (setq doom-theme 'doom-city-lights)
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-ephemeral)
;; (setq doom-theme 'doom-feather-dark)
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-opera)
(setq doom-theme 'deeper-blue)
;; (setq doom-theme 'wombat)



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



;; === dired mode ===
(after! dired
  ;; When the option dired-reuse-buffers is enabled,
  ;; Dired will reuse existing buffers to display new directories.
  (setq dired-reuse-buffers t))



;; === github copilot ===
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-n" . copilot-next-completion))
  :config
  ;; Add a hook to set indentation defaults for Copilot
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local tab-width 4)            ;; Default tab width
              (setq-local indent-tabs-mode nil)   ;; Use spaces instead of tabs
              )))


;; === golden-ratio ===
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



;; === neotree ===
(after! neotree
  ;; Define the function to set the NeoTree window width interactively
  (defun oga/set-neo-window-width ()
    "Prompt user for NeoTree window width and set neo-window-width."
    (interactive)
    (setq neo-window-width (read-number "Enter NeoTree window width: "))
    (message "NeoTree window width set to %d" neo-window-width))

  ;; Set the default NeoTree window width
  (setq neo-window-width 32))



;; === spell check ===

(after! flyspell
  (add-hook 'text-mode-hook #'flyspell-mode)   ;; Enable flyspell for text-related modes
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)) ;; Enable flyspell for comments and strings in programming modes


(after! ispell
  ;; Specify the program used for spell checking
  (setq-default ispell-program-name "aspell")
  
  ;; Set the local dictionary to British English
  (setq ispell-local-dictionary "en_US")


  ;; Set the default dictionary to English.
  (setq ispell-dictionary "english")
  
  ;; Exclude non-ASCII characters from spell checking
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))


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


;; (defun my-markdown-setup-function ()
;;   (interactive)
;;   (message "markdown-mode is active")
;;   ;; Remove hooks
;;   (remove-hook 'before-save-hook 'polymode-before-save t)
;;   (remove-hook 'after-save-hook 'polymode-after-save t)
;;   (remove-hook 'after-save-hook 'markdown-live-preview-if-markdown t))


;; (use-package markdown-mode
;;   :hook ((markdown-mode . my-markdown-setup-function)))

;; (add-hook 'markdown-mode-hook
;;           (lambda ()
;;             (run-with-idle-timer 0.1 nil #'my-markdown-setup-function)))


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


(map! "C-]" #'set-mark-command)


;; === scrolling ===
(map! "C-<down>" #'oga/scroll-up-half)
(map! "C-<up>" #'oga/scroll-down-half)


;; === switching windows and tabs ===
(map! "C-x p" #'oga/window-previous)
(map! "C-x o" #'oga/window-next)
(map! "C-x t p" #'tab-previous) ;; switching tabs


