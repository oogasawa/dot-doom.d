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
(setq doom-theme 'doom-one)

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
(after! flyspell
  (setq ispell-program-name "aspell")  ; 使用するスペルチェッカーツール
  (setq ispell-dictionary "en"))       ; デフォルトの辞書を英語に設定


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



;; === lsp ===
(after! lsp-java
  (setq
        lsp-java-jdt-download-url
        "https://download.eclipse.org/jdtls/milestones/1.42.0/jdt-language-server-1.42.0-202411281516.tar.gz"))

(after! lsp-mode
  (add-to-list 'lsp-enabled-clients 'jdtls))



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
(setq doom-font (font-spec :family "Noto Sans Mono" :size 12))

;; Japanese font
(setq doom-unicode-font (font-spec :family "Noto Sans CJK JP" :size 12))


(map! "C-]" #'set-mark-command)


;; === scrolling ===
(map! "C-<down>" #'oga/scroll-up-half)
(map! "C-<up>" #'oga/scroll-down-half)


;; === switching windows and tabs ===
(map! "C-x p" #'oga/window-previous)
(map! "C-x o" #'oga/window-next)
(map! "C-x t p" #'tab-previous) ;; switching tabs
