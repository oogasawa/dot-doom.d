;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Disabling doom/delete-trailing-newlines in Doom Emacs when saving a buffer
;; https://www.reddit.com/r/emacs/comments/smo79c/disabling_doomdeletetrailingnewlines_in_doom/
(remove-hook 'doom-first-buffer-hook #'ws-butler-global-mode)


(setq doom/set-indent-width 4)


(defun oga/previous-window()
  (interactive)
  (other-window -1))

(defun oga/next-window()
  (interactive)
  (other-window 1))

(bind-key "C-x p" 'oga/previous-window)
(bind-key "C-x o" 'oga/next-window)




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




;; instant calculator
(defun oga/calc-here (expr)
    (insert (concat "\n" (number-to-string expr)))
)


;; fast scrolling
(defun oga/scroll-up-half ()
  (interactive)
  (let ((window-half-height (max 1 (/ (1- (window-height (selected-window))) 2))))
    (scroll-up (window-half-height))
    ))
  

(defun oga/scroll-down-half ()
  (interactive)
    (let ((window-half-height (max 1 (/ (1- (window-height (selected-window))) 2))))
    (scroll-down (window-half-height))
    ))


(bind-key* "C-<down>" 'oga/scroll-up-half)
(bind-key* "C-<up>" 'oga/scroll-down-half)



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
            (yank)))))
    (switch-to-buffer result-buffer)))


(defun oga/shell-dashboard ()
  (interactive)
  (insert-last-five-lines-from-buffers (oga/shell-buffer-name-list)))


(defun oga/docusaurus-port ()
  "Searches for \"Docusaurus is running at \" in the selected region's buffer and returns the line if found."
  (interactive)
  (let ((buf (buffer-substring-no-properties (region-beginning) (region-end))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (when (search-backward "Docusaurus website is running at " nil t)
          (message (thing-at-point 'line)))))))




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
;;(setq doom-theme 'wombat)
;;(setq doom-theme 'tango)
;;(setq doom-theme 'doom-opera)
;;(setq doom-theme 'doom-city-lights)
;;(setq doom-theme 'deeper-blue)
(setq doom-theme 'wheatgrass)


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
(setq default-input-method "japanese-mozc")

(bind-key* "s-\\" 'toggle-input-method)


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

;;soft wrapping
;;(global-visual-line-mode t)

;;turn off auto-fill
(add-hook 'markdown-mode-hook (lambda () (auto-fill-mode -1)))

;; set defarult comment charqcter.
(setq-default comment-start "# ")

;; #company
(setq company-idle-delay 0.2)
