;;; autoload/oga-god.el --- Global god-mode management -*- lexical-binding: t; -*-

;;;###autoload
(defun oga/god-toggle-global ()
  "Toggle god-mode globally."
  (interactive)
  (require 'god-mode)
  (if (bound-and-true-p god-global-mode)
      (progn
        (god-mode-all -1)
        (message "[god-mode] GLOBAL OFF"))
    (god-mode-all 1)
    (message "[god-mode] GLOBAL ON")))

;;;###autoload
(defun oga/setup-god-mode ()
  "Initialize and configure god-mode safely."
  (require 'god-mode)

  ;;--------------------------------------------------------------
  ;; Exempt modes
  ;;--------------------------------------------------------------
  (setq god-exempt-major-modes
        '(eshell-mode term-mode vterm-mode shell-mode comint-mode
          help-mode helpful-mode magit-status-mode treemacs-mode neotree-mode))
  (setq god-exempt-predicates
        '((lambda () (bound-and-true-p isearch-mode))))

  ;;--------------------------------------------------------------
  ;; Cursor feedback
  ;;--------------------------------------------------------------
  (defun oga/god-update-cursor ()
    "Update cursor type depending on god-mode state."
    (setq cursor-type (if (bound-and-true-p god-global-mode) 'box 'bar)))
  (add-hook 'god-mode-enabled-hook  #'oga/god-update-cursor)
  (add-hook 'god-mode-disabled-hook #'oga/god-update-cursor)

  ;;--------------------------------------------------------------
  ;; Easy exit
  ;;--------------------------------------------------------------
  (with-eval-after-load 'god-mode
    (define-key god-local-mode-map (kbd "i")        #'god-local-mode)
    (define-key god-local-mode-map (kbd "<escape>") #'god-local-mode))

  ;;--------------------------------------------------------------
  ;; Minibuffer handling
  ;;--------------------------------------------------------------
  (defvar oga/god-was-enabled-before-minibuffer nil
    "Remember god-mode state before entering minibuffer.")

  (defun oga/god-minibuffer-disable ()
    "Temporarily disable god-mode when entering minibuffer."
    (when (bound-and-true-p god-global-mode)
      (setq oga/god-was-enabled-before-minibuffer t)
      (god-mode-all -1))
    (when (bound-and-true-p god-local-mode)
      (god-local-mode -1)))

  (defun oga/god-minibuffer-restore ()
    "Restore god-mode state after leaving minibuffer."
    (when oga/god-was-enabled-before-minibuffer
      (setq oga/god-was-enabled-before-minibuffer nil)
      (god-mode-all 1)))

  (add-hook 'minibuffer-setup-hook #'oga/god-minibuffer-disable)
  (add-hook 'minibuffer-exit-hook  #'oga/god-minibuffer-restore)

  ;;--------------------------------------------------------------
  ;; Modeline indicator
  ;;--------------------------------------------------------------
  (defvar oga/god--lighter " GOD"
    "Indicator text for god-mode in modeline.")

  (defun oga/god-modeline-string ()
    "Return god-mode indicator for modeline."
    (if (bound-and-true-p god-global-mode)
        (propertize oga/god--lighter 'face '(:weight bold :foreground "#ff8800"))
      ""))

(with-eval-after-load 'doom-modeline
  (doom-modeline-def-segment god-mode
    "Show GOD indicator when active."
    (oga/god-modeline-string))
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp debug minor-modes input-method buffer-encoding major-mode vcs god-mode)))

  ;; Initialize cursor shape
  (oga/god-update-cursor))

(provide 'oga-god)
;;; oga-god.el ends here
