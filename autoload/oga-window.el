;;; autoload/oga-window.el -*- lexical-binding: t; -*-

;;;###autoload
(defun oga/window-split-vertically ()
  "Split the window into two windows vertically, with a 3:1 ratio (top:bottom)."
  (interactive)
  (let* ((total-height (window-total-height))
         (top-height (floor (* total-height 0.75))))  ;; 3:1 ratio means top is 3/4
    (split-window-vertically top-height)))


;;;###autoload
(defun oga/window-swap ()
  "Swap the buffer in the current window with the buffer in the next window (cyclically)."
  (interactive)
  (let* ((this-win (selected-window))
         (next-win (next-window this-win))
         (buf1 (window-buffer this-win))
         (buf2 (window-buffer next-win))
         (start1 (window-start this-win))
         (start2 (window-start next-win)))
    (set-window-buffer this-win buf2)
    (set-window-buffer next-win buf1)
    (set-window-start this-win start2)
    (set-window-start next-win start1)))



;;;###autoload
(defun oga/window-toggle-truncate-lines ()
  "Toggle line truncation (truncate-lines) in the current buffer."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (recenter) ; o immediately reflect the visual change
  (message "truncate-lines is now %s" (if truncate-lines "ON (no wrapping)" "OFF (wrapped)")))


;;;###autoload
(defun oga/window-enlarge ()
  "Enlarge the current window by 10 lines vertically."
  (interactive)
  (enlarge-window 10))


;;;###autoload
(defun oga/window-shrink ()
  "Shrink the current window by 10 lines vertically."
  (interactive)
  (shrink-window 10))


;;;###autoload
(defun oga/emacs-memory-usage ()
  "Display detailed Emacs memory usage and GC stats in the minibuffer."
  (interactive)
  (let* ((used-bytes (car (memory-use-counts)))  ;; Total memory usage of Emacs (in bytes)
         (used-mb (/ (float used-bytes) 1048576.0))  ;; Convert to MB
         (gc-count gcs-done)  ;; Number of GC execution
         (gc-time (format "%.2f sec" (float-time gc-elapsed)))  ;; Cumulative GC time.
         (gc-threshold (/ (float gc-cons-threshold) 1048576.0)))  ;; GC threshold (MB)
    (message "Emacs memory: %.2f MB | GC: %d times (Total time: %s) | GC Threshold: %.2f MB"
             used-mb gc-count gc-time gc-threshold)))



;; === fast scrolling ===
;;;###autoload
(defun oga/scroll-up-half ()
  "ウィンドウの半分だけ上にスクロールし、カーソルの位置を調整します。"
  (interactive)
  (let* ((window (selected-window))
         (window-half-height (max 1 (/ (1- (window-height window)) 2))))
    (scroll-up-command window-half-height)
    (when (> (point) (window-end window t))
      (set-window-point window (window-end window t)))))

;;;###autoload
(defun oga/scroll-down-half ()
  "ウィンドウの半分だけ下にスクロールし、カーソルの位置を調整します。"
  (interactive)
  (let* ((window (selected-window))
         (window-half-height (max 1 (/ (1- (window-height window)) 2))))
    (scroll-down-command window-half-height)
    (when (< (point) (window-start window))
      (set-window-point window (window-start window)))))


;; === window operations ===
;;;###autoload
(defun oga/window-previous()
  (interactive)
  (other-window -1))

;;;###autoload
(defun oga/window-next()
  (interactive)
  (other-window 1))

