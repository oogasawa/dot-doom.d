;;; autoload/oga-window.el -*- lexical-binding: t; -*-

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

;; === frame operations ===


;;;###autoload
(defun oga/perspective-A ()
  "Split the window vertically with the upper 2/3 and lower 1/3."
  (interactive)
  (let ((total-height (window-total-height)))
    (split-window-vertically (round (* total-height (/ 2.0 3.0)))))) 


;;;###autoload
(defun oga/perspective-B ()
  "Split the current frame into three windows."
  (interactive)
  (delete-other-windows)  ; Delete all other windows except the current one
  (split-window-right)    ; Split the window below
  (other-window 1)        ; Move to the next window
  (split-window-below)    ; Split the window below again
  (balance-windows))      ; Adjust the size of the windows evenly


;;;###autoload
(defun oga/perspective-C ()
  "Split the current frame into three horizontal windows."
  (interactive)
  (delete-other-windows)  ; Delete all other windows except the current one
  (split-window-below)    ; Split the window below
  (other-window 1)        ; Move to the next window
  (split-window-below)    ; Split the window below again
  (balance-windows))      ; Adjust the size of the windows evenly

