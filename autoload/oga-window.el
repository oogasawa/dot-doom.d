;;; autoload/oga-window.el -*- lexical-binding: t; -*-

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

;;;###autoload
(defun doom--get-normal-windows ()
  "Retrieve as list of regular windows excluding Neotree."
  (seq-filter
   (lambda (win) (not (string-prefix-p " *NeoTree*" (buffer-name (window-buffer win)))))
   (window-list)))

;;;###autoload
(defun oga/window-split-B ()
  "Adjust the regular windows to a 3/5 (top) : 2/5 (bottom) ratio, ignoring Neotree."
  (interactive)
  (let ((normal-windows (doom--get-normal-windows)))
    (when (= (length normal-windows) 2)
      (let* ((win1 (nth 0 normal-windows))
             (win2 (nth 1 normal-windows))
             (total-height (+ (window-total-height win1) (window-total-height win2)))
             (new-height (/ (* total-height 3) 5)))
        (select-window win1)
        (enlarge-window (- new-height (window-total-height win1)))))))


;;;###autoload
(defun oga/window-split-C ()
  "Adjust the regular windows to a 2/3 (top) : 1/3 (bottom) ratio, ignoring Neotree."
  (interactive)
  (let ((normal-windows (doom--get-normal-windows)))
    (when (= (length normal-windows) 2)
      (let* ((win1 (nth 0 normal-windows))
             (win2 (nth 1 normal-windows))
             (total-height (+ (window-total-height win1) (window-total-height win2)))
             (new-height (/ (* total-height 2) 3)))
        (select-window win1)
        (enlarge-window (- new-height (window-total-height win1)))))))


;;;###autoload
(defun oga/window-split-A ()
  "Restore the window split to the original 1/2 (top) : 1/2 (bottom) ratio, ingoring Neotree."
  (interactive)
  (let ((normal-windows (doom--get-normal-windows)))
    (when (= (length normal-windows) 2)
      (balance-windows))))



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

