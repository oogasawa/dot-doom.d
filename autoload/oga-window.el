;;; autoload/oga-window.el -*- lexical-binding: t; -*-

;;;###autoload
(defun oga/emacs-memory-usage ()
  "Display detailed Emacs memory usage and GC stats in the minibuffer."
  (interactive)
  (let* ((gc-stats (garbage-collect))
         (used-bytes (car (nth 8 gc-stats)))  ;; 使用中のバイト数
         (used-mb (/ used-bytes 1048576.0))  ;; MBに変換
         (gc-count gcs-done)  ;; GC実行回数
         (gc-time (format "%.2f sec" (float-time gc-elapsed)))  ;; GCの累積時間
         (gc-threshold (/ gc-cons-threshold 1048576.0)))  ;; GCのしきい値 (MB)
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

