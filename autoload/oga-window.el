;;; autoload/oga-window.el -*- lexical-binding: t; -*-


;; === fast scrolling ===
;;;###autoload
(defun oga/scroll-up-half ()
  (interactive)
  (let ((window-half-height
         (max 1 (/ (1- (window-height (selected-window))) 2))))
    (scroll-up window-half-height)
    ))

;;;###autoload
(defun oga/scroll-down-half ()
  (interactive)
    (let ((window-half-height (max 1 (/ (1- (window-height (selected-window))) 2))))
    (scroll-down window-half-height)
    ))

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

