;;; autoload/oga-calc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun oga/delete-line-no-kill ()
  "Delete line without adding to kill-ring."
  (interactive) 
  (delete-region (point)
                 (progn
                   (end-of-line)
                   (if (eobp)
                       (point)
                     (forward-char 1)
                     (point)))))
