;;; autoload/oga-calc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun oga/calc (expr)
    (insert (concat "\n" (number-to-string expr)))
)

;;;###autoload
(defun oga/calc-power (base exponent)
  "Calculate the power of BASE raised to EXPONENT."
  (if (= exponent 0)
      1
    (* base (oga/calc-power base (- exponent 1)))))

;; Example
;; (oga/power 2 3)  ;; calculates two to the third power -> 8
