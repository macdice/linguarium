;;; cgen.el --- Experimental code generation hacks for amusement
;; Thomas Munro <munro@ip9.org>

;;; Commentary:
;;
;; Example of usage:
;;
;; (lambda (x y)
;;   (or (= x 7)
;;       (> x y)))
;;
;; ... when given as input to cgen-lambda->stl produces:
;;
;;  std::tr1::bind(std::logical_or<bool>(),
;;                 std::tr1::bind(std::equal_to<int>(),
;;                                std::tr1::placeholders::_1, 7),
;;                 std::tr1::bind(std::greater<int>(),
;;                                std::tr1::placeholders::_1,
;;                                std::tr1::placeholders::_2))
;;

;;; History:
;; 

(require 'cl)

;;; Code:

(defun cgen-build-bind (argmap functor args)
  "Generate a bind expression."
  (format "std::tr1::bind(%s, %s)"
          functor
          (mapconcat (lambda (x) (cgen-build-stl argmap x))
                     args
                     ", ")))

(defun cgen-build-stl (argmap expr)
  "Convert an expression to STL functors."
  ;; TODO convert (+ 1 2 3) into (+ 1 (+ 2 3)) and so on for and, or etc
  (cond ((consp expr)
         (ecase (car expr)
           ((+)
            (cgen-build-bind argmap "std::plus<int>()" (cdr expr)))
           ((-)
            (cgen-build-bind argmap "std::minus<int>()" (cdr expr)))
           ((/)
            (cgen-build-bind argmap "std::divides<int>()" (cdr expr)))
           ((*)
            (cgen-build-bind argmap "std::multiplies<int>()" (cdr expr)))
           ((=)
            (cgen-build-bind argmap "std::equal_to<int>()" (cdr expr)))
           ((<)
            (cgen-build-bind argmap "std::less<int>()" (cdr expr)))
           ((<=)
            (cgen-build-bind argmap "std::less_equal<int>()" (cdr expr)))
           ((>)
            (cgen-build-bind argmap "std::greater<int>()" (cdr expr)))
           ((>=)
            (cgen-build-bind argmap "std::greater_equal<int>()" (cdr expr)))
           ((and)
            (cgen-build-bind argmap "std::logical_and<bool>()" (cdr expr)))
           ((or)
            (cgen-build-bind argmap "std::logical_or<bool>()" (cdr expr)))
           ((not)
            (cgen-build-bind argmap "std::logical_not<bool>()" (second expr)))))
        ((numberp expr)
         (format "%d" expr))
        ((symbolp expr)
         (let ((l (assq expr argmap)))
           (unless l
             (error "Unknown variable %s" expr))
           (second l)))))

(defun cgen-lambda->stl (expression)
  "Compile a simple lambda expression to STL function adapters and binders."
  (unless (and (consp expression)
               (eq (first expression) 'lambda)
               (consp (second expression))
               (= (length expression) 3))
    (error "Malformed lambda expression"))
  (let* ((arglist (second expression))
         (argmap (loop for i from 1
                       for arg in arglist collect
                       (list arg
                             (format "std::tr1::placeholders::_%d" i))))
         (body (third expression)))
    (cgen-build-stl argmap body)))

(provide 'cgen)

;;; cgen.el ends here
