;;; ermite.el --- An experimental multithreaded frankenlisp inside Emacs

;;; Commentary:
;;
;; WORK IN PROGRESS -- only an initial sketch at this stage
;;
;; Ermite is an experimental frankenlisp hosted inside Emacs, mainly
;; written as an exercise to learn more about continuations.  Some
;; lofty vague initial intentions:
;;
;; * Lisp 1, like Scheme (except when calling Emacs Lisp)
;; * first class continuations, like Scheme
;; * tail call elimination, like Scheme
;; * semi-preemptive lightweight threads
;; * call interoperability with Emacs Lisp functions
;; * Emacs's t/nil system for truthiness, nilhood and emptity
;; * multiplexed IO operations built on continuations
;;
;; Somethings I would like to be able to do:
;;
;; * spawn threads, and communicate with them via mailboxes and
;;   pattern matching, generally like Termite Scheme; these
;;   threads are controlled by a scheduler that runs in Emacs's
;;   various callback-based event handlers (like process filters for
;;   incoming data, and idle timers etc)
;; * allow a programming context where synchronous style code (ie
;;   blocking operations like socket-read) are implemented on top of
;;   the scheduling scheme
;; * be able to wrap existing Emacs library like url.el so that
;;   callback-based interfaces can be used in synchronous style
;; * maybe even serialise procedures (including continuations)
;;   over the wire between Emacs processes
;;
;; Decisions to make:
;;
;; * should function arguments have dynamic scope (like Emacs) or
;;   lexical scope (like Scheme & CL)?  depends on whether I'm trying
;;   to 'add continuation-based IO multiplexing to Emacs Lisp' but
;;   leave Emacs Lisp basically as it is, or build something much more
;;   like Termite Scheme, that just happens to live inside Emacs

(require 'cl)

;;; Code:

(defstruct ermite-thread
  "A record type for threads."
  (state)                    ;; :run, :wait, :sleep
  (pc)                       ;; function to invoke
  (variables))               ;; captured dynamic variables

(defstruct ermite-function
  "A record type for compiled Ermite functions."
  (name)
  (arguments)
  (body)
  (compiled))

(defvar ermite-running nil
  "Whether the scheduler is running.")

(defvar ermite-run-queue nil
  "Threads waiting for a timeslice to run.")

(defvar ermite-wait-queue nil
  "Threads waiting for IO.")

(defvar ermite-sleep-queue nil
  "Threads waiting for time to pass.")

(defvar ermite-current-thread nil
  "The thread currently running.")

(defvar ermite-stack nil
  "The stack where arguments are pushed.")

(defvar ermite-return-stack nil
  "The return stack for function calls.")

(defvar ermite-pc nil
  "The program counter.")

(defvar ermite-timeslice-size 100
  "How many instructions run before threads are rescheduled.")

(defun ermite-function-preamble (formals)
  "Generate the calling convention preamble."
  ;; TODO for now we just set all the arguments as global variables!
  ;; TODO sort out scoping discipline etc
  (append '(new-stack-frame)
          (loop for formal in formals
                append (list 'set-global formal))))

(defun ermite-compile-progn (exprs tailp)
  "Compile a list of expressions keeping only the last value."
  (loop for (head . tail) on exprs
        append (if tail
                   (append (ermite-compile head nil) '(pop))
                   (ermite-compile head tailp))))

(defun ermite-compile (expr tailp)
  "Transform the source code of EXPR into Ermite virtual machine code."
  (cond ((consp expr)
         (case (car expr)
           ((if)
            (let ((test (ermite-compile (second expr) nil))
                  (consequent (ermite-compile (third expr) t))
                  (alternative (if (= (length expr) 3)
                                   '(constant nil)
                                 (ermite-compile-progn (nthcdr 3 expr) t))))
              (append test 
                      (list 'goto-if consequent alternative))))          
           ((progn)
            (ermite-compile-progn (cdr expr) tailp))
           ((setq)
            (append (ermite-compile (third expr) tailp)
                    (list 'set-global (second expr))))
           ((lambda)
            (let ((formals (second expr))
                  (body (nthcdr 2 expr)))
              (list 'constant
                    (make-ermite-function
                     :compiled (append (ermite-function-preamble formals)
                                       (ermite-compile-progn body tailp))))))
           (t
            (append (loop for argument in (reverse (cdr expr))
                          append (ermite-compile argument nil))
                    (ermite-compile (car expr) nil)
                    (list (if tailp 'apply-tail 'apply))))))
        ((symbolp expr)
         (list 'get-global expr))
        (t
         (list 'constant expr))))

(defun ermite-run-timeslice ()
  "Run code for the current thread until timeslice is used up, or
a blocking operation is encountered, or there is no more code to
run."
  (while (plusp ermite-timeslice)
    (cond ((null ermite-pc)
           (if ermite-return-stack
               (setf ermite-pc (pop ermite-return-stack))
             (setf ermite-timeslice 0)
             (setf (ermite-thread-state ermite-current-thread) :finished)))
          (t
           (ecase (pop ermite-pc)
             ((new-stack-frame)
              (push ermite-stack ermite-frames)
              (setf ermite-stack nil))
             ((constant)
              (push (pop ermite-pc) ermite-stack))
             ((pop)
              (pop ermite-stack))
             ((goto-if)
              (let ((consequent (pop ermite-pc))
                    (alternative (pop ermite-pc)))
                (if (pop ermite-stack)
                    (setf ermite-pc consequent)
                    (setf ermite-pc alternative))))
             ((goto)
              (setf ermite-pc (pop ermite-pc)))
             ((set-global)
              (setf (symbol-value (pop ermite-pc)) (pop ermite-stack)))
             ((get-global)
              (let ((name (pop ermite-pc)))
                (cond ((boundp name)
                       (push (symbol-value name) ermite-stack))
                      ((fboundp name)
                       (push (symbol-function name) ermite-stack))
                      (t
                       (error "Unbound variable %s" name)))))
             ((apply-tail)
              (let ((f (pop ermite-stack)))
                (if (ermite-function-p f)                    
                    (setf ermite-pc (ermite-function-compiled f))
                  (let ((arguments ermite-stack))
                    (setf ermite-stack nil)
                    (push (apply f arguments) ermite-stack)))))
             ((apply)
              (let ((f (pop ermite-stack)))
                (if (ermite-function-p f)
                    (progn
                      (push ermite-pc ermite-return-stack)
                      (setf ermite-pc (ermite-function-compiled f)))
                  (let ((arguments ermite-stack))
                    (setf ermite-stack nil)
                    (push (apply f arguments) ermite-stack))))))))
    (decf ermite-timeslice)))

(defun ermite-apply (fun arguments)
  "An interface allowing standard Emacs Lisp to call Ermite functions."
  (let ((ermite-pc (ermite-function-compiled fun))
        (ermite-current-thread (make-ermite-thread :state :run)))
    (setf ermite-stack arguments)
    (while (eq (ermite-thread-state ermite-current-thread) :run)
      (setf ermite-timeslice ermite-timeslice-size)
      (ermite-run-timeslice))
    (pop ermite-stack)))

(defmacro* ermite-lambda (args &body body)
  "A convenience macro for creating Ermite functions from Emacs Lisp.
Invoking this macro in Emacs Lisp is equivalent to using the form
'lambda' inside Ermite code in that it returns an Ermite
function."
  `(make-ermite-function
    :arguments ',args
    :body ',(if (> (length body) 1)
                `(progn ,@body)
                (car body))
    :compiled (append (ermite-function-preamble ',args)
                      (ermite-compile-progn ',body t))))

(defmacro* define-ermite-function (name args &body body)
  "A convenience macro for defining an Ermite function and
storing it in the value slot of a symbol, while storing an Emacs
Lisp adapter function in the function cell.  The resulting
function can therefore be called from Emacs Lisp AND Ermite code
with the same name.  I told you it was a frankenlisp."
  (let ((arglist (gensym)))
    `(progn
       (setf ,name (ermite-lambda ,args ,@body))
       (defun ,name (&rest ,arglist) (ermite-apply ,name ,arglist)))))

(defun ermite-spawn (k)
  "Start a new thread using K."
  'TODO)

(defun ermite-start ()
  "Start the scheduler."
  (unless (not (ermite-running))
    (error "Already running"))
  ;; TODO use the Emacs idle timer?
  (setf ermite-running t))

(defun ermite-stop ()
  "Stop the scheduler."
  42)

(provide 'ermite)

;;; ermite.el ends here
