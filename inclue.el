;;; inclue.el --- A simple-minded #include helper for C++

;;; Commentary:
;; 
;; Sometimes it's hard to remember which header you're supposed to include,
;; according to the C++ standard, and sometimes your program will compile
;; on some compilers/libraries even if you don't get it right.  This is a
;; stupid hack to help me try to get it right at least sometimes.
;;
;; You need to load some mappings before it will be useful.
;;
;; TODO WORK IN PROGRESS

;;; History:
;; 
;; I have been planning this for years.  Actually I have been planning
;; something much more ambitious.  But this will do for now.

(require 'cl)

;;; Code:

(defun inclue-identifier-at-point ()
  "Return the C++ idendifier under the point."
  (save-excursion
    (skip-chars-backward "abcdefghijklmnopqrstuvwxyz_:")
    (if (looking-at "[a-zA-Z_:]+")
        (buffer-substring-no-properties (point) (match-end 0))
      nil)))

(defun inclue-has-header (name)
  "Test whether NAME is already included."
  (save-excursion
    (goto-char 0)
    (re-search-forward (format "#include +< *%s *>" name) nil t)))

(defun inclue-add-header (name)
  "Add the header NAME at a sensible location."
  ;; TODO improve heuristics to taste
  (save-excursion
    (goto-char 0)
    ;; find the last existing #include directive
    (when (re-search-forward "^ *#include.*" nil t)
      (goto-char 0)
      (while (re-search-forward "^ *#include.*" nil t)
        (forward-line)))
    ;; we are now after the last one, now find alphabetical insertion point
    (let ((end (point))
          (new-line (format "#include <%s>" name)))
      ;; TODO find alphabetical insertion point
      (insert new-line)
      (insert "\n"))))
        
(defvar inclue-mappings (make-hash-table :test 'equal)
  "A hash table of indentifier -> header.")

(defun inclue-load-mappings (header identifiers)
  "Load LIST consisting of header name followed by identifier strings."
  (loop for identifier in identifiers
        do (setf (gethash identifier inclue-mappings) header))))

(defun inclue-add-header-for-identifier-at-point ()
  "Add the appropriate header for the C++ identifier at point."
  (interactive)
  (let ((identifier (inclue-identifier-at-point)))
    (if identifier
        (let ((header (gethash identifier inclue-mappings)))
          (cond ((null header)
                 (message "I don't know the header to include for %s" identifier))
                ((inclue-has-header header)
                 (message "Header %s already included" header))
                (t
                 (message "Including header %s" header)
                 (inclue-add-header header))))
      (message "No recognisable identifier at point"))))
              
(provide 'inclue)

;;; inclue.el ends here
