;;; inclue.el --- A simple-minded #include helper for C and C++

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

(defstruct inclue-library
  "A record type to hold documentation and header info."
  (identifier)
  (include-style 'angle-brackets)
  (names)                                               ;; list of strings
  (header-table (make-hash-table :test 'equal))         ;; string -> string
  (documentation-table (make-hash-table :test 'equal))) ;; string -> string

(defvar inclue-library-table (make-hash-table :test 'eq)
  "A hash-table of library identifier symbol -> inclue-library.")

(defvar inclue-active-libraries '()
  "A list of active library identifiers (symbols).  For example,
  this could be set to the list (c-99 posix-1003) while
  programming in C, and the list (c++-03 posix-1003) while
  programming in C++, possibly in a mode hook.  TODO: make buffer
  local")

(defun inclue-load-mappings (library-identifier 
                             header 
                             identifier-documentation-pairs)
  "Load LIST consisting of header name followed by identifier strings."
  (let ((library (gethash library-identifier inclue-library-table)))
    (unless library
      (error "inclue-load-mappings -- unknown library %s" library-identifier))
    (loop 
     for (identifier doc-string) in identifier-documentation-pairs 
     as name = (symbol-name identifier) do 
     (push name (inclue-library-names library))
     (setf (gethash name (inclue-library-header-table library))
           header)
     (setf (gethash name (inclue-library-documentation-table library))
           doc-string))))

(defmacro* define-inclue-library ((identifier include-style) doc-string)
  `(setf (gethash ',identifier inclue-library-table)
         (make-inclue-library :identifier ',identifier
                              :include-style ',include-style)))

(defmacro define-inclue-header (desc doc-string &rest identifier-pairs)
  `(inclue-load-mappings ',(first desc) ,(second desc) ',identifier-pairs))

(defun inclue-add-header-for-identifier-at-point ()
  "Add the appropriate header for the C++ identifier at point."
  (interactive)
  (let ((identifier (inclue-identifier-at-point)))
    (loop
     for library-identifier in inclue-active-libraries until
     (let ((library (gethash library-identifier inclue-library-table)))
       (when library
         (let ((header (gethash identifier 
                                (inclue-library-header-table library))))
           (when header
             (if (inclue-has-header header)
                 (message "Header %s already included" header) ;; terminate
               (progn
                 (message "Including header %s" header)
                 (inclue-add-header header))))))))))

(defun inclue-show-documentation-for-identifier-at-point ()
  "Show the documentation string for the identifier at point, if we have it."
  (interactive)
  (let ((identifier (inclue-identifier-at-point)))
    (loop for library-identifier in inclue-active-libraries do
          (let ((library (gethash library-identifier inclue-library-table)))
            (when library
              (let ((documentation
                     (gethash identifier 
                              (inclue-library-documentation-table library))))
                (when documentation
                  (message documentation))))))))

(defun inclue-activate-library ()
  "Activate registered libraries in the current buffer."
  (interactive)
  (let ((library-name
         (completing-read 
          "Activate library: "
          (loop for identifier being the hash-keys of inclue-library-table 
                collect (symbol-name identifier))
          nil
          t)))
    (add-to-list 'inclue-active-libraries (intern library-name))))

(defun inclue-construct-ac-candidates ()
  "Build a list of competions from all currently active libraries."
  ;; TODO find out how to plug this into auto-complete.el
  (loop for library being the hash-value of inclue-library-table
        when (memq (inclue-library-identifier library) inclue-active-libraries)
        append (inclue-library-names library)))

(provide 'inclue)

;;; inclue.el ends here
