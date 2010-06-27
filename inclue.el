;;; inclue.el --- A simple-minded #include helper for C and C++

;;; Commentary:
;; 
;; M-x inclue-add-header-for-identifier-at-point tries to insert an
;; #include directive for the header that defines the identifier
;; (type, macro, function) at the point, according to the C or C++
;; standard (ie, not by scanning headers, but by using knowledge of
;; the standards), unless it is already present.  It tries to find a
;; reasonable location for the directive, according to my personal
;; header aesthetics...

;; M-x inclue-show-documentation-for-identifier-at-point shows very
;; limited documentation of function arguments to standard functions
;; in the message buffer, but it's not very clever compared to
;; c-eldoc-mode, CEDET summary, and various other modes out there that
;; use real langugae parsing (directly or indirectly) rather than
;; stupid keyword matching, so it's probably not much use to anyone.

;; M-x inclue-activate-library lets you control which libraries'
;; headers are considered when searching for the appropriate header;
;; this is important since there are names which are defined by both C
;; and C++ headers, and you probably prefer one over the other.

;; To use this you need to load one or more header definitions -- for
;; example, inclue-c99.el or inclue-c++03.el, and then activate one or
;; more in the current buffer.

;; It may seem stupid to have hand-maintained meta-data about header
;; when you could scan the headers themselves, but things are not
;; always defined in the header that the language standards require
;; you to include.  The problem seems more acute with standard library
;; implementations that with typical application code.  This is an
;; experimental approach to that problem which might turn out to be
;; useless.

;; Future plans:
;;
;; * look into options for scanning headers of other libraries
;; * research ways to interact with electric keys
;; * better heuristics for organising #include directives
;; * highlight the current argument in the message buffer (like SLIME)
;; * to try understand namespace/using scopes
;; * give up and learn about the Semantic project

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
    (skip-chars-backward "a-zA-Z_:") ;; TODO review characters
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
  ;; Intention: keep all the #include <...> directives together and
  ;; sorted alphabetically; keep all the #include "..." directives
  ;; together and sorted alphabetically except that the first one may
  ;; be special if its name is related to the name of the present file
  ;; (ie in foo.cpp, foo.h/foo.hpp should be the first thing that you
  ;; include)
  ;; TODO we only handle #include <...> (system/vendor headers) for now!
  ;; TODO step over special sibling header
  ;; TODO handle whitespace like "    #   include     <foo>"
  (save-excursion
    ;; find the last existing #include directive
    (goto-char (point-max))
    (if (re-search-backward "^#include" nil t)
        (progn
          (beginning-of-line))
      (goto-char 0))
    ;; we are now after the last one, or at top of buffer
    (let ((new-line (format "#include <%s>" name)))
      ;; walk up to the top of the current block of #includes
      (while (and (> (line-number-at-pos) 1)
                  (looking-at "#include"))
        (forward-line -1))
      ;; now walk down until we find the right insertion point
      (while (and (looking-at "#include")
                  (string< (thing-at-point 'line) new-line))
        (forward-line))
      (insert new-line)
      (insert "\n")
      ;; if the next line isn't an include or a blank line then insert
      ;; a new line
      (unless (or (looking-at " *# *include") (looking-at " *$"))
        (insert "\n")))))

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
                 (message "Header %s already included" header)
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
