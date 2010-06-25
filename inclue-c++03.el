;;; inclue-c++03.el --- C++ 03 header info for inclue.el

;; TODO write me              

(require 'inclue)

(inclue-load-mappings
 '("string"
   "std::string"))

(inclue-load-mappings
 '("algorithm"
   "std::find"
   "std::erase"))

(provide 'inclue-c++03)

;;; inclue-c++03.el ends here
