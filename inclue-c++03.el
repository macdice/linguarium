;;; inclue-c++03.el --- C++ 03 header info for inclue.el
;; Copyright (c) 2010 Thomas Munro munro@ip9.org

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; TODO this is inkorrect and incomplet, WRITE ME


;;; Commentary:
;; 
;; Incomplet, inkorrect, work in progress...

;;; History:
;; 

(require 'inclue)

;;; Code:

(define-inclue-library (c++03 angle-brackets)
  "The C++ standard library headers, from ANSI ISO IEC 14882 2003.")

(define-inclue-header (c++03 "exception")
  "18.6. Exceptio handling [lib.support.exception]."
  (std::exception)
  (std::bad_exception)
  (std::unexpected_handler)
  (std::terminate_handler)
  (std::set_unexpected "std::unexpected_handler std::set_unexpected(std::unexpected_handler f) throw ()")
  (std::set_terminate "std::terminate_handler std::set_terminate(std::terminate_handler f) throw ()")
  (std::terminate "void std::terminate()")
  (std::uncaught_exception "bool std::uncaught_exception() throw ()"))

(define-inclue-header (c++03 "stdexcept")
  "19.1. Exception classes [lib.std.exceptions]."
  (std::logic_error "std::logic_error(const string& what)")
  (std::domain_error "std::domain_error(const string& what)")
  (std::invalid_argument "std::invalid_argument(const string& what)")
  (std::length_error "std::length_error(const string& what)")
  (std::out_of_range "std::out_of_range(const string& what)")
  (std::runtime_erro "std::runtime_error(const string& what)")
  (std::range_error "std::range_error(const string& what)")
  (std::overflow_error "std::overflow_error(const string& what)")
  (std::underflow_error "std::underflow_error(const string& what)"))

(define-inclue-header (c++03 "cassert")
  "19.2. Assertions [lib.assertions]."
  (assert "assert(...)"))

(define-inclue-header (c++03 "cerrno")
  "19.3. Error numbers [lib.errno]."
  ;; TODO
  (ERANGE)
  (EDOM)
  (errno))

(define-inclue-header (c++03 "limits")
  "The <limits> header."
  (std::numeric_limits)
  (std::float_round_style)
  (std::float_denorm_style)
  (std::numeric_limits))

(define-inclue-header (c++03 "climits")
  "The <climits> header."
  (CHAR_BIT)
  (CHAR_MAX)
  (CHAR_MIN)
  (INT_MAX)
  (INT_MIN)
  (LONG_MAX)
  (LONG_MIN)
  (MB_LEN_MAX)
  (SCHAR_MAX)
  (SCHAR_MIN)
  (SHRT_MAX)
  (SHRT_MIN)
  (UCHAR_MAX)
  (UINT_MAX)
  (ULONG_MAX)
  (USHRT_MAX)) ;; TODO

(define-inclue-header (c++03 "utility")
  "The General utilities library [lib.utilities]."
  (std::pair "std::pair(const T1& x, const T2& y)")
  (std::make_pair "std::pair<T1, T2> std::make_pair(T1, T2)"))

(define-inclue-header (c++03 "functional")
  "The functional library [lib.function.objects]."
  (std::plus)
  (std::minus)
  (std::multiplies)
  (std::divides)
  (std::modulus)
  (std::negate)
  (std::equal_to)
  (std::not_equal_to)
  (std::greater)
  (std::less)
  (std::greater_equal)
  (std::less_equal)
  (std::logical_and)
  (std::logical_or)
  (std::logical_not)
  (std::unary_negate)
  (std::not1 "std::unary_negate<Predicate> not1(const Predicate&)")
  (std::binary_negate)
  (std::not2 "std::binary_negate<Predicate> not2(const Predicate&)")
  (std::binder1st)
  (std::bind1st "std::binder1st<Operation> bind1st(const Operator&, const T&)")
  (std::binder2nd)
  (std::bind2nd "std::binder2nd<Operation> bind2nd(const Operator& const T&)")
  (std::pointer_to_unary_function)
  (std::ptr_fun "std::pointer_to_unary_function<Arg, Result> std::ptr_fun(Result(*)(Arg))
std::pointer_to_binary_function<Arg1, Arg2, Result> std::ptr_fun(Result(*)(Arg1, Arg2))")
  (std::pointer_to_binary_function)
  (std::mem_fun_t)
  (std::mem_fun1_t)
  (std::mem_fun "std::mem_fun_t<S, T> std::mem_fun(S (T::*f)())
std::const_mem_fun_t<S, T> std::mem_fun(S (T::*f)() const)
std::mem_fun1_t<S, T, A> std::mem_fun(S (T::*f)(A))
std::const_mem_fun1_t<S, T, A> std::mem_fun(S (T::*f)(A) const)")
  (std::mem_fun_ref_t)
  (std::mem_fun1_ret_t)
  (std::mem_fun_ref "std::mem_fun_ref_t<S, T> std::mem_fun_ref(S (T::*f)())
std::const_mem_fun_ref_t<S, T> std::mem_fun_ref(S (T::*f)() const)
std::mem_fun_ref1_t<S, T, A> std::mem_fun_ref(S (T::*s)(A))
std::const mem_fun_ref1_t<S, T, A> std::mem_fun_ref(S (T::*s)(A) const)")
  (std::const_mem_fun_t)
  (std::const_mem_fun1_t))

(define-inclue-header (c++03 "memory")
  "The Memory library [lib.memory]."
  (std::allocator)
  (std::raw_storage_iterator)
  (std::get_temporary_buffer "std::pair<T*, std::ptrdiff_t> std::get_temporary_buffer(std::ptrdirr_t n)")
  (std::return_temporary_buffer "void std::return_temporary_buffer(T* p)")
  (std::uninitialized_copy "ForwardIterator std::uninitialized_copy(InputIterator first, InputIterator last, const T& x)")
  (std::uninitialized_fill "void std::uninitialized_fill(ForwardIterator first, ForwardIterator last, const T& x)")
  (std::uninitialized_fill_n "void std::uninitialized_fill_n(ForwardIterator first, Size n, const T& x)")
  (std::auto_ptr "std::auto_ptr(X* p = 0)
std::auto_ptr(std::auto_ptr&)
std::auto_ptr(std::auto_ptr<Y>&)"))

(define-inclue-header (c++03 "cstdlib")
  "The Standard C library [lib.c.malloc]."
  (EXIT_FAILURE)
  (EXIT_SUCCESS)
  (std::abort "void std::abort()")
  (std::exit "void exit(int status)")
  (std::calloc)
  (std::malloc)
  (std::free)
  (std::realloc)
  (std::atol)
  (std::atof)
  (std::atoi)
  (std::mblen)
  (std::mbstowcs)
  (std::mbtowc)
  (std::strtod)
  (std::strtol)
  (std::strtoul)
  (std::wctomb)
  (std::wcstombs))

(define-inclue-header (c++03 "locale")
  "The locale library."
  (std::use_facet "const Facet& std::use_facet(const std::locale&)")
  (std::has_facet "bool std::has_facet(const std::locale&)")
  (std::isspace "bool std::isspace(charT c, const std::locale& loc)")
  (std::isprint "bool std::isprint(charT c, const std::locale& loc)")
  (std::iscntrl "bool std::iscntrl(charT c, const std::locale& loc)")
  (std::isupper "bool std::isupper(charT c, const std::locale& loc)")
  (std::islower "bool std::islower(charT c, const std::locale& loc)")
  (std::isalpha "bool std::isalpha(charT c, const std::locale& loc)")
  (std::isdigit "bool std::isdigit(charT c, const std::locale& loc)")
  (std::ispunct "bool std::ispunct(charT c, const std::locale& loc)")
  (std::isxdigit "bool std::isxdigit(charT c, const std::locale& loc)")
  (std::isalnum "bool std::isalnum(charT c, const std::locale& loc)")
  (std::isgraph "bool std::isgraph(charT c, const std::locale& loc)")
  (std::toupper "charT std::toupper(charT c, const std::locale& loc)")
  (std::tolower "charT std::tolower(charT c, const std::locale& loc)")
  (std::ctype)
  (std::ctype_byname)
  (std::codecvt)
  (std::codecvt_byname)
  (std::num_get)
  (std::num_put)
  (std::numpunct)
  (std::numpunct_byname)
  (std::collate)
  (std::collate_byname)
  (std::time_get)
  (std::time_get_byname)
  (std::time_put)
  (std::time_put_byname)
  (std::money_get)
  (std::money_put)
  (std::moneypunct)
  (std::moneypunct_byname)
  (std::locale "std::locale()
std::locale(const std::locale&)
explicit std::locale(const char *name)
std::locale(const std::locale& other, const char *std_name, category)
std::locale(const std::locale& other, Facet *f)
std::locale(const std::locale& other, const std::locale& one, category)"))


(define-inclue-header (c++03 "cstring")
  "The C library string header."
  (std::size_t)
  (std::memchr)
  (std::memcmp)
  (std::memmove)
  (std::memset)
  (std::memcpy)
  (std::strcat)
  (std::strchr)
  (std::strcmp)
  (std::strcoll)
  (std::strcpy)
  (std::strcspn)
  (std::strerror)
  (std::strlen)
  (std::strncat)
  (std::strncmp)
  (std::strncpy)
  (std::strpbrk)
  (std::strrchr)
  (std::strspn)
  (std::strstr)
  (str::strtok)
  (str::strxfrm))


(define-inclue-header (c++03 "ctime")
  "The C library time header."
  (std::clock_t)
  (std::time_t)
  (std::tm)
  (std::asctime)
  (std::ctime)
  (std::clock)
  (std::gmtime)
  (std::difftime)
  (std::mktime)
  (std::localtime)
  (std::time)
  (std::strftime))

(define-inclue-header (c++03 "string")
  "The string library."
  ;; TODO string vs basic_string...
  (std::char_traits)
  (std::basic_string)
  (std::string "std::string(const Allocator& a = Allocator())
std::string(const std::basic_string& str)
std::string(const std::basic_string& str, size_type pos, size_type n = npos, const Allocator& a = Allocator())
std::string(const charT* s, size_type n, const Allocator& a = Allocator())
std::string(const charT* s, const Allocator& a = Allocator())
std::string(InputIterator begin, InputIterator end, const Allocator& a = Allocator())
std::string()")
  (std::getline "std::basic_istream<charT, traits>& std::getline(std:basic_istream<charT, traits>& is, std::basic_string<charT, traits, Allocator>& str)
std::basic_istream<charT, traits>& std::getline(std:basic_istream<charT, traits>& is, std::basic_string<charT, traits, Allocator>& str, charT delim)"))

(define-inclue-header (c++03 "cctype")
  "The character type library."
  (std::isalnum)
  (std::isalpha)
  (std::iscntrl)
  (std::isdigit)
  (std::isgraph)
  (std::islower)
  (std::isprint)
  (std::ispunct)
  (std::isspace)
  (std::isupper)
  (std::isxdigit)
  (std::tolower)
  (std::toupper))

(define-inclue-header (c++03 "cwctype")
  "The wide character library."
  (std::wctrans_t)
  (std::wctype_t)
  (std::wint_t)
  (std::iswalnum)
  (std::iswalpha)
  (std::iswcntrl)
  (std::iswctype)
  (std::iswdigit)
  (std::iswgraph)
  (std::iswlower)
  (std::iswprint)
  (std::iswpunct)
  (std::iswspace)
  (std::iswupper)
  (std::iswxdigit)
  (std::towctrans)
  (std::towlower)
  (std::towupper)
  (std::wctrans)
  (std::wctype))

(define-inclue-header (c++03 "algorithm")
  "The Algorithms library [lib.algorithms]."
  (std::for_each "Function std::for_each(InputIterator first, InputIterator last, Function f)")
  (std::find "InputIterator std::find(InputIterator first, InputIterator last, const T& value)")
  (std::find_if "InputIterator std::find_if(InputIterator first, InputIterator last, Predicate p)")
  (std::find_end "ForwardIterator1 std::find_end(ForwardIterator1 first1, ForwardIterator1 last1, ForwardIterator2 first2, ForwardIterator2 last2)
ForwardIterator1 std::find_end(ForwardIterator1 first1, ForwardIterator1 last1, ForwardIterator2 first2, ForwardIterator2 last2, BinaryPredicate pred)")
  (std::find_first_of "ForwardIterator1 std::find_first_of(ForwardIterator1 first1, ForwardIterator1 last1, ForwardIterator2 first2, ForwardIterator2 last2)
ForwardIterator1 std::find_first_of(ForwardIterator1 first1, ForwardIterator1 last1, ForwardIterator2 first2, ForwardIterator2 last2, BinaryPredicate pred)")
  (std::adjacent_find "ForwardIterator std::adjacent_find(ForwardIterator first, ForwardIterator last)
ForwardIterator std::adjacent_find(ForwardIterator first, ForwardIterator last, BinaryPredicate pred])")
  (std::count "std::iterator_traits<InputIterator>::difference_type std::count(InputIterator first, InputIterator last)")
  (std::count_if "std::iterator_traits<InputIterator>::difference_type std::count(InputIterator first, InputIterator last, Predicate pred)")
  (std::mismatch "std::pair<InputIterator1, InputIterator2> std::mismatch(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2)
std::pair<InputIterator1, InputIterator2> std::mismatch(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, BinaryPredicate pred)")
  (std::equal "bool std::equal(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2[, BinaryPredicate pred])")
  (std::search "ForwardIterator1 std::search(ForwardIterator1 first1, ForwardIterator1 last1, ForwardIterator2 first2, ForwardIterator2 last2)
ForwardIterator1 std::search(ForwardIterator1 first1, ForwardIterator1 last1, ForwardIterator2 first2, ForwardIterator2 last2, BinaryPredicate pred)")
  (std::search_n "ForwardIterator std::search_n(ForwardIterator first, ForwardIterator last, Size count, const T& value)
ForwardIterator std::search_n(ForwardIterator first, ForwardIterator last, Size count, const T& value, BinaryPredicate pred)")
  (std::copy "OutputIterator std::copy(InputIterator first, InputIterator last, OutputIterator result)")
  (std::copy_backward "BidirectionalIterator2 std::copy_backward(BidirectionalIterator1 first, BidirectionalIterator1 last, BidirectionalIterato2 result)")
  (std::swap "void std::swap(T& a, T& b)")
  (std::swap_ranges "ForwardIterator2 std::swap_ranges(ForwardIterator1 first1, ForwardIterator1 last1, ForwardIterator2 first1)")
  (std::iter_swap "void std::iter_swap(ForwardIterator1 a, ForwardIterator2 b)")
  (std::transform "OuputIterator std::transform(InputIterator first, InputIterator last, OuputIterator result, UnaryOperation op)
OuputIterator std::transform(InputIterator1 first1, InputIterator1 last1, InpuIterator2 first2, OutputIterator result, BinaryOperation binary_op)")
  (std::replace "void std::replace(ForwardIterator first, ForwardIterator last, const T& old_value, const T& new_value)")
  (std::replace_if "void std::replace_if(ForwardIterator first, ForwardIterator last, Predicate pred, const T& new_value)")
  (std::replace_copy "OutputIterator replace_copy(InputIterator first, InputIterator last, OutputIterator result, const T& old_value, const T& new_value)")
  (std::replace_copy_if "OutputIterator replace_copy_if(Iterator first, Iterator last, OutputIterator result, Predicate pred, const T& new_value)")
  (std::fill "void std::fill(ForwardIterator first, ForwardIterator last, const T& value)")
  (std::fill_n "void std::fill_n(OutputIterator first, Size n, const T& value)")
  (std::generate "void std::generate(ForwardIterator first, ForwardIterator last, Generator gen)")
  (std::generate_n "void std::generate_n(OutputIterator first, Size n, Generator gen)")
  (std::remove "ForwardIterator std::remove(ForwardIterator first, ForwardIterator last, const T& value)")
  (std::remove_if "ForwardIterator std::remove_if(ForwardIterator first, ForwardIterator last, Predicate pred)")
  (std::remove_copy "OutputIterator std::remove_if_copy(InputIterator first, InputIterator last, OutputIterator result, const T& value)")
  (std::remove_if_copy "OutputIterator std::remove_if_copy(InputIterator first, InputIterator last, OutputIterator result, Predicate pred)")
  (std::unique "ForwardIterator std:unique(ForwardIterator first, ForwardIterator last)
ForwardIterator std:unique(ForwardIterator first, ForwardIterator last, BinaryPredicate pred)")
  (std::unique_copy "OutputIterator std::unique_copy(InputIterator first, InputIterator last, OutputIterator result)
OutputIterator std::unique_copy(InputIterator first, InputIterator last, OutputIterator result, BinaryPredicate pred)")
  (std::reverse "void std::reverse(BidirectionalIterator first, BidirectionalIterator last)")
  (std::reverse_copy "OutputIterator std::reverse_copy(BidirectionalIterator first, BidirectionalIterator last, OutputIterator result)")
  (std::rotate "void std::rotate(ForwardIterator first, ForwardIterator middle, ForwardIterator last)")
  (std::rotate_copy "void std::rotate_copy(ForwardIterator first, ForwardIterator middle, ForwardIterator last, OutputIterator result)")
  (std::random_shuffle "void std::random_shuffle(RandomAccessIterator first, RandomAccessIterator last)
void std::random_shuffle(RandomAccessIterator first, RandomAccessIterator last, RandomNumberGenerator& rand)")
  (std::partition "BidirectionalIterator std::partition(BidirectionalIterator first, BidirectionalIterator last, Predicate pred)")
  (std::stable_partition "BidirectionalIterator std::stable_partition(BidirectionalIterator first, BidirectionalIterator last, Predicate pred)")
  (std::sort "void std::sort(RandomAccessIterator first, RandomAccessIterator last)
void std::sort(RandomAccessIterator first, RandomAccessIterator last, Compare comp)")
  (std::stable_sort "void std::stable_sort(RandomAccessIterator first, RandomAccessIterator last)
void std::stable_sort(RandomAccessIterator first, RandomAccessIterator last, Compare comp)")
  (std::partial_sort "void std::partial_sort(RandomAccessIterator first, RandomAccessIterator middle, RandomAccessIterator last)
void std::partial_sort(RandomAccessIterator first, RandomAccessIterator middle, RandomAccessIterator last, Compare comp)")
  (std::partial_sort_copy "void std::partial_sort_copy(RandomAccessIterator first, RandomAccessIterator last, RandomAccessIterator result_first, RandomAccessIterator result_last)
void std::partial_sort(RandomAccessIterator first, RandomAccessIterator last, RandomAccessIterator result_first, RandomAccessIterator result_last, Compare comp)")
  (std::nth_element "void std::nth_element(RandomAccessIterator first, RandomAccessIterator nth, RandomAccessIterator last)
void std::nth_element(RandomAccessIterator first, RandomAccessIterator nth, RandomAccessIterator last, Compare comp)")
  (std::lower_bound "ForwardIterator std::lower_bound(ForwardIterator first, ForwardIterator last, const T& value)
ForwardIterator std::lower_bound(ForwardIterator first, ForwardIterator last, const T& value, Compare comp)")
  (std::upper_bound "ForwardIterator std::upper_bound(ForwardIterator first, ForwardIterator last, const T& value)
ForwardIterator std::upper_bound(ForwardIterator first, ForwardIterator last, const T& value, Compare comp)")
  (std::equal_range "std::pair<ForwardIterator, ForwardIterator> std::equal_range(ForwardIterator first, ForwardIterator last, const T& value)
std::pair<ForwardIterator, ForwardIterator> std::equal_range(ForwardIterator first, ForwardIterator last, const T& value, Compare comp)")
  (std::binary_search "bool std::binary_search(ForwardIterator first, ForwardIterator last, const T& value)
bool std::binary_search(ForwardIterator first, ForwardIterator last, const T& value, Compare comp)")
  (std::merge "OutputIterator std::merge(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, OutputIterator result)
OutputIterator std::merge(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, OutputIterator result, Compare comp)")
  (std::inplace_merge "void std::inplace_merge(BidirectionalIterator first, BidirectionalIterator middle, BidirectionalIterator last)
void std::inplace_merge(BidirectionalIterator first, BidirectionalIterator middle, BidirectionalIterator last, Compare comp)")
  (std::includes "bool std::includes(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2)
bool std::includes(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, Compare comp)")
  (std::set_union "OutputIterator std::set_union(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, OuputIterator result)
OutputIterator std::set_union(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, OuputIterator result, Compare comp)")
  (std::set_intersection "OutputIterator std::set_intersection(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, OutputIterator result)
OutputIterator std::set_intersection(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, OutputIterator result, Compare comp)")
  (std::set_difference "OutputIterator std::set_difference(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, OutputIterator result)
OutputIterator std::set_difference(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, OutputIterator result, Compare comp)")
  (std::set_symmetric_difference "OutputIterator std::set_symmetric_difference(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, OutputIterator result)
OutputIterator std::set_symmetric_difference(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, OutputIterator result, Compare comp)")
  (std::push_heap "void std::push_heap(RandomAccessIterator first, RandomAccessIterator last)
void std::push_heap(RandomAccessIterator first, RandomAccessIterator last, Compare comp)")
  (std::pop_heap "void std::pop_heap(RandomAccessIterator first, RandomAccessIterator last)
void std::pop_heap(RandomAccessIterator first, RandomAccessIterator last, Compare comp)")
  (std::make_heap "void std::make_heap(RandomAccessIterator first, RandomAccessIterator last)
void std::make_heap(RandomAccessIterator first, RandomAccessIterator last, Compare comp)")
  (std::sort_heap "void std::sort_heap(RandomAccessIterator first, RandomAccessIterator last)
void std::sort_heap(RandomAccessIterator first, RandomAccessIterator last, Compare comp)")
  (std::min "const T& min(const T& a, const T& b)
const T& min(const T& a, const T& b, Compare comp)")
  (std::max "const T& max(const T& a, const T& b)
const T& max(const T& a, const T& b, Compare comp)")
  (std::min_element "ForwardIterator std::min_element(ForwardIterator first, ForwardIterator last)
ForwardIterator std::min_element(ForwardIterator first, ForwardIterator last, Compare comp)")
  (std::max_element "ForwardIterator std::max_element(ForwardIterator first, ForwardIterator last)
ForwardIterator std::max_element(ForwardIterator first, ForwardIterator last, Compare comp)")
  (std::lexicographical_compare "bool std::lexicographical_compare(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2)
bool std::lexicographical_compare(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, Compare comp)")
  (std::next_permutation "bool std::next_permutation(BidirectionalIterator first, BidirectionalIterator last)
bool std::next_permutation(BidirectionalIterator first, BidirectionalIterator last, Compare comp)")
  (std::prev_permutation "bool std::prev_permutation(BidirectionalIterator first, BidirectionalIterator last)
bool std::prev_permutation(BidirectionalIterator first, BidirectionalIterator last, Compare comp)"))

(define-inclue-header (c++03 "deque")
  "The <deque> header."
  (std::deque))

(define-inclue-header (c++03 "list")
  "The <list> header."
  (std::list))

(define-inclue-header (c++03 "queue")
  "The <queue> header."
  (std::queue))

(define-inclue-header (c++03 "stack")
  "The <stack> header."
  (std::stack))

(define-inclue-header (c++03 "vector")
  "The <vector> header."
  (std::vector))

(define-inclue-header (c++03 "map")
  "The <map> header."
  (std::map)
  (std::multimap))

(define-inclue-header (c++03 "set")
  "The <set> header."
  (std::set)
  (std::multiset))

(define-inclue-header (c++03 "bitset")
  "The <bitset> header."
  (std::bitset))

(define-inclue-header (c++03 "iterator")
  "The <iterator> header."
  (std::iterator_traits)
  (std::iterator)
  (std::input_iterator_tag)
  (std::output_iterator_tag)
  (std::forward_iterator_tag)
  (std::bidirectional_iterator_tag)
  (std::random_access_iterator_tag)
  (std::advance "void std::advance(InputIterator& i, Distance n)")
  (std::distance "std::iterator_traits<InputIterator>::difference_type distance(InputIterator first, InputIterator last)")
  (std::back_insert_iterator)
  (std::back_inserter "std::back_insert_iterator<Container> std::back_inserter(Container& x)")
  (std::front_insert_iterator)
  (std::front_inserter "std::front_insert_iterator<Container> std::front_inserter(Container& x)")
  (std::insert_iterator)
  (std::inserter "std::insert_iterator<Container> std::inserter(Container& x)")
  (std::istream_iterator)
  (std::istreambuf_iterator)
  (std::ostream_iterator)
  (std::ostreambuf_iterator))

(define-inclue-header (c++03 "iostream")
  "27.3. Standard iostream objects [lib.iostream.objects]."
  (std::cin)
  (std::cout)
  (std::cerr)
  (std::clog)
  (std::wcin)
  (std::wcout)
  (std::wcerr)
  (std::wclog))

(define-inclue-header (c++03 "ios")
  "27.4. Iostream base classes [lib.iostreams.base]."
  (std::streamoff)
  (std::streamsize)
  (std::fpos)
  (std::ios_base)
  (std::boolalpha "std::ios_base& std::boolalpha(std::ios_base& str)")
  (std::noboolalpha "std::ios_base& std::noboolalpha(std::ios_base& str)")
  (std::showbase "std::ios_base& std::showbase(std::ios_base& str)")
  (std::noshowbase "std::ios_base& std::noshowbase(std::ios_base& str)")
  (std::showpoint "std::ios_base& std::showpoint(std::ios_base& str)")
  (std::noshowpoint "std::ios_base& std::noshowpoint(std::ios_base& str)")
  (std::showpos "std::ios_base& std::showpos(std::ios_base& str)")
  (std::noshowpos "std::ios_base& std::noshowpos(std::ios_base& str)")
  (std::skipws "std::ios_base& std::skipws(std::ios_base& str)")
  (std::noskipws "std::ios_base& std::noskipws(std::ios_base& str)")
  (std::uppercase "std::ios_base& std::uppercase(std::ios_base& str)")
  (std::nouppercase "std::ios_base& std::nouppercase(std::ios_base& str)")
  (std::unitbuf "std::ios_base& std::unitbuf(std::ios_base& str)")
  (std::nounitbuf "std::ios_base& std::nounitbuf(std::ios_base& str)")
  (std::internal "std::ios_base& std::internal(std::ios_base& str)")
  (std::left "std::ios_base& std::left(std::ios_base& str)")
  (std::right "std::ios_base& std::right(std::ios_base& str)")
  (std::dec "std::ios_base& std::dec(std::ios_base& str)")
  (std::hex "std::ios_base& std::hex(std::ios_base& str)")
  (std::oct "std::ios_base& std::oct(std::ios_base& str)")
  (std::fixed "std::ios_base& std::fixed(std::ios_base& str)")
  (std::scientific "std::ios_base& std::scientific(std::ios_base& str)")
  ;; the following are a bit hairy -- generally we haven't tried to
  ;; describe class members, because we lack the syntactic grunt required
  ;; to handle them -- in this case we could almost get away with something
  ;; as primivite as the following keywords, except that these members
  ;; are sometimes used via the names of derived classes... gah
  (std::ios_base::fmtflags)
  (std::ios_base::boolalpha)
  (std::ios_base::dec)
  (std::ios_base::fixed)
  (std::ios_base::hex)
  (std::ios_base::internal)
  (std::ios_base::left)
  (std::ios_base::oct)
  (std::ios_base::right)
  (std::ios_base::scientific)
  (std::ios_base::showbase)
  (std::ios_base::showpoint)
  (std::ios_base::showpos)
  (std::ios_base::skipws)
  (std::ios_base::unitbuf)
  (std::ios_base::uppercase)
  (std::ios_base::adjustfield)
  (std::ios_base::basefield)
  (std::ios_base::floatfield)
  (std::ios_base::iostate)
  (std::ios_base::badbit)
  (std::ios_base::eofbit)
  (std::ios_base::failbit)
  (std::ios_base::goodbit)
  (std::ios_base::openmode)
  (std::ios_base::app)
  (std::ios_base::ate)
  (std::ios_base::binary)
  (std::ios_base::in)
  (std::ios_base::out)
  (std::ios_base::trunc)
  (std::ios_base::seekdir)
  (std::ios_base::beg)
  (std::ios_base::cur)
  (std::ios_base::end)
  (std::ios_base::failure))

(provide 'inclue-c++03)

;;; inclue-c++03.el ends here
