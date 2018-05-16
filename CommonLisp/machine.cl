;;;-----------------------------------------------------------------------------
;;; 0. Syntax
;;;-----------------------------------------------------------------------------

;;; General form

;;; CL has two fundamental pieces of syntax: ATOM and S-EXPRESSION.
;;; Typically, grouped S-expressions are called `forms`.

(print 10)            ; an atom; it evaluates to itself
(print :thing)        ; another atom; evaluating to the symbol :thing
(print t)             ; another atom, denoting true
(print (+ 1 2 3 4))   ; an s-expression
(print '(4 :foo t))   ; another s-expression


;;; Comments

;;; Single-line comments start with a semicolon; use four for file-level
;;; comments, three for section descriptions, two inside definitions, and one
;;; for single lines. For example,

;;;; life.lisp

;;; Foo bar baz, because quu quux. Optimized for maximum krakaboom and umph.
;;; Needed by the function LINULUKO.

(defun meaning (life)
  "Return the computed meaning of LIFE"
  (print life)
  (let ((meh "abc"))
    ;; Invoke krakaboom
    (loop :for x :across meh
       :collect x)))                    ; store values into x, then return it

;;; Block comments, on the other hand, allow for free-form comments. They are
;;; delimited with #| and |#

#| This is a block comment which
   can span multiple lines and
    #|
       they can be nested!
    |#
|#


;;; Environment

;;; A variety of implementations exist; most are standards-conformant. SBCL
;;; is a good starting point. Third party libraries can be easily installed with
;;; Quicklisp

;;; CL is usually developed with a text editor and a Real Eval Print
;;; Loop (REPL) running at the same time. The REPL allows for interactive
;;; exploration of the program while it is running "live".


;;;-----------------------------------------------------------------------------
;;; 1. Primitive datatypes and operators
;;;-----------------------------------------------------------------------------

;;; Symbols

'foo ; => FOO  Notice that the symbol is upper-cased automatically.

;;; INTERN manually creates a symbol from a string.

(intern "AAAA")        ; => AAAA
(intern "aaa")         ; => |aaa|

;;; Numbers

9999999999999999999999 ; integers
#b111                  ; binary => 7
#o111                  ; octal => 73
#x111                  ; hexadecimal => 273
3.14159s0              ; single
3.14159d0              ; double
1/2                    ; ratios
#C(1 2)                ; complex numbers

;;; Function application are written as (f x y z ...) where f is a function and
;;; x, y, z, ... are the arguments.

(+ 1 2)                ; => 3

;;; If you want to create literal data, use QUOTE to prevent it from being
;;; evaluated

(quote (+ 1 2))        ; => (+ 1 2)
(quote a)              ; => A

;;; The shorthand for QUOTE is '

'(+ 1 2)               ; => (+ 1 2)
'a                     ; => A

;;; Basic arithmetic operations

(+ 1 1)                ; => 2
(- 8 1)                ; => 7
(* 10 2)               ; => 20
(expt 2 3)             ; => 8
(mod 5 2)              ; => 1
(/ 35 5)               ; => 7
(/ 1 3)                ; => 1/3
(+ #C(1 2) #C(6 -4))   ; => #C(7 -2)

;;; Booleans

t                      ; true; any non-NIL value is true
nil                    ; false; also, the empty list: ()
(not nil)              ; => T
(and 0 t)              ; => T
(or 0 nil)             ; => 0
