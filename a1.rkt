#lang racket
(require "a1-student-tests.rkt")
 (print-as-expression #f) 
#|
1. Define and test a procedure countdown that takes a natural number and returns a list of the natural numbers less than or equal to that number, in descending order.
> (countdown 5)
(5 4 3 2 1 0)

定义一个过程，要求
输入：自然数
返回：小于或等于这个自然数的一个列表。
|#

(define (countdown i )
  (cond
   ((zero? i) `(0))                                    ; if i equal 0, just return an list with element 0.
   ((> i 0) (cons i (countdown (- i 1))))              ; if i great than 0, cons i to the recursively applied of  the (countdown (- i 1))
   (else (exit "arg must great or equal than 0"))))    ; argument is illegal




#|
2. Define and test a procedure insertR that takes two symbols and a list and returns a new list with the second symbol inserted after each occurrence of the first symbol. For this and later questions, these functions need only hold over eqv?-comparable structures.
> (insertR 'x 'y '(x z z x y x))
(x y z z x y y x y)

定义一个过程,要求
输入参数1: symbol a
输入参数2: symbol b
输入参数3: list of symbol
返回: 返回一个列表，其中出现symbol a的地方后面加一个symbolb
|#


(define (insertR x y l)
  (cond
    ((null? l) `())                                               ; if list is null, just return null list
    ((eqv? (car l) x) (cons x (cons y (insertR x y (cdr l)))))     ; if the car of list is equal to x , then cons x to y to the recursively applied of  the insertR.
    (else (cons (car l) (insertR x y (cdr l))))))                 ; if the car of list is not equal to x, then cons x to the recursively applied of  the insertR.

  
#|
3. Define and test a procedure remv-1st that takes a a symbol and a list and returns a new list with the first occurrence of the symbol removed.
> (remv-1st 'x '(x y z x))
(y z x)
> (remv-1st 'y '(x y z y x))
(x z y x)
定义一个过程,要求
输入参数1: symbol i
输入参数2: list l
返回: 删除列表中第一次出现symbol i 后的列表。
|#

(define (remv-1st i l)
  (cond
    ((null? l) `())                                        ; if list is null, just return null list
    ((eqv? (car l) i) (cdr l))                              ; if the car of list is equal to i , then return cdr l
    (else (cons (car l) (remv-1st i (cdr l))))))           ; if (car l) not equal to i , then cons (car l) to the recursively applied of  the remv-1st. 


#|
4. Define and test a procedure list-index-ofv? that takes an element and a list and returns the (base 0) index of that element in the list. A list missing that element will be considered bad data.
> (list-index-ofv? 'x '(x y z x x))
0
> (list-index-ofv? 'x '(y z x x))
2

定义一个过程,要求
输入参数1: symbol i
输入参数2: list l
返回: i在l中的索引(第一个元素索引为0)。
|#

(define (list-index-ofv? i l)
  (cond
    ((null? l) (exit "bad data!"))                        ; if not found i in list l, the input is bad data
    ((eqv? i (car l)) 0)                                   ; if i and car l is equal, then return 0
    (else (+ 1 (list-index-ofv? i (cdr l))))              ; else return + 1 to recursively apply the list-index-ofv?.
    ))
 


#|
5. Define and test a procedure filter that takes a predicate and a list and returns a new list containing the elements that satisfy the predicate. A predicate is a procedure that takes a single argument and returns either #t or #f. The number? predicate, for example, returns #t if its argument is a number and #f otherwise. The argument satisfies the predicate, then, if the predicate returns #t for that argument.
> (filter even? '(1 2 3 4 5 6))
(2 4 6)
定义一个过程,要求
输入参数1: 过滤函数 fn
输入参数2: list l
返回:  返回一个列表， 对列表的每个元素i都有 (fn t) 为 true。
|#


(define ( filter fn l)
  (cond
    ((null? l) `())                                               ; if l is empty, return an empty list
    ((eqv? (fn (car l)) #t) (cons (car l) ( filter fn (cdr l))))    ; if the car l satisfy fn , then cons (car l) to the recursively applied of remv-1st. 
    (else ( filter fn (cdr l)))))



#|
6. Define and test a procedure zip that takes two lists and forms a new list, each element of which is a pair formed by combining the corresponding elements of the two input lists. If the two lists are of uneven length, then drop the tail of the longer one.
> (zip '(1 2 3) '(a b c))
((1 . a) (2 . b) (3 . c))
> (zip '(1 2 3 4 5 6) '(a b c))
((1 . a) (2 . b) (3 . c))
> (zip '(1 2 3) '(a b c d e f))
((1 . a) (2 . b) (3 . c))

定义一个过程,要求
输入参数1: list l
输入参数2: list r
返回:  返回l和r zip后的列表。
|#

(define (zip l r)
  (cond
    ((or (null? l) (null? r)) `())             ; if l or r is null , then return '(). drop then tail of the longer one 
    (else (cons                                ; else cons follow two expressions
           (cons (car l) (car r)) 
           (zip (cdr l) (cdr r))))))

#|
7. Define and test a procedure map that takes a procedure p of one argument and a list ls and returns a new list containing the results of applying p to the elements of ls. Do not use Racket's built-in map in your definition.
> (map add1 '(1 2 3 4))
(2 3 4 5)

定义一个过程,要求
输入参数1: 函数 fn
输入参数2: list l
返回:  返回一个对列表l中每个元素apply fun后的列表。
|#

(define ( map fn l)
  (cond
    ((null? l) `())                                         ; if l or r is null , then return '().
    (else (cons (fn (car l)) ( map fn (cdr l))))))         ; else apply the first element of l to fn and cons it to the recursively applied of  map.


#|
8. Define and test a procedure append that takes two lists, ls1 and ls2, and appends ls1 to ls2.
> (append '(a b c) '(1 2 3))
(a b c 1 2 3)


定义一个过程,要求
输入参数1: list l
输入参数2: list r
返回:  合并 l r 后的列表。
|#

(define ( append l r)
  (cond
    ((null? l) r)                             ; if l is null then just return r;
    
    (else (cons (car l)                       ; else cons first element of l to the recursively applied of  append
                ( append (cdr l) r)))))



#|
9. Define and test a procedure reverse that takes a list and returns the reverse of that list.
> (reverse '(a 3 x))
(x 3 a)
定义一个过程,要求
输入参数1: list l
返回:  reverse l 后的列表。
|#
(define ( reverse l)
  (cond
    ((null? l) `())                                       ; if l is null just return empty list
    (else                                                 ; else 
     (append ( reverse (cdr l)) (cons (car l) `())))))  ; the append will slow down the execution dramatically





#|
10. Define and test a procedure fact that takes a natural number and computes the factorial of that number. The factorial of a number is computed by multiplying it by the factorial of its predecessor. The factorial of 0 is defined to be 1.
> (fact 0)
1
> (fact 5)
120
定义一个过程,要求
输入参数1: 自然数 x
返回:  x的阶乘。
|#

(define (fact x)
  (cond
    ((= x 0) 1)
    ((< x 0) (exit "ilegal input in fact"))
    (else
     (* x (fact (- x 1))))))

#|
11. Define and test a procedure memv that takes an element and a list and returns the first cdr whose car is eqv? to the element, or #f if the element is absent from the list.
> (memv 'a '(a b c))
(a b c)
> (memv 'b '(a ? c))
#f
> (memv 'b '(a b c b))
(b c b)

定义一个过程,要求
输入参数1: 元素x
输入参数2: list l
返回:  返回l的子list， 这个子list的car 等于 x。
|#
(define (memv x l)
  (cond
    ((null? l) #f)
    ((eqv? x (car l)) l)
    (else (memv x (cdr l)))
  ))

#|
12. Define and test a procedure fib that takes a natural number n as input and computes the nth number, starting from zero, in the Fibonacci sequence (0, 1, 1, 2, 3, 5, 8, 13, 21, ...). Each number in the sequence is computed by adding the two previous numbers.
> (fib 0)
0
> (fib 1)
1
> (fib 7)
13
定义一个过程,要求
输入参数1: 自然数 x
返回:  x的斐波那契的值。
|#

(define (fib x)
  (cond
    ((< x 0) (exit "ilegal input!"))
    ((= x 0) 0)
    ((= x 1) 1)
    (else (+ (fib (- x 1)) (fib (- x 2))))))

#|
13. The expressions (a b) and (a . (b . ())) are equivalent. Using this knowledge, rewrite the expression ((w x) y (z)) using as many dots as possible. Be sure to test your solution using Racket's equal? predicate. (You do not have to define a rewrite procedure; just rewrite the given expression by hand and place it in a comment.)

已知 (a b) 和 (a . (b . ())) 等价， 写出  ((w x) y (z)) 的等价形式。 要求使用尽可能的点(.)

|#
; ((w . (x)) . (y . ((z))))
; (equal? `((w x) y (z)) `((w . (x)) . (y . ((z)))))

; (test-file #:file-name "a1.rkt")



#|
14. Define and test a procedure binary->natural that takes a flat list of 0s and 1s representing an unsigned binary number in reverse bit order and returns that number. For example:
> (binary->natural '())
0
> (binary->natural '(0 0 1))
4
> (binary->natural '(0 0 1 1))
12
> (binary->natural '(1 1 1 1))
15
> (binary->natural '(1 0 1 0 1))
21
> (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))
8191
定义一个过程,要求
输入参数1: 一个列表， 元素为0或1，
输出： 元素1 * 2^0 + 元素2 * 2^1  + 元素3 * 2^2  + 元素4 * 2^3  + 元素5 * 2^4 .... 等 
|#
(define (binary->natural l)
  (cond
    ((null? l) 0)
    ((null? (cdr l )) (car l))
    ((= (car l) 0) (*  2 (binary->natural (cdr l))))
    ((= (car l) 1) (+ (*  2 (binary->natural (cdr l))) 1) )
    ))


#|

15. Define subtraction using natural recursion. Your subtraction function, minus, need only take nonnegative inputs where the result will be nonnegative.
> (minus 5 3)
2
> (minus 100 50)
50
定义一个过程,要求
输入参数1: 非负数 x y
输出: x - y的值   使用递归实现
|#


(define (minus x y)
  (cond
    ((< x 0) #f)
    ((< y 0) #f)
    ((= y 0) x)
    ((> y 0) (minus (sub1 x) (sub1 y)))))


#|
16. Define division using natural recursion. Your division function, div, need only work when the second number evenly divides the first. Division by zero is of course bad data.
> (div 25 5)
5
> (div 36 6)
6

定义一个过程,要求 
输入参数1: 非负数 x y
输出: x - y的值   使用递归实现
|#

(define (div x y)
  (cond
    ((< (* x y) 0)  (exit "only work when the second number evenly divides the first and x y must positive. "))
    ((= y 0)        (exit "only work when the second number evenly divides the first and x y must positive. ") )
    ((= x 0 )       0)
    (else           (+ 1 (div (- x y) y)))))
    


#|
17. Define a function append-map that, similar to map, takes both a procedure p of one argument a list of inputs ls and applies p to each of the elements of ls. Here, though, we mandate that the result of p on each element of ls is a list, and we append together the intermediate results. Do not use Racket's built-in append-map in your definition.
> (append-map countdown (countdown 5))
(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0)

定义一个过程,要求 
输入参数1: 函数f
输入参数2: 列表l。
输出:  函数f, 对某个列表(第二个参数)中的每一项应用后都会产生一个列表。合并这些列表。
|#

(define (append-map f l)
  (cond
    ((null? l) `())
    (else      (append (f (car l)) (append-map f (cdr l))))))









