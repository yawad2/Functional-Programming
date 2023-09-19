
(defun .length (L)
   (if (null L) ;base case
      0
      (+ 1 (.length(cdr L)))
   )
)

(defun .append (L1 L2)
    (if (null L1)
        L2
        (cons (car L1) (.append (cdr L1) L2))
    )
)

(defun .foldr (L F Z)
  (if (null L)
      Z
      (funcall F (car L) (.foldr (cdr L) F Z))
  )
)

;;;function that returns a list except the last N elements
(defun .butlast (L N)
  (cond ((or (null L) (= N 0)) L)
        ((< N 0) nil)
        (t (.butlast (cdr-but-last L) (- N 1)))
  )
)

;;;helper function that takes a list, deletes its last element, and returns the resulting list
(defun cdr-but-last (lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) nil)
        (t (cons (car lst) (cdr-but-last (cdr lst))))
  )
)

(defun .contains (S X)
  (cond ((null S) nil)
        ((equalp (car S) X) t)
        (t (.contains (cdr S) X))
  )
)

;;; helper function that removes a given element from a given list
(defun .remove (element list)
  (cond ((null list) nil)
        ((equalp element (car list)) (cdr list))
        (t (cons (car list) (.remove element (cdr list))))
  )
)

(defun .set-equal (S1 S2)
  (cond ((and (null S1) (null S2)) t)
        ((or (null S1) (null S2)) nil)
        ((.contains S1 (car S2)) (.set-equal (cdr S2) (.remove (car S2) S1)))
        (t nil)
  )
)

(defun .difference (S1 S2)
  (cond ((null S1) nil)
        ((.contains S2 (car S1)) (.difference (cdr S1) S2))
        (t (cons (car S1) (.difference (cdr S1) S2)))
  )
)

(defun .subseteq (S1 S2)
   (cond ((null S1) t)
         ((not (.contains S2 (car S1))) nil)
         (t (.subseteq (cdr S1) S2))
   )
)

(defun .abs (N)
   (if (> N 0)
      N
      (* N -1)
   )
)

(defun .right-tri (A B C)
   (if (equalp (* C C) (+ (* A A) (* B B)))
     t
     nil
   )
)

(defun .mod (X Y)
  (cond ((< X Y) X)
        (t (.mod (- X Y) Y))
  )
)

(defun .nth-fibo (N)
   (cond ((equalp N 0) 0)
         ((equalp N 1) 1)
         (t (+ (.nth-fibo (- N 1)) (.nth-fibo (- N 2))))
   )
)

(defun test (f args)
  "Evaluate the function whose name is the symbol F on list of arguments
ARGS and print an informative message."
  (format t "~S => ~S~%" (cons f args) (apply f args)))

(defun repl (f)
  "Run the function whose name is the symbol F in a Read-Eval-Print Loop."
  (format t "Testing ~A~%" (symbol-name f))
  (repl1 f))

(defun repl1 (f)
  (format t "Enter arguments for ~A (enter a list, or ! to stop): " (symbol-name f))
  (finish-output t)
  (repl2 f (read)))

(defun repl2 (f args)
  (cond
    ((eq args '!)
     t)
    (t
     (test f args)
     (repl1 f))))


(repl 'g)
(defun g (e x)
(cond
(x 
(cons (car x) (g e (cdr x))))
(t
(list e))))
