(deftest test-mean ()
  (check (equal (mean '(1 2 3 4)) 5/2))
  (check (equal (mean '(1)) 1))
  (check (equal (mean '(4 3 2 1)) 5/2))
  (check (errors-p (mean nil))))

(deftest test-median ()
  (check (equal (median '(1 2 3 4 5)) 3))
  (check (equal (median '(1 2 3 4)) 5/2))
  (check (equal (median '(1)) 1))
  (check (equal (median '(5 3 2 4 1)) 3))
  (check (not (errors-p (median nil))))
  (let ((lst '(5 4 3 2 1)))
    (median lst)
    ;if median is destructive, it may modify lst, but leave it in a state where (length lst) returns the 
    ;correct length, but if you print the lst, it isn't the entire list. This at least happens in SBCL
    ;so a workaround is to copy lst, and then check if the length of that list is equal to what is expected
    (check (equal (length (copy-list lst)) 5))))

(defun test-letf ()
  (test-mean)
  (test-median))