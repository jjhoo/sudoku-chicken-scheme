(use sudoku)
(import foreign)

(define-foreign-type scell_fun (function void (int int int nonnull-c-pointer)))

(define-external
  (sudoku_test (scheme-object result)
               (scell_fun scfun)
               (c-pointer user-data)) void
  (let ((asdf (foreign-lambda* void ((int r) (int c) (int v)
                                     (scell_fun scfun)
                                     (c-pointer userdata))
                "scfun(r, c, v, userdata);")))
    (sudoku-scand-map
     (lambda (row col val)
       ;; (print "Solved " row ", " col ", " val)
       (asdf row col val scfun user-data))
     (car result))))

(define-external (sudoku_solve (c-string sudoku)) scheme-object
  (let* ((solved (sudoku-string->grid sudoku))
         (cands (sudoku-init solved))
         (res (sudoku-solve solved cands)))
    (print "Solved "
           (sudoku-scand-map
            (lambda (row col val)
              ;; (print "Solved " row ", " col ", " val)
              (list row col val)) (car res)))
    (print "Left "
           (sudoku-scand-map
            (lambda (row col val)
              ;; (print " cand " row ", " col ", " val)
              (list row col val)) (cdr res)))
    res))

(return-to-host)
