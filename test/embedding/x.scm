(import sudoku)
(import (only chicken.platform return-to-host))

(define-foreign-type scell_fun (function void (int int int nonnull-c-pointer)))

(define-external
  (sudoku_map_result (scheme-object result)
                     (scell_fun scfun)
                     (c-pointer user-data)) void
                     (let ((cfun (foreign-lambda*
                                     void ((scell_fun scfun)
                                           (int r) (int c) (int v)
                                           (c-pointer userdata))
                                   "scfun(r, c, v, userdata);")))
                       (sudoku-scand-map
                        (lambda (row col val)
                          ;; (print "Solved " row ", " col ", " val)
                          (cfun scfun row col val user-data))
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
