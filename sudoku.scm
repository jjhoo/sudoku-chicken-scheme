;; Copyright (c) 2018 Jani J. Hakala <jjhakala@gmail.com> Jyväskylä, Finland
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as
;;  published by the Free Software Foundation, version 3 of the
;;  License.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.
;;
;;  You should have received a copy of the GNU Affero General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(module sudoku
    (sudoku-init
     sudoku-solve
     sudoku-scand-fold
     sudoku-scand-map
     sudoku-string->grid)

  (import scheme)
  (cond-expand
    (chicken-4
     (import chicken)
     (import (only extras fprintf))
     (import (only data-structures flatten merge sort))
     (require-library combinatorics)
     (require-library matchable)
     (require-library srfi-121))
    (else
     (import chicken.base
             chicken.format
             (only chicken.memory.representation
                   record-instance-slot record-instance?)
             (only chicken.sort merge sort))))
  (import (only srfi-1
                append-map any every filter find first fold iota
                lset-difference lset-intersection lset-union lset-xor
                partition remove second third))
  (import (only combinatorics
                combination-fold permutation-fold unordered-subset-map))
  (import (only matchable match))
  (import (only srfi-121
                gdelete-neighbor-dups generator-fold generator->list
                gfilter list->generator make-coroutine-generator
                make-iota-generator))

(define-record box row col)
(define-record pos row col box)
(define-record scand pos value)
(define-record scell pos values)
(define-record find-result solved eliminated)

(define-constant SUDOKU-BOXES 3)
(define-constant SUDOKU-NUMBERS 9)

(define-record-printer (box value out)
  (fprintf out "(box ~s ~s)"
           (box-row value) (box-col value)))

(define-record-printer (pos value out)
  (fprintf out "(pos ~s ~s ~s)"
           (pos-row value) (pos-col value) (pos-box value)))

(define-record-printer (scand value out)
  (fprintf out "(scand ~s ~s)"
           (scand-pos value) (scand-value value)))

(define-record-printer (scell value out)
  (fprintf out "(scell ~s ~s)"
           (scell-pos value) (scell-values value)))

(define (make-2d-generator yfun gen1 gen2-init)
  (make-coroutine-generator
   (lambda (yield)
     (let loop ()
       (let ((i (gen1)))
         (if (eof-object? i)
             (yield i)
             (let loop2 ((gen2 (gen2-init i)))
               (let ((j (gen2)))
                 (if (eof-object? j)
                     (loop)
                     (begin
                       (yield (yfun i j))
                       (loop2 gen2)))))))))))

(define (box-number-to-box num)
  (let ((q (quotient (sub1 num) SUDOKU-BOXES))
        (r (remainder (sub1 num) SUDOKU-BOXES)))
    (make-box (add1 q) (add1 r))))

(define (num-to-box-number num)
  (add1 (quotient (sub1 num) SUDOKU-BOXES)))

(define (cell-box-calc row col)
  (make-box (num-to-box-number row) (num-to-box-number col)))

(define (scand-init row col n)
  (make-scand (make-pos row col (cell-box-calc row col)) n))

(define (sudoku-string->grid grid)
  (let loop ((i 1) (j 1) (solved '()) (tmp (string->list grid)))
    (if (null? tmp)
        (reverse solved)
        (let* ((val (string->number (string (car tmp))))
               (nsolved (if (= val 0)
                            solved
                            (cons (scand-init i j val) solved)))
               (ni (if (= j SUDOKU-NUMBERS) (+ i 1) i))
               (nj (if (= j SUDOKU-NUMBERS) 1 (+ j 1))))
          (loop ni nj nsolved (cdr tmp))))))

(define (grid-init)
  (let ((g (make-coroutine-generator
            (lambda (yield)
              (let loop ((i 1) (j 1) (n 1))
                (when (not (and (= i SUDOKU-NUMBERS) (= j SUDOKU-NUMBERS) (= n (add1 SUDOKU-NUMBERS))))
                      (cond ((> n SUDOKU-NUMBERS) (loop i (+ j 1) 1))
                            ((> j SUDOKU-NUMBERS) (loop (+ i 1) 1 1))
                            (else (begin
                                    (yield (scand-init i j n))
                                    (loop i j (+ n 1)))))))))))
    (generator->list g)))

(define (pos-same? pos1 pos2)
  (equal? pos1 pos2))

(define (pos-same-box? pos1 pos2)
  (equal? (pos-box pos1) (pos-box pos2)))

(define (pos-same-col? pos1 pos2)
  (= (pos-col pos1) (pos-col pos2)))

(define (pos-same-row? pos1 pos2)
  (= (pos-row pos1) (pos-row pos2)))

(define (pos-sees? pos1 pos2)
  (or (pos-same-box? pos1 pos2)
      (pos-same-col? pos1 pos2)
      (pos-same-row? pos1 pos2)))

(define (scand-filter-value n cands)
  (filter (lambda (cell) (= n (scand-value cell))) cands))

(define (scand-get-box box cands)
  (let ((box (if (integer? box)
                 (box-number-to-box box)
                  box)))
    (filter (lambda (cell) (equal? box (pos-box (scand-pos cell)))) cands)))

(define (scand-get-cell pos cands)
  (filter (lambda (cell) (equal? pos (scand-pos cell))) cands))

(define (scand-get-col col cands)
  (filter (lambda (cell) (= col (pos-col (scand-pos cell)))) cands))

(define (scand-get-row row cands)
  (filter (lambda (cell) (= row (pos-row (scand-pos cell)))) cands))

(define (unique-positions-gen cands)
  ;; (unique (map (lambda (cell) (first cell)) cands)))
  (gdelete-neighbor-dups
   (list->generator (map scand-pos cands))))

(define (numbers cands)
  (sort (map scand-value cands) <))

(define (delete-neighbour-dups = numbers)
  (let ((nil (gensym)))
    (let loop ((xs numbers) (res '()) (current nil))
      (if (null? xs)
          (reverse res)
          (match
           xs
           [(H . T) (cond ((eq? nil current) (loop T (list H) H))
                          ((= H current) (loop T res H))
                          (else (loop T (cons H res) H)))])))))

(define (number-counts numbers #!optional (limit 'nil))
  (let* ((unums (delete-neighbour-dups = numbers))
         (counts (map (lambda (n)
                        (cons n (length (filter (lambda (x) (= x n))
                                                numbers))))
                      unums)))
    (if (eq? limit 'nil)
        counts
        (filter (lambda (item) (= limit (cdr item))) counts))))

(define (number-counts-cond pred numbers)
  (filter (lambda (item) (pred (cdr item))) (number-counts numbers)))

(define (scand-unique-numbers cands)
  (delete-neighbour-dups = (numbers cands)))

(define (unique-numbers-gen cands)
  (gdelete-neighbor-dups
   (list->generator (numbers cands))))

(define (pos-less? pos1 pos2)
  (cond ((< (pos-row pos1) (pos-row pos2)) #t)
        ((= (pos-row pos1) (pos-row pos2))
         (cond ((< (pos-col pos1) (pos-col pos2)) #t)
               (else #f)))
        (else #f)))

(define (scand-less? cell1 cell2)
  (cond ((< (pos-row (scand-pos cell1)) (pos-row (scand-pos cell2))) #t)
        ((= (pos-row (scand-pos cell1)) (pos-row (scand-pos cell2)))
         (cond ((< (pos-col (scand-pos cell1)) (pos-col (scand-pos cell2))) #t)
               ((= (pos-col (scand-pos cell1)) (pos-col (scand-pos cell2)))
                (< (scand-value cell1) (scand-value cell2)))
               (else #f)))
        (else #f)))

(define (cands-sort+dedupe cands)
  (delete-neighbour-dups equal? (sort cands scand-less?)))

(define (scand-same-pos? cell1 cell2)
  (equal? (scand-pos cell1) (scand-pos cell2)))

(define (scand-same-val? cell1 cell2)
  (= (scand-value cell1) (scand-value cell2)))

(define (scand-cell-sees? cell1 cell2)
  (pos-sees? (scand-pos cell1) (scand-pos cell2)))

(define (scand-cell-row cell)
  (pos-row (scand-pos cell)))

(define (scand-cell-col cell)
  (pos-col (scand-pos cell)))

(define (cells-in-col? xs)
  (match xs
    [(x . '()) #f]
    [(x0 . rest)
     (every (lambda (cell)
              (= (scand-cell-col x0)
                 (scand-cell-col cell))) rest)]))

(define (cells-in-row? xs)
  (match xs
    [(x . '()) #f]
    [(x0 . rest)
     (every (lambda (cell)
              (= (scand-cell-row x0)
                 (scand-cell-row cell))) rest)]))

(define (cells-in-line? xs)
  (or (cells-in-row? xs) (cells-in-col? xs)))

(define (sudoku-init solved)
  (let loop ((xs solved) (cands (grid-init)))
    (if (null? xs)
        cands
        (let* ((cell1 (car xs))
               (rest (cdr xs))
               (pos1 (scand-pos cell1))
               (pred (lambda (cell2)
                       (let ((pos2 (scand-pos cell2)))
                         (or (pos-same? pos1 pos2) ;; cell is solved
                             (and (pos-sees? pos1 pos2)
                                  (scand-same-val? cell1 cell2))))))
               (ncands (remove pred cands)))
          (loop rest ncands)))))

(define (finder pred cands)
  (let* ((yfun (lambda (i fun) (cons i fun)))
         (gen (make-2d-generator
               yfun
               (make-iota-generator SUDOKU-NUMBERS 1)
               (lambda (i)
                 (list->generator (list scand-get-row
                                        scand-get-col
                                        scand-get-box))))))
    (let loop ((solved '()) (eliminated '()))
      (match
       (gen)
       [#!eof (make-find-result (cands-sort+dedupe solved)
                                (cands-sort+dedupe eliminated))]
       [(i . f)
        (begin
          ;; (print "Now i = " i ", fun is " f)
          (let ((res (pred (f i cands))))
            (loop (append solved (find-result-solved res))
                  (append eliminated (find-result-eliminated res)))))]))))

;; Find unsolved cells that have only one candidate left
(define (find-singles-simple solved cands)
  (print "Hidden singles (simple)")
  (let ((fun (lambda (cands)
               (make-find-result
                (generator-fold
                 (lambda (pos res)
                   (match
                       (generator->list
                        (gfilter (lambda (cell)
                                   (pos-same? pos (scand-pos cell)))
                                 (list->generator cands)))
                     [() res]
                     [(cell) (cons cell res)]
                     [(cell . rest) res]))
                 '()
                 (unique-positions-gen cands))
                '()))))
    (finder fun cands)))

(define (find-singles solved cands)
  (print "Hidden singles")
  (let ((fun (lambda (cands)
               (make-find-result
                (generator-fold
                 (lambda (n res)
                   (match
                       (generator->list
                        (gfilter (lambda (cell) (= n (scand-value cell)))
                                 (list->generator cands)))
                     [() res]
                     [(cell) (cons cell res)]
                     [(cell . rest) res]))
                 '()
                 (unique-numbers-gen cands))
                '()))))
    (finder fun cands)))

(define (list-less? < =)
  (lambda (alist blist)
    (let loop ((as alist) (bs blist))
      (cond ((and (null? as) (null? bs)) #f)
            ((or (null? as) (null? bs)) #f)
            ((< (car as) (car bs)) #t)
            ((= (car as) (car bs)) (loop (cdr as) (cdr bs)))
            (else #f)))))

(define (cell-numbers-for-pos pos cands)
  (map scand-value (scand-get-cell pos cands)))

(define (find-cells-cond pred cands)
  (let* ((poss (generator->list (unique-positions-gen cands)))
         (pos-nums (map (lambda (pos)
                          (cons pos (cell-numbers-for-pos pos cands)))
                        poss)))
    (filter (lambda (pos-num-p) (pred (cdr pos-num-p))) pos-nums)))

(define (find-cells-with-n-candidates limit cands)
  (find-cells-cond (lambda (numbers) (= limit (length numbers))) cands))

(define (find-naked-groups-in-set limit cands)
  (if (< (length (generator->list (unique-positions-gen cands))) (+ limit 1))
      ;; (print "Early return " (generator->list (unique-positions-gen cands)))
      (make-find-result '() '())
      (begin
        (let* ((nums (numbers cands))
               (ncounts (number-counts nums))
               (unums (map first ncounts))
               (kperms (sort (unordered-subset-map
                              (lambda (c) (sort c <))
                              unums limit)
                             (list-less? < equal?))))
          ;; now find if 'limit' count of cells contain a naked group
          ;; of 'limit' count of numbers
          ;; (print "Combos (" limit ") >> " kperms " << for cands >> " cands)
          (let loop ((permgen (list->generator kperms)) (found '()))
            (match
             (permgen)
             [#!eof (make-find-result '() found)]
             [perm
              (let* ((foos (find-cells-cond
                            (lambda (numbers)
                              (or (equal? perm numbers)
                                  (null? (lset-difference equal? numbers perm))))
                            cands)))
                (if (= (length foos) limit)
                    (let ((positions (map car foos)))
                      ;; (print "Found something? " perm " " foos)
                      (loop permgen
                            (append found
                                    (filter
                                     (lambda (cell)
                                       (and (find (lambda (x) (= x (scand-value cell))) perm)
                                            (not (find (lambda (x) (equal? x (scand-pos cell))) positions))))
                                     cands))))
                    (loop permgen found)))]))))))

(define (find-naked-groups limit solved cands)
  (print "Naked group (" limit ")")
  (let ((fun (lambda (cands) (find-naked-groups-in-set limit cands))))
    (finder fun cands)))

(define (find-hidden-groups-in-set limit cands)
  (call/cc
   (lambda (ret/cc)
     (if (< (length (generator->list (unique-positions-gen cands)))
            (+ limit 1))
         ;; (print "Early return " (generator->list (unique-positions-gen cands)))
         (ret/cc (make-find-result '() '())))
     (let* ((nums (numbers cands))
            (ncounts (number-counts-cond
                      (lambda (len) (and (>= len 2) (<= len limit)))
                      nums))
            (unums (map first ncounts)))
       (if (< (length unums) limit)
           (ret/cc (make-find-result '() '())))

       ;; (print "Unums " unums ", ncounts " ncounts ", cands " cands)
       (let ((kperms (sort (unordered-subset-map
                            (lambda (c) (sort c <))
                            unums limit)
                           (list-less? < equal?))))
         ;; now find if a limited number of cells contain
         ;; a certain hidden group
         (let loop ((permgen (list->generator kperms)) (found '()))
           (match
               (permgen)
             [#!eof (ret/cc (make-find-result '() found))]
             [perm
              (let* ((ncands (remove
                              (lambda (cell)
                                (not
                                 (any
                                  (lambda (x)
                                    (= x (scand-value cell)))
                                  perm)))
                              cands))
                     (positions (generator->list
                                 (unique-positions-gen ncands)))
                     (cells (map (lambda (pos)
                                   (cons pos
                                         (cell-numbers-for-pos pos ncands)))
                                 positions)))
                ;; (print "asdf limit " limit ", perm " perm ", cells " cells)
                (if (= limit (length cells))
                    (begin
                      ;; (print "Found hidden group? " perm
                      ;;        ", positions " positions
                      ;;        "\n\t cands " cands)
                      (loop permgen
                            (append
                             found
                             (filter
                              (lambda (cell)
                                (and (not (any
                                           (lambda (val)
                                             (= val (scand-value cell)))
                                           perm))
                                     (any
                                      (lambda (pos)
                                        (equal? pos (scand-pos cell)))
                                      positions)))
                              cands))))
                    (loop permgen found)))])))))))

(define (find-hidden-groups limit solved cands)
  (print "Hidden group (" limit ")")
  (let ((fun (lambda (cands) (find-hidden-groups-in-set limit cands))))
    (finder fun cands)))

;; 2 or 3 candidates of a number in the same line and box,
;; no other candidates for the same box
;; --> other candidates in the same line can be eliminated
(define (find-pointing-pairs solved cands)
  (print "Pointing pairs")
  (let loop ((bs (iota SUDOKU-NUMBERS 1)) (found '()))
    (match bs
      ['() (make-find-result '() found)]
      [(b . rest)
       (let ((box (scand-get-box b cands)))
         (if (< (length box) 2)
             (loop rest found)
             (let nloop ((nums (scand-unique-numbers box)) (nfound found))
               (match nums
                 ['() (loop rest nfound)]
                 [(n . nrest)
                  (let ((xs (scand-filter-value n box)))
                    (if (not (and (or (= (length xs) 2)
                                      (= (length xs) 3))
                                  (cells-in-line? xs)))
                        (nloop nrest nfound)
                        (let* ((ys (if (cells-in-row? xs)
                                       (scand-get-row
                                        (scand-cell-row (first xs)) cands)
                                       (scand-get-col
                                        (scand-cell-col (first xs))
                                        cands)))
                               (positions (map scand-pos xs))
                               (nnfound (filter
                                         (lambda (cell)
                                           (and (= n (scand-value cell))
                                                (not (any (lambda (pos)
                                                            (equal?
                                                             pos
                                                             (scand-pos cell)))
                                                          positions))))
                                         ys)))
                          (match nnfound
                            ['() (nloop nrest nfound)]
                            [(h . t) (nloop nrest (append nfound nnfound))]))))]))))])))

(define (cells-in-same-box cells)
  (match cells
    ['() #f]
    [(cell0 . '()) #t]
    [(cell0 . cells)
     (every (lambda (cell)
              (pos-same-box? (scand-pos cell0) (scand-pos cell)))
            cells)]))

;; 2 or 3 candidates of a number in the same line and box,
;; no other candidates for the same line
;; --> other candidates in the same box can be eliminated
(define (find-box/line-reduction solved cands)
  (print "Box/line reduction")
  (let*
      ((setfun
        (lambda (cset cands)
          (let* ((fun (lambda (n) (scand-filter-value n cset)))
                 (num-occurrances (remove (lambda (n-cells)
                                           (null? (cdr n-cells)))
                                         (map (lambda (n) (cons n (fun n)))
                                              (iota SUDOKU-NUMBERS 1))))
                 (interesting (filter
                               (lambda (n-cells)
                                 (let ((len (length (cdr n-cells))))
                                   (and
                                    (or (= len 2) (= len 3))
                                    (cells-in-same-box (cdr n-cells)))))
                               num-occurrances))
                 (results (map (lambda (foo)
                                 (let* ((n (car foo))
                                        (cells (cdr foo))
                                        (b (pos-box (scand-pos (first cells))))
                                        (box-cells (scand-get-box b cands))
                                        (other (filter
                                                (lambda (cell)
                                                  (and
                                                   (= n (scand-value cell))
                                                   (not (find (lambda (x) (equal? x cell)) cells))))
                                                box-cells)))
                                   other)) interesting)))
            ;; (print "Foos " num-occurrances)
            ;; (print "Interesting " interesting)
            ;; (print "    IN " cset)
            ;; (print "    results " results)
            (flatten results))))
       (gen (make-2d-generator
             (lambda (i fun) (cons i fun))
             (list->generator (list scand-get-row scand-get-col))
             (lambda (i) (make-iota-generator SUDOKU-NUMBERS 1)))))
    (let loop ((g gen) (found '()))
      (match (g)
        [#!eof (make-find-result '() found)]
        [(fun . i)
         (match
             (setfun (fun i cands) cands)
           [() (loop g found)]
           [xs (loop g (append found xs))])]))))

(define (is-ywing-numbers-comb? pivot winga wingb)
  (equal? (cdr pivot)
          (sort (lset-difference
                 =
                 (lset-union = (cdr winga) (cdr wingb))
                 (lset-intersection = (cdr winga) (cdr wingb))) <)))

(define (is-ywing? pivot wing1 wing2)
  (cond
   ((not (and (pos-sees? (car pivot) (car wing1))
              (pos-sees? (car pivot) (car wing2)))) #f)
   ((not (= 3 (length (delete-neighbour-dups
                       = (sort (append-map cdr (list pivot wing1 wing2))
                               <))))) #f)
   (else (is-ywing-numbers-comb? pivot wing1 wing2))))

;; three cells wing1-pivot-wing2
;; with values  AC-AB-BC
;; -> C eliminated from cells seen by wing1 and wing2
(define (find-ywing solved cands)
  (print "Y-wing")
  (let* ((pairs (find-cells-with-n-candidates 2 cands))
         (good-comb-fun
          (lambda (comb)
            ;; (print "Inspect comb " comb)
            (let ((numbers
                   (delete-neighbour-dups
                    = (sort (append-map cdr comb) <))))
              (and (= 3 (length numbers))
                   (every (lambda (n)
                            (= 2 (length
                                  (filter (lambda (x)
                                            (= n x))
                                          (append-map cdr comb)))))
                          numbers)))))
         (combs (reverse
                 (map
                  (lambda (comb)
                    (sort comb (lambda (a b) (pos-less? (car a) (car b)))))
                  (combination-fold (lambda (comb res)
                                      (if (good-comb-fun comb)
                                          (cons comb res)
                                          res))
                                    '() pairs 3)))))
    ;; (print "Pairs " pairs)
    ;; (print "Combinations for " (length pairs)
    ;;        " pairs, filtered combinations " (length combs))
    ;; (for-each (lambda (c) (print " comb " c)) combs)
    (let loop ((cs combs) (found '()))
      (match cs
        ['()
         (let ((tmp (delete-neighbour-dups equal? (sort found scand-less?))))
           ;; (print "Y-wing return " tmp)
           (make-find-result '() tmp))]
        [(combo . rest)
         (loop
          rest
          (append
           found
           (permutation-fold
            (lambda (perm res)
              (let* ((wing1 (first perm))
                     (pivot (second perm))
                     (wing2 (third perm)))
                ;; (print "Inspect perm " wing1 " - " pivot " - " wing2)
                (if (not (is-ywing? pivot wing1 wing2))
                    res
                    (let* ((n (first (lset-intersection
                                      = (cdr wing1) (cdr wing2))))
                           (poss (map car (list pivot wing1 wing2)))
                           (xs (filter (lambda (cell)
                                         (let ((pos (scand-pos cell)))
                                           (and (= n (scand-value cell))
                                                (every
                                                 (lambda (pos2)
                                                   (not (equal? pos pos2)))
                                                 poss)
                                                (pos-sees? (car wing1) pos)
                                                (pos-sees? (car wing2) pos))))
                                       cands)))
                      ;; (print "Possible y-wing")
                      (match xs
                        ['() res]
                        [(a . as)
                         (begin
                           ;; (print "Is a y-wing perm! " wing1 " - " pivot " - " wing2)
                           ;; (print "   maybe eliminate (" n ") << " xs " >>")
                           ;; (print " c       " perm)
                           (append res xs))])))))
            '() combo 3)))]))))

(define (find-xwing solved cands)
  (print "X-wing")
  (let* ((ijgen (make-2d-generator
                 (lambda (i j) (cons i j))
                 (make-iota-generator 8 1)
                 (lambda (i) (make-iota-generator (- SUDOKU-NUMBERS i) (+ i 1)))))
         (relim (lambda (xset cands)
                  (match
                      xset
                    [(($ scand ($ pos i1 j1 _) n) ($ scand ($ pos i1 j2 _) n)
                      ($ scand ($ pos i2 j1 _) n) ($ scand ($ pos i2 j2 _) n))
                     (filter (lambda (cell)
                               (and (= n (scand-value cell))
                                    (not (= i1 (pos-row (scand-pos cell))))
                                    (not (= i2 (pos-row (scand-pos cell))))
                                    (or
                                     (= j1 (pos-col (scand-pos cell)))
                                     (= j2 (pos-col (scand-pos cell))))))
                             cands)])))
         (celim (lambda (xset cands)
                  (match
                      xset
                    [(($ scand ($ pos i1 j1 _) n) ($ scand ($ pos i2 j1 _) n)
                      ($ scand ($ pos i1 j2 _) n) ($ scand ($ pos i2 j2 _) n))
                     (filter (lambda (cell)
                               (and (= n (scand-value cell))
                                    (not (= j1 (pos-col (scand-pos cell))))
                                    (not (= j2 (pos-col (scand-pos cell))))
                                    (or
                                     (= i1 (pos-row (scand-pos cell)))
                                     (= i2 (pos-row (scand-pos cell))))))
                             cands)])))
         (rowfun (lambda (i j cands)
                   (let* ((is (scand-get-row i cands))
                          (js (scand-get-row j cands))
                          (goodset
                           (lambda (n is js)
                             (let* ((nis (scand-filter-value n is))
                                    (njs (scand-filter-value n js)))
                               (cond ((and (= 2 (length nis))
                                           (= 2 (length njs)))
                                      (match
                                          (map scand-pos (append nis njs))
                                        [(($ pos i1 j1 _) ($ pos i1 j2 _)
                                          ($ pos i2 j1 _) ($ pos i2 j2 _))
                                         (append nis njs)]
                                        [_ '()]))
                                     (else '()))))))
                     (cond ((> 2 (length is)) '())
                           ((> 2 (length js)) '())
                           (else
                            (let nloop ((ns (iota SUDOKU-NUMBERS 1)) (found '()))
                              (match ns
                                ['() found]
                                [(n . nrest)
                                 (match
                                     (goodset n is js)
                                   ['() (nloop nrest found)]
                                   [(c1 c2 c3 c4)
                                    (nloop nrest
                                           (append found
                                                   (relim (list c1 c2 c3 c4)
                                                          cands)))])])))))))
         (colfun (lambda (i j cands)
                   (let* ((is (scand-get-col i cands))
                          (js (scand-get-col j cands))
                          (goodset
                           (lambda (n is js)
                             (let* ((nis (scand-filter-value n is))
                                    (njs (scand-filter-value n js)))
                               (cond ((and (= 2 (length nis))
                                           (= 2 (length njs)))
                                      (match
                                          (map scand-pos (append nis njs))
                                        [(($ pos i1 j1 _) ($ pos i2 j1 _)
                                          ($ pos i1 j2 _) ($ pos i2 j2 _))
                                         (append nis njs)]
                                        [_ '()]))
                                     (else '()))))))
                     (cond ((> 2 (length is)) '())
                           ((> 2 (length js)) '())
                           (else
                            (let nloop ((ns (iota SUDOKU-NUMBERS 1)) (found '()))
                              (match ns
                                ['() found]
                                [(n . nrest)
                                 (match
                                     (goodset n is js)
                                   ['() (nloop nrest found)]
                                   [(c1 c2 c3 c4)
                                    (nloop nrest
                                           (append found
                                                   (celim (list c1 c2 c3 c4)
                                                          cands)))])])))))))
         (gen (make-2d-generator
               (lambda (f ij) (cons f ij))
               (list->generator (list rowfun colfun))
               (lambda (f)
                 (make-2d-generator
                  (lambda (i j) (cons i j))
                  (make-iota-generator 8 1)
                  (lambda (i) (make-iota-generator (- SUDOKU-NUMBERS i) (+ i 1))))))))
    (make-find-result
     '()
     (let loop ((found '()))
       (match (gen)
         [#!eof found]
         [(f . (i . j))
          (begin
            ;; (print "  (f, i, j) = (" f ", " i ", " j ")")
            (loop (append found (f i j cands)))) ])))))

(define (scands-to-scells cands)
  (if (null? cands)
      '()
      (let ((pos0 (scand-pos (car cands)))
            (n0 (scand-value (car cands))))
        (let loop ((cs cands)
                   (current-pos pos0)
                   (current-values '())
                   (res '()))
          (if (null? cs)
              (reverse (cons (make-scell
                              current-pos (reverse current-values)) res))
              (let* ((cell (car cs))
                     (pos1 (scand-pos cell))
                     (n (scand-value cell)))
                (if (pos-same? current-pos pos1)
                    (loop (cdr cs)
                          current-pos
                          (cons n current-values)
                          res)
                    (loop (cdr cs)
                          pos1
                          (cons n '())
                          (cons (make-scell
                                 current-pos
                                 (reverse current-values))
                                res)))))))))


(define (find-xyzwing solved cands)
  (print "XYZ-wing")
  (let-values (((threes rest) (partition
                               (lambda (scell)
                                 (= 3 (length (scell-values scell))))
                               (scands-to-scells cands))))
    (if (null? threes)
        (make-find-result '() '())
        (let-values (((twos blah) (partition
                                   (lambda (scell)
                                     (= 2 (length (scell-values scell))))
                                   rest)))
          (if (null? twos)
              (make-find-result '() '())
              (let loop ((ts threes) (res '()))
                (if (null? ts)
                    (make-find-result '() res)
                    (let* ((hinge (car ts))
                           (ntwos (filter
                                   (lambda (wing)
                                     (and (pos-sees?
                                           (scell-pos hinge)
                                           (scell-pos wing))
                                          (= 2
                                             (length
                                              (lset-intersection
                                               =
                                               (scell-values hinge)
                                               (scell-values wing))))))
                                   twos)))
                      ;; (print "Hinge is now " hinge ", " ntwos)
                      (if (null? ntwos)
                          (loop (cdr ts) res)
                          (loop
                           (cdr ts)
                           (combination-fold
                            (lambda (comb res)
                              (let* ((w1 (first comb))
                                     (w2 (second comb))
                                     (ns (delete-neighbour-dups
                                          =
                                          (sort
                                           (append-map scell-values
                                                       (list hinge w1 w2))
                                           <)))
                                     (zs (lset-intersection
                                          =
                                          (scell-values w1)
                                          (scell-values w2)))
                                     (z (if (null? zs) 0 (car zs))))
                                (if (not (and (= 3 (length ns))
                                              (= 1 (length zs))))
                                    res
                                    (append
                                     res
                                     (filter
                                      (lambda (cell)
                                        (and (= z (scand-value cell))
                                             (every
                                              (lambda (scell)
                                                (and (not (pos-same? (scand-pos cell)
                                                           (scell-pos scell)))
                                                     (pos-sees? (scand-pos cell)
                                                                (scell-pos scell))))
                                              (list hinge w1 w2))))
                                      cands)))))
                            res ntwos 2)))))))))))

(define (update-candidates-solved cands solved)
  (let ((pred (lambda (cell1 cell2)
                (or (scand-same-pos? cell1 cell2)
                    (and (scand-same-val? cell1 cell2)
                         (scand-cell-sees? cell1 cell2))))))
  (let loop ((gen (list->generator solved)) (ncands cands))
    (match
     (gen)
     [#!eof ncands]
     [cell1 (loop gen (remove (lambda (cell2) (pred cell1 cell2)) ncands))]))))

(define (update-candidates cands removals)
  (lset-difference equal? cands removals))

(define (sudoku-solve solved cands)
  (if (null? cands)
      (cons solved cands)
      (let ((funs
             (list find-singles-simple
                   find-singles
                   (lambda (solved cands) (find-naked-groups 2 solved cands))
                   (lambda (solved cands) (find-naked-groups 3 solved cands))
                   (lambda (solved cands) (find-hidden-groups 2 solved cands))
                   (lambda (solved cands) (find-hidden-groups 3 solved cands))
                   (lambda (solved cands) (find-naked-groups 4 solved cands))
                   (lambda (solved cands) (find-hidden-groups 4 solved cands))
                   find-pointing-pairs
                   find-box/line-reduction
                   find-xwing
                   find-ywing
                   find-xyzwing
                   )))
        (let loop ((fgen (list->generator funs)))
          (let ((f (fgen)))
            (if (eof-object? f)
                (cons solved cands)
                (let* ((result (f solved cands))
                       (nsolved (find-result-solved result))
                       (eliminated (find-result-eliminated result)))
                  (cond ((and (null? nsolved) (null? eliminated))
                         (loop fgen))
                        ((null? nsolved)
                         (begin
                           (print "Eliminated " eliminated)
                           (sudoku-solve solved (lset-xor equal? cands eliminated))))
                        (else
                         (begin
                           (print "Found some solutions? " nsolved)
                           (sudoku-solve (merge solved nsolved scand-less?)
                                         (update-candidates-solved
                                          (update-candidates cands eliminated)
                                          nsolved))))))))))))

(define (sudoku-scand-fold fun acc0 cands)
  (fold (lambda (cell acc)
         (let ((pos (scand-pos cell)))
           (fun (pos-row pos) (pos-col pos) (scand-value cell) acc)))
        acc0
        cands))

(define (sudoku-scand-map fun cands)
  (map (lambda (cell)
         (let ((pos (scand-pos cell)))
           (fun (pos-row pos) (pos-col pos) (scand-value cell))))
       cands))

)
