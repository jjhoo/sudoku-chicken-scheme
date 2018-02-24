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
(use srfi-1)
(require-extension combinatorics)
(require-extension matchable)
(require-extension srfi-121)

(define (make-2d-generator yfun gen1 gen2-init)
  (make-coroutine-generator
   (lambda (yield)
     (let loop ()
       (match (gen1)
              [#!eof (yield #!eof)]
              [i (let loop2 ((gen2 (gen2-init)))
                   (match (gen2)
                          [#!eof (loop)]
                          [j (yield (yfun i j))
                             (loop2 gen2)]))])))))

(define (box-number-to-index num)
  (match num
    [1 (cons 1 1)] [2 (cons 1 2)] [3 (cons 1 3)]
    [4 (cons 2 1)] [5 (cons 2 2)] [6 (cons 2 3)]
    [7 (cons 3 1)] [8 (cons 3 2)] [9 (cons 3 3)]
    [_ "invalid"]))

(define (num-to-box-number num)
  (case num
    ((1 2 3) 1)
    ((4 5 6) 2)
    ((7 8 9) 3)))

(define (cell-box-calc row col)
  (cons (num-to-box-number row) (num-to-box-number col)))

(define (scand-cell-pos cell)
  (car cell))

(define (scand-cell-value cell)
  (cdr cell))

(define (scand-init row col n)
  (cons (list row col (cell-box-calc row col)) n))

(define (str-to-solved grid)
  (let loop ((i 1) (j 1) (solved '()) (tmp (string->list grid)))
    (if (null? tmp)
        (reverse solved)
        (let* ((val (string->number (string (car tmp))))
               (nsolved (if (= val 0)
                            solved
                            (cons (scand-init i j val) solved)))
               (ni (if (= j 9) (+ i 1) i))
               (nj (if (= j 9) 1 (+ j 1))))
          (loop ni nj nsolved (cdr tmp))))))

(define (grid-init)
  (let ((g (make-coroutine-generator
            (lambda (yield)
              (let loop ((i 1) (j 1) (n 1))
                (when (not (and (= i 9) (= j 9) (= n 10)))
                      (cond ((> n 9) (loop i (+ j 1) 1))
                            ((> j 9) (loop (+ i 1) 1 1))
                            (else (begin
                                    (yield (scand-init i j n))
                                    (loop i j (+ n 1)))))))))))
    (generator->list g)))

(define (pos-same? pos1 pos2)
  (equal? pos1 pos2))

(define (pos-same-box? pos1 pos2)
  (equal? (third pos1) (third pos2)))

(define (pos-same-col? pos1 pos2)
  (= (second pos1) (second pos2)))

(define (pos-same-row? pos1 pos2)
  (= (first pos1) (first  pos2)))

(define (pos-sees? pos1 pos2)
  (or (pos-same-box? pos1 pos2)
      (pos-same-col? pos1 pos2)
      (pos-same-row? pos1 pos2)))

(define (scand-get-box box cands)
  (let ((box (if (integer? box)
                  (box-number-to-index box)
                  box)))
    (filter (match-lambda [((r c b) . n) (equal? box b)] [_ #f]) cands)))

(define (scand-get-cell pos cands)
  (filter (match-lambda [(p . n) (equal? pos p)] [_ #f]) cands))

(define (scand-get-col col cands)
  (filter (match-lambda [((r c (a . b)) . n) (= col c)] [_ #f]) cands))

(define (scand-get-row row cands)
  (filter (match-lambda [((r c (a . b)) . n) (= row r)] [_ #f]) cands))

(define (unique-positions-gen cands)
  ;; (unique (map (lambda (cell) (first cell)) cands)))
  (gdelete-neighbor-dups
   (list->generator (map (lambda (cell) (first cell)) cands))))

(define (numbers cands)
  (sort (map cdr cands) <))

(define (delete-neighbour-dups = numbers)
  (let ((nil (gensym)))
    (let loop ((xs numbers) (res '()) (current nil))
      (if (eq? xs '())
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

(define (unique-numbers-gen cands)
  (gdelete-neighbor-dups
   (list->generator (numbers cands))))

(define (scand-less? cell1 cell2)
  (cond ((< (first (car cell1)) (first (car cell2))) #t)
        ((= (first (car cell1)) (first (car cell2)))
         (cond ((< (second (car cell1)) (second (car cell2))) #t)
               ((= (second (car cell1)) (second (car cell2)))
                (< (cdr cell1) (cdr cell2)))
               (else #f)))
        (else #f)))

(define (cands-sort+dedupe cands)
  (delete-neighbour-dups equal? (sort cands scand-less?)))

(define (scand-same-pos? cell1 cell2)
  (equal? (car cell1) (car cell2)))

(define (scand-same-val? cell1 cell2)
  (= (cdr cell1) (cdr cell2)))

(define (scand-cell-sees? cell1 cell2)
  (pos-sees? (car cell1) (car cell2)))

(define (scand-pos cell)
  (car cell))

(define (init solved)
  (let loop ((xs solved) (cands (grid-init)))
    (if (null? xs)
        cands
        (let* ((cell1 (car xs))
               (rest (cdr xs))
               (pos1 (scand-cell-pos cell1))
               (pred (lambda (cell2)
                       (let ((pos2 (scand-cell-pos cell2)))
                         (or (pos-same? pos1 pos2) ;; cell is solved
                             (and (pos-sees? pos1 pos2)
                                  (scand-same-val? cell1 cell2))))))
               (ncands (remove pred cands)))
          (loop rest ncands)))))

(define (finder pred cands)
  (let* ((yfun (lambda (i fun) (cons i fun)))
         (gen (make-2d-generator
               yfun
               (make-iota-generator 9 1)
               (lambda ()
                 (list->generator (list scand-get-row
                                        scand-get-col
                                        scand-get-box))))))
    (let loop ((solved '())
               (eliminated '()))
      (match
       (gen)
       [#!eof (cons (cands-sort+dedupe solved) (cands-sort+dedupe eliminated))]
       [(i . f)
        (begin
          ;; (print "Now i = " i ", fun is " f)
          (match
           (pred (f i cands))
           [(() . ())
            (loop solved eliminated)]
           [(nsolved . neliminated)
            (loop (append solved nsolved)
                  (append eliminated neliminated)) ]
         ;; [xxx (begin (print "wtf " xxx ) (cons solved eliminated))]
           ))]))))

;; (for (i 1 9)
  ;;      (dolist (fun2 funs2)
    ;;              (set 'found (append found (pred (fun2 i cands))))))
    ;; (unique (sort found))))

;; Find unsolved cells that have only one candidate left
(define (find-singles-simple solved cands)
  (print "Hidden singles (simple)")
  (let ((fun (lambda (cands)
               (generator-fold
                (lambda (pos res)
                  (match
                   (generator->list
                    (gfilter (lambda (cell)
                               (pos-same? pos (scand-pos cell)))
                             (list->generator cands)))
                   [() res]
                   [(cell) (cons (cons cell (car res)) '())]
                   [_ res]))
                (cons '() '())
                (unique-positions-gen cands)))))
    (finder fun cands)))

(define (find-singles solved cands)
  (print "Hidden singles")
  (let ((fun (lambda (cands)
               (generator-fold
                (lambda (n res)
                  (match
                   (generator->list
                    (gfilter (lambda (cell) (= n (cdr cell)))
                             (list->generator cands)))
                   [() res]
                   [(cell) (cons (cons cell (car res)) '())]
                   [xxx res]))
                (cons '() '())
                (unique-numbers-gen cands)))))
    (finder fun cands)))

(define (list-less? < =)
  (lambda (alist blist)
    (let loop ((as alist) (bs blist))
      (cond ((and (eq? as '()) (eq? bs '())) #f)
            ((or (eq? as '()) (eq? bs '())) #f)
            ((< (car as) (car bs)) #t)
            ((= (car as) (car bs)) (loop (cdr as) (cdr bs)))
            (else #f)))))

(define (cell-numbers-for-pos pos cands)
  (map (lambda (cell) (cdr cell)) (scand-get-cell pos cands)))

(define (find-cells-cond pred cands)
  (let* ((poss (generator->list (unique-positions-gen cands)))
         (pos-nums (map (lambda (pos)
                          (cons pos (cell-numbers-for-pos pos cands)))
                        poss)))
    (filter (lambda (pos-num-p) (pred (cdr pos-num-p))) pos-nums)))

(define (find-naked-groups-in-set limit cands)
  (if (< (length (generator->list (unique-positions-gen cands))) (+ limit 1))
      ;; (print "Early return " (generator->list (unique-positions-gen cands)))
      (cons '() '())
      (begin
        (let* ((nums (numbers cands))
               (ncounts (number-counts nums))
               (unums (map (lambda (x) (first x)) ncounts))
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
             [#!eof (cons '() found)]
             [perm
              (let* ((foos (find-cells-cond
                            (lambda (numbers)
                              (or (equal? perm numbers)
                                  (eq? '() (lset-difference equal? numbers perm))))
                            cands)))
                (if (= (length foos) limit)
                    (let ((positions (map (lambda (cell) (first cell)) foos)))
                      ;; (print "Found something? " perm " " foos)
                      (loop permgen
                            (append found
                                    (filter
                                     (lambda (cell)
                                       (and (find (lambda (x) (= x (scand-cell-value cell))) perm)
                                            (not (find (lambda (x) (equal? x (scand-cell-pos cell))) positions))))
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
         (ret/cc (cons '() '())))
     (let* ((nums (numbers cands))
            (ncounts (number-counts-cond
                      (lambda (len) (and (>= len 2) (<= len limit)))
                      nums))
            (unums (map (lambda (x) (first x)) ncounts)))
       (if (< (length unums) limit)
           (ret/cc (cons '() '())))

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
             [#!eof (ret/cc (cons '() found))]
             [perm
              (let* ((ncands (remove
                              (lambda (cell)
                                (not
                                 (any
                                  (lambda (x)
                                    (= x (scand-cell-value cell)))
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
                                             (= val (scand-cell-value cell)))
                                           perm))
                                     (any
                                      (lambda (pos)
                                        (equal? pos (scand-cell-pos cell)))
                                      positions)))
                              cands))))
                    (loop permgen found)))])))))))

(define (find-hidden-groups limit solved cands)
  (print "Hidden group (" limit ")")
  (let ((fun (lambda (cands) (find-hidden-groups-in-set limit cands))))
    (finder fun cands)))

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

(define (solver solved cands)
  (if (eq? cands '())
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
                   )))
        (let loop ((fgen (list->generator funs)))
          (match
           (fgen)
           [#!eof (cons solved cands)]
           [f (match
               (f solved cands)
               [(() . ()) (loop fgen)]
               [(() . eliminated)
                (begin
                  (print "Eliminated " eliminated)
                  (solver solved (lset-xor equal? cands eliminated)))]
               [(nsolved . eliminated)
                (begin
                  (print "Found some solutions? " nsolved)
                  (solver (merge solved nsolved scand-less?)
                          (update-candidates-solved
                           (update-candidates cands eliminated)
                           nsolved)))])])))))

(define ns (list 2 2 2 2 2 5 5 6 6 7 7 9 9 9))
(print "list " ns
       "\n deduped "
       (delete-neighbour-dups = ns)
       "\n number counts (no limit) "
       (number-counts ns)
       "\n number counts (limit = 2) "
       (number-counts ns 2)
       "\n number counts (limit = 3) "
       (number-counts ns 3))

;; (define GRID "014600300050000007090840100000400800600050009007009000008016030300000010009008570")
;; has hidden triple
(define GRID "300000000970010000600583000200000900500621003008000005000435002000090056000000001")
(print GRID)

(define solved (str-to-solved GRID))
;; (print (length solved))
;; (print solved)

(define cands (init solved))
;; (print (find-singles solved cands))
;; (print cands)
;; (print (length cands))

(match
 (solver solved cands)
 [(cells . ()) (print "Solved")]
 [(nsolved . ncands)
  (let* ((xx (lset-difference equal? nsolved solved))
         (len (length xx)))
    (print "Found some (count = " len ") " xx))])
