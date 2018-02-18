;; Copyright (c) 2018 Jani J. Hakala <jjhakala@gmail.com> Jyv�skyl�, Finland
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
(define (num-to-box-number num)
  (case num
    ((1 2 3) 1)
    ((4 5 6) 2)
    ((7 8 9) 3)))

(define (cell-box-calc row col)
  (cons (num-to-box-number row) (num-to-box-number col)))

(define (cell-init row col n)
  (cons (list row col (cell-box-calc row col)) n))

(define (str-to-solved grid)
  (let loop ((i 1) (j 1) (solved '()) (tmp (string->list grid)))
    (if (null? tmp)
        (reverse solved)
        (let* ((val (string->number (string (car tmp))))
               (nsolved (if (= val 0)
                            solved
                            (cons (cell-init i j val) solved)))
               (ni (if (= j 9) (+ i 1) i))
               (nj (if (= j 9) 1 (+ j 1))))
          (loop ni nj nsolved (cdr tmp))))))

(define GRID "014600300050000007090840100000400800600050009007009000008016030300000010009008570")
(print GRID)

(define solved (str-to-solved GRID))
(print solved)
