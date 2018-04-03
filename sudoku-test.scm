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

(use sudoku)
(require-extension matchable)

;; This has box/line reduction, simple colouring and y-wing
;; (define GRID "000040700500780020070002006810007900460000051009600078900800010080064009002050000")
;; needs naked triple, y-wing
;; (define GRID "014600300050000007090840100000400800600050009007009000008016030300000010009008570")
;; can proceed with hidden pair
;; (define GRID "000000000904607000076804100309701080008000300050308702007502610000403208000000000")
;; naked triple, hidden triple
;; (define GRID "300000000970010000600583000200000900500621003008000005000435002000090056000000001")
;; has pointing pairs
;; (define GRID "000004910009003520050100840000306080000000000070209000037008060085400700014700000")
;; has pointing pairs and box/line
;; (define GRID "000921003009000060000000500080403006007000800500700040003000000020000700800195000")
;; has x-wing
;; (define GRID "700600008800030000090000310006740005005806900400092100087000020000060009600008001")
;; has x-wing, xyz-wing, x-cycles (+ simple colouring)
(define GRID "000704005020010070000080002090006250600070008053200010400090000030060090200407000")
(print GRID)

(define solved (sudoku-string->grid GRID))
;; (print (length solved))
;; (print solved)

(define cands (sudoku-init solved))
;; (print (find-singles solved cands))
;; (print cands)
;; (print (length cands))
;; (define test (make-scand (make-pos 1 1 (make-box 1 1)) 8))

(match
 (sudoku-solve solved cands)
 [(cells . ()) (print "Solved")]
 [(nsolved . ncands)
  (let* ((xx (lset-difference equal? nsolved solved))
         (len (length xx)))
    (print "Found some (count = " len ") " xx))])
