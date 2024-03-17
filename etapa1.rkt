#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))
;MIGHT DELETE LATER
(define stree-1
  '(((#\$))
    ((#\a) ((#\$))
           ((#\n #\a) ((#\$))
                      ((#\n #\a #\$))))
    ((#\b #\a #\n #\a #\n #\a #\$))
    ((#\n #\a) ((#\$))
               ((#\n #\a #\$)))))
; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix w1 w2)
         (append (list (lcp-h w1 w2 '()))
                 (list (drop w1 (length (lcp-h w1 w2 '()))))
                 (list (drop w2 (length (lcp-h w1 w2 '()))))
          );facem o lista de 3 liste
 )

(define (concat-drop L1 L2 acc)
     (lcp-h (cdr L1) (cdr L2) (append acc (list(car L1)))) ;apel recursiv
  )

(define (lcp-h L1 L2 acc)
  (if (and (not (null? L1)) (not (null? L2))) ;daca nicio lista nu e nula
      (if (equal? (car L1) (car L2)) ;verific daca au prima litera aceeasi
          (concat-drop L1 L2 acc);adaug la prefixul curent
          acc ;daca nu-s egale am gasit prefixu
       )
      acc ;daca una e nula am gasit prefixu
  )
 )

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (longest-common-prefix-of-list words)
   (lcpl-helper (cdr words) (car words)) ;avem primul cuvant ca "prefixul curent" (vom parcurge restul cuvintelor)
 )

(define (lcpl-helper words acc)
   (if (not (null? words)) ;daca lista nu e nula
       (lcpl-helper (cdr words) ;restul cuvintelor
                    (car (longest-common-prefix (car words) acc)
                     ) ;prefixul comun dintre "prefixul curent" si urmatorul cuvant
       );apelul recursiv
       acc
    )
 )

;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.
(define (match-pattern-with-label st pattern)
  (if (not (null? st))
      (if (equal? (car (get-branch-label(first-branch st
                                         )
                        )
                   )
                  (car pattern)
           ) ;daca eticheta si sablonul incep cu aceeasi litera avem 3 cazuri
           (cond
                ((equal? (car (longest-common-prefix (get-branch-label(first-branch st
                                                                       )
                                                      )
                                                      pattern
                               )
                          )
                          pattern
                  )
                  #t ; 1 daca sablonul este continut in eticheta
                 )
                ((not(equal? (length(car (longest-common-prefix (get-branch-label(first-branch st
                                                                              )
                                                             )
                                                             pattern
                                      )
                                 )
                           ); lungimea prefixului comun
                          (length(get-branch-label(first-branch st
                                                   )
                                  )
                                                             
                           ) ; lungimea etichetei
                      )
                   )
                  (list #f (car (longest-common-prefix (get-branch-label(first-branch st
                                                                         )
                                                        )
                                                        pattern
                                 )
                            )
                   )
                 ) ;3 daca eticheta nu este continuta integral in sablon
                   
                (else (case2 st pattern '())) ; 2 daca eticheta e continuta in sablon mai cautam in subtree continuarea
            )
            (match-pattern-with-label (other-branches st) pattern) ;cautam alta ramura
        )
         
       (list #f '()) ; nu gasim o ramura cu aceeasi eticheta ca sablonul
  )
 )


 (define (case2 st pattern acc)
   (if (not(= (length pattern) 0))
      (if (equal? (car (longest-common-prefix (get-branch-label(first-branch st
                                                                )
                                               )
                                               pattern
                        )
                   ) 
                   (get-branch-label(first-branch st
                                     )
                    )
            ) ; daca toata eticheta e continuta de sablon
           (case2 (get-branch-subtree st)
                  (cadr (longest-common-prefix (get-branch-label(first-branch st
                                                                )
                                               )
                                               pattern
                        )
                   )
                  (append (car (longest-common-prefix (get-branch-label(first-branch st
                                                                        )
                                                       )
                                                       pattern
                                )
                                                  
                           )
                           (list acc)
                   )
            ) ; 2 daca eticheta este continuta in sablon mai avem de cautat in subtreeul de sub eticheta
            (list #f acc) ; nu e continuta complet in continuare
       )
       st
  )
)
; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
     ; daca inca a ramas o bucata din pattern de cautat
    (if (not (null? pattern)
         ) 
               ; cat timp st-ul nu e null(daca e null intoarcem false)
              (if (not (null? st)
                   )
                   ; verificam cat din pattern e continut pana atunci
                   (if (equal? (car (longest-common-prefix (get-branch-label(get-ch-branch st (car pattern)
                                                                             )
                                                            )
                                                            pattern
                                     )
                                )
                                (get-branch-label(get-ch-branch st (car pattern)
                                                  )
                                 )
                                
                        )
                        (st-has-pattern? st (drop pattern (length(get-branch-label(get-ch-branch st (car pattern
                                                                                                     )
                                                                                   )
                                                                  )
                                                           )
                                             )
                         )
                        #f
                    )
                    #f
               )
        #t
     )
 )