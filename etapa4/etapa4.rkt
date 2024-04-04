#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (let* (
           (pref (reverse(lcp-h w1 w2 '())))
           (pref-length (length pref))
         )
      (cons pref
            (cons (drop w2 pref-length)
                  (list(drop w1 pref-length))
             )
       )
    );facem o lista de 3 liste
 )

(define (concat-drop L1 L2 acc)
     (lcp-h (cdr L1) (cdr L2) (cons (car L1) acc)) ;apel recursiv
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


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
   (lcpl-helper (stream-rest words) (stream-first words)) ;avem primul cuvant ca "prefixul curent" (vom parcurge restul cuvintelor)
 )

(define (lcpl-helper words acc)
  
   (if (not (stream-empty? words)) ;daca streamul nu e nul
       (lcpl-helper (stream-rest words) ;restul cuvintelor
                    (car (longest-common-prefix (stream-first words) acc)
                     ) ;prefixul comun dintre "prefixul curent" si urmatorul cuvant
       );apelul recursiv
       acc
    )
 )



(define (match-pattern-with-label st pattern)
  (if (stream-empty? st)
     (let* (
            (label (get-branch-label(first-branch st)))
            (lcp (longest-common-prefix label pattern))
            (pref (car lcp))
           )
      (if (and
             (not (null? pattern))
              (equal? (car label)
                  (car pattern))
              ) ;daca eticheta si sablonul incep cu aceeasi litera avem 3 cazuri
           (cond
                ((equal?  pref
                          pattern
                  )
                  #t ; 1 daca sablonul este continut in eticheta
                 )
                ((equal?  pref
                          label)
                     (list
                        label
                        (caddr lcp)
                         (get-branch-subtree(get-ch-branch st (car pattern)))
                     )
                 ) ; 2 daca eticheta e continuta in sablon mai cautam in subtree continuarea

                ((not(equal? (length pref); lungimea prefixului comun
                             (length label) ; lungimea etichetei
                      )
                   )
                  (list #f pref)
                 ) ;3 daca eticheta nu este continuta integral in sablon
                 (else (list #f '()))
              )
            (match-pattern-with-label (other-branches st) pattern) ;cautam alta ramura
        )
       )

       (list #f '()) ; nu gasim o ramura cu aceeasi eticheta ca sablonul
  )
 )


(define (st-has-pattern? st pattern)
  (let (
        (result (match-pattern-with-label st pattern))
        )
     (if (equal?  result #t)
         #t
         (if 
             (equal?
                   (car result)
                   #f
                 )
                
           #f
           (st-has-pattern? (caddr result) (cadr result))
         )
      )
    )
 )


(define (get-suffixes text)
        (if (and
               (not (null? text))
               (not (equal?           ; comparam primul caracter cu $
                       (car text)
                        #\$
                     )
                 )
             )
            (stream-cons text (get-suffixes (cdr text))) ; facem lista din text si listele anterioare(sufixele mai mici)
            (stream text) ; cazul de baza intoarce lista ($) la care se concateneaza
         )
  )

(define (stream-foldr f init stream)
  (if(stream-empty? stream)
     init
     (f (stream-first stream)
        (stream-foldr f init (stream-rest stream)
      )
  )
 )
 )

(define (get-ch-words words ch)
  
  (stream-foldr (lambda(w acc) ; mereu 2 parametrii, elementul curent si acumulatorul
            (if (and
                  (not (null? w)) ; cuvantul sa nu fie null
                  (equal? ch (car w)) ; sa inceapa cu caracterul ch
                 )
                 (stream-cons w acc)
                 acc
             )
           )
          empty-stream
          words
   )
 )


(define (ast-func suffixes)
  (cons
     (list(car(stream-first suffixes))) ; lista formata din primul caracter
     (stream-map (lambda(x) (cdr x)) suffixes)) ; lista formata din restul cuvintelor
  )


(define (cst-func suffixes)
  (let*
       ( ;blockul in sine care contine asocierile
         (lcp-list (longest-common-prefix-of-collection suffixes))
        )
       
       (cons  ; functia pentru care folosim asocierile
          lcp-list
          (stream-map (lambda(x) (drop x (length lcp-list))) suffixes)
        )
   )
 )


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-function suffixes alphabet)
  (define (process-alphabet ch st) ; in st vom pastra arborele, iar in ch litera la care am ajuns din alfabet
      (if (stream-empty? (get-ch-words suffixes ch)) ; verifica daca exista sufixe care incep cu ch
          st ; daca nu exista lasam branchul la fel
          (let* (
                 (get-ch-suffixes (get-ch-words suffixes ch)) ; sufixele care incep cu caracterul ch
                 (get-label (car (labeling-function get-ch-suffixes))) ; eticheta
                 (get-new-suffixes (cdr (labeling-function get-ch-suffixes))) ; celelalte sufixe (cu eticheta scoasa)
                 (get-subtree (suffixes->st labeling-function get-new-suffixes alphabet)) ;apelul recursiv pentru crearea arborelui din noile sufixe
                 (get-branch (cons get-label get-subtree))
                 ) ; se leaga eticheta si se apeleaza recursiv pentru a se adauga etichetele urmatoare
                 (stream-cons get-branch st)
           )
       )
   ) ; primul cons creeaza branchul, al doilea il leaga la arbore

  (foldr (lambda (ch st) (process-alphabet ch st)) empty-stream alphabet)
  ; se face concatenarea (functia din foldr o sa intoarca in final branchurile pentru fiecare litera valida)
 )


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (text)
    (lambda (labeling-func)
      (let* ((text-with-$ (append text (list #\$))) ;textul cu $
             (get-alphabet (sort (remove-duplicates text-with-$ char=?) char<?))) ; literele din text sortate crescator
        (suffixes->st labeling-func (get-suffixes text-with-$) get-alphabet)))) ; arborele
  )


(define (text->ast text)
  ((text->st text) ast-func)
 )


(define (text->cst text)
  ((text->st text) cst-func)
 )



; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  ;(let * (
         ; (text->ast text)
       ;  )
       ; (st-has-pattern?  ast pattern)
  ;)
  2
 )


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let manipulate-subtree
         (
          (st (text->cst text)) ; arborele
          (result '()) ; stocam prefixul in result, pe care il initializam cu null         
          )
      
      (if (st-empty? st) ; verificam daca am ajuns la arborele null  
          #f ; daca am ajuns pe null inseamna ca nu exista un astfel de cuvant
          (let* (
                 (current-branch (first-branch st)) ; ramura curenta               
                 (current-label (get-branch-label current-branch)) ; eticheta curenta                 
                 (current-subtree (get-branch-subtree current-branch)) ; subarborele curent                 
                 (current-prefix (append result current-label)) ; prefixul current(adaugam la ce am gasit pana acum eticheta curenta)
                 (new-length (length current-prefix)) ; lungimea noului prefix
                 )
           
                (if (and
                     (>= new-length len) ; daca am gasit un cuvant care se repeta care are lungimea cel putin len
                     (not (st-empty? current-subtree)) ; daca subarborele nu e null vrem sa continuam deplasarea
                     (not (st-empty? (other-branches current-subtree))) ;daca mai avem cel putin 1 branch pe langa first
                     )
                    (take current-prefix len) ; luam primele len caractere din prefixul gasit
                    (or (manipulate-subtree current-subtree current-prefix) ; ne deplasam pe subarbore (actualizam st si result)
                        (manipulate-subtree (other-branches st) result))  ; ne deplasam pe celelalte ramuri (actualizam st si result)
                    ;or o sa intoarca #f sau rezultatul gasit
                )
            )
          )
      )
  )
