#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (let * (
          (ast (text->ast text))
         )
         (st-has-pattern?  ast pattern)
  )
 )

; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (longest-common-substring text1 text2)
    (let traverse-suffixes (
                            (current-text text2)
                            (result '())
                            )
       (let* (
             (st1 (text->cst text1)) ; arborele de sufixe pentru primul text
             (suffixes (get-suffixes current-text)) ; sufixele pentru textul2
             (current-common-string (foldl (lambda(x acc)   ; facem o lista care va contine cel mai bun sufix al text2 care apare in text1           
                                             (if(and (not (null? x)) ; nu e null sufixul
                                                     (st-has-pattern? st1 x) ; apare in arborele de sufixe al text1
                                                     (> (length x) (length acc)) ; are lungimea mai mare decat cea de pana acum
                                                 )
                                                 x
                                                 acc
                                              )
                                            )
            
                                           '()
                                           suffixes
                                     )
               )
            )
        
            (if (null? current-text) ; apelam recursiv pentru toate prefixele lui text2
                  result
                  (let ((new-text(drop-right current-text 1)))
                       (if (< (length current-common-string) (length result)) ; daca am gasit unul cu lungimea mai buna
                           (traverse-suffixes new-text result)
                           (traverse-suffixes new-text current-common-string)
                        )
                   )
             )
       )
    )     
 )


; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
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
                     (not (null? (other-branches current-subtree))) ;daca mai avem cel putin 1 branch pe langa first
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
