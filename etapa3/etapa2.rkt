#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")

(provide (all-defined-out))

;; În această etapă definim algoritmul de construcție a unui
;; arbore de sufixe (atât cel compact, cât și cel atomic) pe
;; baza unui text și a unui alfabet dat. Se știe că textul
;; utilizează doar simboluri prezente în alfabet.
;; Abordarea este următoarea:
;; 1. obținem toate sufixele textului
;; 2. pentru fiecare caracter din alfabet, determinăm sufixele
;;    care încep cu acel caracter: ele vor fi grupate în câte
;;    o ramură a arborelui de sufixe (exceptând caracterele
;;    care nu apar in text - acestea nu generează ramuri)
;; 3. pentru fiecare listă de sufixe care încep cu un același
;;    caracter, determinăm eticheta ramurii, respectiv noile
;;    sufixe care vor genera subarborele de sub etichetă
;;    - în cazul unui AST (atomic suffix tree), eticheta este
;;      chiar primul caracter, iar noile sufixe sunt vechile
;;      sufixe fără primul caracter
;;    - în cazul unui CST (compact suffix tree), eticheta este
;;      cel mai lung prefix comun al sufixelor, iar sufixele
;;      noi se obțin din cele vechi prin îndepărtarea acestui
;;      prefix
;; 4. transformăm fiecare rezultat de la pasul 3 într-o ramură
;;    - eticheta este deja calculată
;;    - calculul subarborilor se realizează repetând pașii 2-4
;;      pentru noile sufixe
; sufixe banana
(define suff-1
  '((#\b #\a #\n #\a #\n #\a #\$)
    (#\a #\n #\a #\n #\a #\$)
    (#\n #\a #\n #\a #\$)
    (#\a #\n #\a #\$)
    (#\n #\a #\$)
    (#\a #\$)
    (#\$)))

; TODO 1
; Implementați recursiv o funcție care primește un text (listă 
; de caractere) și determină lista tuturor sufixelor acestuia
; (de la cel mai lung la cel mai scurt).
; Se știe că textul se va termina cu caracterul special "$", 
; și cu acest caracter trebuie să se termine și toate sufixele
; din lista rezultat (de la întreg textul urmat de "$" până la
; sufixul vid - reprezentat de un singur "$").
; ex:
; (get-suffixes '(#\w #\h #\y #\$))
; => '((#\w #\h #\y #\$) (#\h #\y #\$) (#\y #\$) (#\$))
; Folosiți recursivitate pe stivă.
(define (get-suffixes text)
        (if (and
               (not (null? text))
               (not (equal?           ; comparam primul caracter cu $
                       (car text)
                        #\$
                     )
                 )
             )
            (cons text (get-suffixes (cdr text))) ; facem lista din text si listele anterioare(sufixele mai mici)
            (list text) ; cazul de baza intoarce lista ($) la care se concateneaza
         )
  )

; TODO 2
; Implementați o funcție care primește o listă de cuvinte 
; și un caracter ch și întoarce acele cuvinte din listă care 
; încep cu caracterul ch.
; Atenție, este posibil ca unele cuvinte să fie vide.
; Folosiți funcționale (și nu folosiți recursivitate explicită).
(define (get-ch-words words ch)
  
  (foldr (lambda(w acc) ; mereu 2 parametrii, elementul curent si acumulatorul
            (if (and
                  (not (null? w)) ; cuvantul sa nu fie null
                  (equal? ch (car w)) ; sa inceapa cu caracterul ch
                 )
                 (cons w acc)
                 acc
             )
           )
          '()
          words
   )
 )

     


; TODO 3
; Implementați o funcție care primește o listă nevidă de sufixe 
; care încep cu același caracter și calculează perechea
; (etichetă AST pentru aceste sufixe, lista noilor sufixe).
; Reamintim că pentru un AST eticheta este chiar primul caracter
; (dar sub formă de listă de caractere, pentru a putea fi 
; procesată la fel ca etichetele mai complexe), iar noile sufixe 
; se obțin din cele vechi prin eliminarea acestui caracter.
; Nu folosiți recursivitate explicită.
(define (ast-func suffixes)
  (cons
     (list (caar suffixes)) ; lista formata din primul caracter
     (map (lambda(x) (cdr x)) suffixes)) ; lista formata din restul cuvintelor
  )


; TODO 4
; Implementați o funcție care primește o listă nevidă de sufixe 
; care încep cu același caracter și calculează perechea
; (etichetă CST pentru aceste sufixe, lista noilor sufixe).
; Reamintim că pentru un CST eticheta este cel mai lung prefix
; comun, iar noile sufixe se obțin din cele vechi prin eliminarea 
; acestui prefix.
; Nu folosiți recursivitate explicită.

(define (cst-func suffixes)
  (let*
       ( ;blockul in sine care contine asocierile
         (lcp-list (longest-common-prefix-of-list suffixes))
        )
       
       (cons  ; functia pentru care folosim asocierile
          lcp-list
          (map (lambda(x) (drop x (length lcp-list))) suffixes)
        )
   )
 )

; TODO 5
; Implementați funcția suffixes->st care construiește un
; arbore de sufixe pe baza unei liste de sufixe, a unui 
; alfabet (listă de caractere care include toate caracterele
; din sufixe), și a unei funcții care indică modul de
; etichetare (atomic sau compact).
; Când argumentul funcție va fi ast-func, vom obține un AST.
; Când argumentul funcție va fi cst-func, vom obține un CST.
; Obs: Funcția surprinde pașii 2-4 descriși mai sus.
; Funcția suffixes->st poate fi recursivă explicit, dar
; pentru celelalte prelucrări (pașii 2 și 3) trebuie să
; folosiți funcționale.


(define (suffixes->st labeling-function suffixes alphabet)
  (define (process-alphabet ch st) ; in st vom pastra arborele, iar in ch litera la care am ajuns din alfabet
      (if (null? (get-ch-words suffixes ch)) ; verifica daca exista sufixe care incep cu ch
          st ; daca nu exista lasam branchul la fel
          (let* (
                 (get-ch-suffixes (get-ch-words suffixes ch)) ; sufixele care incep cu caracterul ch
                 (get-label (car (labeling-function get-ch-suffixes))) ; eticheta
                 (get-new-suffixes (cdr (labeling-function get-ch-suffixes))) ; celelalte sufixe (cu eticheta scoasa)
                 (get-subtree (suffixes->st labeling-function get-new-suffixes alphabet)) ;apelul recursiv pentru crearea arborelui din noile sufixe
                 (get-branch (cons get-label get-subtree))
                 ) ; se leaga eticheta si se apeleaza recursiv pentru a se adauga etichetele urmatoare
                 (cons get-branch st)
           )
       )
   ) ; primul cons creeaza branchul, al doilea il leaga la arbore

  (foldr (lambda (ch st) (process-alphabet ch st)) '() alphabet)
  ; se face concatenarea (functia din foldr o sa intoarca in final branchurile pentru fiecare litera valida)
 )


; TODO 6
; Această sarcină constă în implementarea a trei funcții:
; text->st, text->ast, text->cst, unde text->ast și text->cst
; trebuie obținute ca aplicații parțiale ale lui text->st.
; În acest scop, funcția text->st trebuie să fie curry.

; a) Implementați funcția text->st care primește un text
; (listă de caractere) și o funcție de etichetare și întoarce
; arborele de sufixe corespunzând acestui text cu această
; metodă de etichetare.
; Pași:
; - obținem sufixele textului la care adăugăm marcajul final $
; - obținem alfabetul sortat asociat textului prin utilizarea
;   funcțiilor de bibliotecă sort, remove-duplicates, char<?
;   (inclusiv caracterul $)
; - apelăm corespunzător funcția suffixes->st
; Vă veți defini singuri parametrii funcției text->st și ordinea 
; acestora, dar funcția trebuie să fie curry într-un mod care 
; facilitează derivarea funcțiilor text->ast și text->cst de 
; mai jos prin aplicație parțială a funcției text->st.
; Obs: Din acest motiv, checker-ul testează doar funcțiile
; text->ast și text->cst.


(define text->st
  (lambda (text)
    (lambda (labeling-func)
      (let* ((text-with-$ (append text (list #\$))) ;textul cu $
             (get-alphabet (sort (remove-duplicates text-with-$ char=?) char<?))) ; literele din text sortate crescator
        (suffixes->st labeling-func (get-suffixes text-with-$) get-alphabet)))) ; arborele
  )
; b) Din funcția text->st derivați funcția text->ast care
; primește un text (listă de caractere) și întoarce AST-ul
; asociat textului.
(define (text->ast text)
  ((text->st text) ast-func)
 )
; c) Din funcția text->st derivați funcția text->cst care
; primește un text (listă de caractere) și întoarce CST-ul
; asociat textului.
(define (text->cst text)
  ((text->st text) cst-func)
 )
