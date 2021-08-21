#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(define C5 (make-counter 5 12 8 (queue '((remus . 6) (vivi . 4)) '() 2 0)))

(define (update f counters index)
  (update-help f counters index '()))

(define (update-help f counter index acc)
  (if (null? counter)
      acc
      (if (eq? index (counter-index (car counter)))
          (update-help f (cdr counter) index (append acc (list(f(car counter)))))
          (update-help f (cdr counter) index (append acc (list(car counter)))))))

(define tt+
  (λ (minutes)
    (λ (C)
      (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-et C) (counter-queue C)))))

(define et+
  (λ (minutes)
    (λ (C)
      (make-counter (counter-index C) (+ (counter-tt C) minutes) (+ (counter-et C) minutes) (counter-queue C)))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (counter-queue C))
        (make-counter (counter-index C) (counter-tt ((tt+ items) C)) (counter-et ((et+ items) C)) (enqueue (cons name items) (counter-queue C)))
        (make-counter (counter-index C) (counter-tt ((tt+ items) C)) (counter-et C) (enqueue (cons name items) (counter-queue C)))
     )))

(define rezolvare
  (λ (proc)
    (λ (counters)
      (if (equal? proc min-tt)
          (proc counters)
          (proc counters)))))
          
(define min-tt
  (λ (counters)
    (foldl (λ (element minim)
             (if (< (counter-tt element) (cdr minim))
                 (cons (counter-index element) (counter-tt element))
                 minim))
           (cons (counter-index (car counters)) (counter-tt (car counters))) (cdr counters))))
(define min-et
  (λ (counters)
    (foldl (λ (element minim)
             (if (or (and (< (counter-et element) (cdr minim)) (not(queue-empty? (counter-queue element)))) (and (eq? (car minim) -1) (not(queue-empty? (counter-queue element)))))
                         (cons (counter-index element) (counter-et element))
                         minim ))
                   (cons -1 -1) counters)))

(define (remove-first-from-counter C)   ; testată de checker
  (make-counter (counter-index C) (- (counter-tt C) (counter-et C)) (if (queue-empty? (dequeue(counter-queue C))) 0 (cdr (top (dequeue (counter-queue C))))) (dequeue (counter-queue C))))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (if (and (equal? (counter-et C) 0) (queue-empty? (counter-queue C)))
        C
        (if (and (> minutes (counter-et C)) (queue-empty? (counter-queue C)))
            (make-counter (counter-index C) (- (counter-tt C) (counter-et C)) 0 (counter-queue C))
            (make-counter (counter-index C) (- (counter-tt C) minutes) (- (counter-et C) minutes) (counter-queue C))))))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))
        
(define (serve-helper requests fast-counters slow-counters acc)
  (if (null? requests)
      (cons acc (append fast-counters slow-counters))
      (match (car requests)
        [(list 'ensure average)(if (<= (/ (foldl (λ (element acc) (+ acc (counter-tt element))) 0 (append fast-counters slow-counters)) (length (append fast-counters slow-counters))) average)
                                   (serve-helper (cdr requests) fast-counters slow-counters acc)
                                   (serve-helper requests fast-counters (append slow-counters (list (empty-counter (+ (length (append fast-counters slow-counters)) 1))))acc))]
        [(list 'delay index minutes) (serve-helper (cdr requests) (update (et+ minutes) fast-counters index) (update (et+ minutes) slow-counters index) acc)]
        [(list name items) (if (<= items ITEMS)
                              (serve-helper (cdr requests) (update (add-to-counter name items) fast-counters (car (min-tt(append fast-counters slow-counters))))
                                    (update (add-to-counter name items) slow-counters (car (min-tt(append fast-counters slow-counters)))) acc)
                              (serve-helper (cdr requests) fast-counters (update (add-to-counter name items) slow-counters (car (min-tt slow-counters))) acc))]
        [number (serve-helper (cdr requests)
                              (foldl (λ (element acc)(append acc (list ((modify-counter number) element))))'() fast-counters)
                              (foldl (λ (element acc)(append acc (list ((modify-counter number) element))))'() slow-counters)
                              (append acc (((passing-time number) (append fast-counters slow-counters)) '())))])))


(define (modify-counter minutes)
  (λ (C)
    (if (and (>= minutes (counter-et C)) (not(queue-empty? (counter-queue C)))) ((modify-counter (- minutes (counter-et C))) (remove-first-from-counter C))
        ((pass-time-through-counter minutes) C) )))
(define (get-ones-out minutes)
  (λ (C)
    (λ (acc)
      (if (and (>= minutes (counter-et C)) (not (queue-empty? (counter-queue C))))
          (((get-ones-out (- minutes (counter-et C))) (remove-first-from-counter C)) (append acc (list(cons (counter-index C) (car(top (counter-queue C)))))))
           acc))))
(define (passing-time minutes)
  (λ (counters)
    (λ (acc)
      (if (equal? minutes 0) acc
          (((passing-time (- minutes 1))
            (foldl (λ (element acc)(append acc (list ((modify-counter 1) element))))'() counters))
           (append acc (foldl (λ (element acc)
                                (if (empty? (((get-ones-out 1) element) '()))
                                    acc
                                    (append acc (((get-ones-out 1) element) '())) ))'() counters))) ))))