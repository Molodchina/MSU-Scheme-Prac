; заготовка "Доктора". Сентябрь 2024
; В учебных целях используется базовая версия Scheme
#lang scheme/base

; Подключаем Racket-библиотеки для векторов и списков, на всякий случай
(require racket/vector)
; (require racket/lists)

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name (vector)))

; основная функция, запускающая "Доктора"
; параметр stop-word -- стоп-слово для завершения работы
; параметр max-patients -- максимальное количество пациентов
(define (visit-doctor-v2 stop-word max-patients)
  (let loop ((patients-served 0))
    (if (>= patients-served max-patients)
        (println '(all patients have been served, time to go home))
        (let ((name (ask-patient-name)))
          (if (equal? name stop-word)
              (println '(time to go home))
              (begin
                (printf "Hello, ~a!\n" name)
                (print '(what seems to be the trouble?))
                (doctor-driver-loop-v2 name (vector))
                (loop (add1 patients-served))))))))

; функция для запроса имени следующего пациента
(define (ask-patient-name)
  (begin
    (println '(next!))
    (println '(who are you?))
    (print '**)
    (car (read))))

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
; параметр history -- история реплик
(define (doctor-driver-loop-v2 name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (println '(see you next week)))
            (else
             (print (reply user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
             (let ((new-history (update-history history user-response)))
               (doctor-driver-loop-v2 name new-history)
             )
            )
       )
      )
)

; обновление истории реплик
(define (update-history history user-response)
 (if (vector-member user-response history)
    history         ; если ответ в векторе, ничего не делать
    (vector-append  ; иначе добавить новый ответ в начало
     (vector user-response)  
     (if (< (vector-length history) 7)   ; если длина вектора уже 7, отбрасываем последний элемент
         history
         (vector-take history 6)
     )
    )
 )
)

; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply user-response history)
      (case (random (if (has-keywords? user-response) -1 0) (if (vector-empty? history) 2 3)) ; с равной вероятностью выбирается один из трех способов построения ответа
          ((-1) (keyword-answer user-response)) ; 4й способ
          ((0) (hedge-answer))  ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ
          ((2) (history-answer history)) ; 3й способ       
       )
)

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
       (pick-random-vector '#((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (tell me more about that)
                              (everything will get better, go on)
                              (I see, please continue))
         )
)

; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr)))
)

; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (it seems you think that)
                                       (you appear to feel that)
                                       (you seem to feel that))
                )
                (change-person user-response)
        )
 )

; 3й способ генерации ответной реплики -- использование истории реплик
(define (history-answer history)
  (let ((random-response (vector-ref history (random 0 (vector-length history)))))
    (append '(earlier you said that) (change-person random-response))))

; замена лица во фразе
(define (change-person phrase)
        (many-replace-v3
		'((am are)
        (are am)
        (i you)
        (I you)
        (me you)
        (mine yours)
        (my your)
        (myself yourself)
        (you I)
        (your my)
        (yours mine)
        (yourself myself)
        (we you)
        (us you)
        (our your)
        (ours yours)
        (ourselves yourselves)
        (yourselves ourselves)
        (shall will))
                      phrase)
 )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
)

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs с использованием хвостовой рекурсии
(define (many-replace-v2 replacement-pairs lst)
  (let loop ((lst lst) (result '()))
    (cond ((null? lst) (reverse result))
          (else
           (let ((pat-rep (assoc (car lst) replacement-pairs)))
             (loop (cdr lst)
                   (cons (if pat-rep (cadr pat-rep) (car lst)) result)))))))

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs с использованием map
(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (x)
         (let ((pat-rep (assoc x replacement-pairs)))
           (if pat-rep (cadr pat-rep) x)))
       lst))

; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i 0) (result init))
   (if (= i length) result
    (loop (add1 i) (f i result (vector-ref vctr i)))))))
	
; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i (sub1 length)) (result init))
   (if (= i -1) result
    (loop (sub1 i) (f i result (vector-ref vctr i)))))))

; структура данных для ключевых слов и шаблонов ответов
(define keywords-structure '#(
  #( ; начало данных 1й группы
    #(depressed suicide exams university) ; вектор ключевых слов 1й группы
    #( ; вектор шаблонов для составления ответных реплик 1й группы
	  (when you feel depressed, go out for ice cream) ; 1й шаблон 1й группы -- список символов
      (depression is a disease that can be treated)
      (have you talked to someone about your feelings?)
      (it's important to take care of your mental health)
	)
  ) ; завершение данных 1й группы
  #( ; начало данных 2й группы
    #(mother father parents brother sister uncle aunt grandma grandpa)
    #(
	  (tell me more about your *. I want to know all about your *)
      (why do you feel that way about your * ?)
      (how is your relationship with your * ?)
      (do you spend enough time with your * ?)
	)
  )
  #(
    #(university scheme lections)
	#(
	  (your education is important)
	  (how much time do you spend on your studies ?)
      (do you enjoy studying * ?)
      (what are your favorite subjects in * ?)
	)
  )
  #(
    #(friends social life hobbies)
	#(
	  (how often do you see your friends?)
	  (what do you like to do in your free time?)
	  (do you have any hobbies?)
	  (how do you balance your social life and studies?)
	)
  )
  #(
    #(work job career)
	#(
	  (how is your job going?)
	  (do you enjoy your work?)
	  (what are your career goals?)
	  (how do you manage work-life balance?)
	)
  )
))

; Получаем вектор всех ключевых слов из keywords-structure
(define keywords
  (vector-foldl
       (lambda (i result elem)
         (vector-append
             result
             (vector-filter-not
                 (lambda (word) (vector-member word result))
                 (vector-ref elem 0)
             )
         )
        )
       `#()
       keywords-structure))

; Получение списка ключевых слов, найденных в фразе
(define (find-keywords phrase)
  (let loop ((phrase phrase) (result `#()))
    (cond
      ((null? phrase) result)
      ((vector-member (car phrase) keywords)
       (loop (cdr phrase) (vector-append (vector (car phrase)) result)))
      (else (loop (cdr phrase) result))))) ; # TODO: filter / vector-foldl

; Проверка фразы на наличие ключевых слов
(define (has-keywords? user-response)
  (ormap
      (lambda (elem) (vector member elem keywords))
      user-response))

; 4 стратегий ответа на основе ключевых слов в реплике
(define (keyword-answer user-response)
  (let ((keywords (find-keywords user-response)))
    (if (vector-empty? keywords)
        (vector 'no-keywords-found)
        (let* ((selected-keyword (pick-random-vector keywords))
               (response (get-response-for-keyword selected-keyword)))
          (many-replace-v3 (list(list '* selected-keyword)) (car response))))))

; получение ответа доктора на основе ключевого слова
(define (get-response-for-keyword keyword)  
  (for/list ([group keywords-structure]
             #:when (vector-member keyword (vector-ref group 0)))
    (let ([responses (vector-ref group 1)])
      (pick-random-vector (vector-filter (lambda (response) (not (eq? response '()))) responses)))))
