;Nome             Cognome      Matricola
;Gabriele         Carrivale    872488

(defun check-port (port)
  (cond ((null port) 80)
        (T (parse-integer (coerce port '(string))))))

(defun check-field (field)
  (cond ((or (null field) (eq field 'no)) nil)
        (T (coerce field '(string)))))

(defun uri-parse (stringa)
  (let ((lista (coerce stringa 'list)))
    (cond ((null lista) nil)
          ((not(id (first lista))) nil)
          ((let ((scheme (scheme lista nil)))
             (if (null (first scheme)) nil 
               (if (special-scheme  (first scheme))

                   (if (equal (first scheme) '(#\m #\a #\i #\l #\t #\o))
                       (let ((userinfo (mailto (rest scheme) nil)))
                         (if (eq userinfo 'error) nil
                           (let ((host (mailto-ip (rest userinfo) 0 nil)))
                             (if (eq host 'error) nil
                               (let ((uri (list (check-field (first scheme)) 
                                                (check-field (first userinfo)) 
                                                (check-field (first host)) 
                                                80 
                                                nil 
                                                nil 
                                                nil))) uri)))))

                     (if (equal (first scheme) '(#\n #\e #\w #\s))
                         (let ((host (news (rest scheme))))
                           (if (eq host 'error) nil                   
                             (let ((uri (list (check-field (first scheme)) 
                                              nil 
                                              (check-field (first host)) 
                                              80 
                                              nil 
                                              nil 
                                              nil))) uri))) 

                       (if (or (equal (first scheme) '(#\t #\e #\l)) 
                               (equal (first scheme) '(#\f #\a #\x)))
                           (let ((userinfo (tel-fax (rest scheme) nil)))
                             (if (eq userinfo 'error) nil                   
                               (let ((uri (list (check-field (first scheme)) 
                                                (check-field (first userinfo))  
                                                nil
                                                80 
                                                nil 
                                                nil 
                                                nil))) uri))))))            

                 (let ((userinfo (authorithy (rest scheme) scheme nil)))
                   (if (eq userinfo 'error) nil
                     (let ((host (ip (rest userinfo) 0 nil (first userinfo))))
                       (if (and 
                            (null (first host)) 
                            (not (eq (first userinfo) 'no))) nil 
                         (let ((port (port (rest host) nil scheme)))
                           (if (eq port 'error) nil
                             (let ((path 
                                    (path (rest port) nil (first scheme))))
                               (if (eq path 'error) nil 
                                 (let ((query (query (rest path) nil)))
                                   (if (eq query 'error) nil 
                                     (let ((fragment (fragment (rest query))))
                                       (if (eq fragment 'error) nil
                                         (let 
                                             ((uri 
                                               (list 
                                                (check-field (first scheme)) 
                                                (check-field (first userinfo)) 
                                                (check-field (first host)) 
                                                (check-port (first port)) 
                                                (check-field (first path)) 
                                                (check-field (first query)) 
                                                (check-field fragment)))) uri
))))))))))))))))))))

(defun scheme (lista acc)
  (cond ((null lista) nil)
        ((char= (first lista) #\:) (cons acc (rest lista))) 
        ((not(id (first lista))) nil)
        (T (scheme (rest lista) (append acc (list (car lista)))))))

(defun special-scheme (scheme)
  (cond ((null scheme) nil)
        ((or (equal scheme '(#\m #\a #\i #\l #\t #\o))
        (equal scheme '(#\n #\e #\w #\s))
        (or (equal scheme '(#\t #\e #\l)) 
             (equal scheme '(#\f #\a #\x)))) T)))

(defun authorithy (lista scheme acc) 
  (cond ((null lista) (cons 'no lista))
        ((char= (first lista) #\/) (secondo-slash (rest lista) scheme acc))
        (T (cons 'no lista))))

(defun secondo-slash (lista scheme acc)
  (cond  ((null lista) (cons 'no lista))
         ((char= (first lista) #\/) (scelta (rest lista) acc))
         (T (cons 'no lista))))

(defun scelta (lista acc) 
  (cond ((null lista) nil)
        ((not (null (member #\@ lista))) (userinfo lista acc))
        (T (cons acc lista))))

(defun userinfo (lista acc)
  (cond ((null lista) nil)
        ((and (char= (first lista) #\@) 
              (null acc)) 'error)
        ((and (char= (first lista) #\@) 
              (not(null acc))) (cons acc (rest lista)))
        ((not(id (first lista))) 'error)
        (T (userinfo (rest lista) (append acc (list (car lista)))))))

(defun host (lista acc)
  (cond ((and (null lista) (null acc)) nil)
        ((null lista) (cons acc lista))
        ((char= (first lista) #\:) (cons acc lista))
        ((char= (first lista) #\/) (cons acc (rest lista)))
        ((and (char= (first lista) #\.) (not (null acc)))  
         (host2 (rest lista) (append acc '(#\.))))
        ((char= (first lista) #\?) (cons acc lista))
        ((char= (first lista) #\#) (cons acc lista))
        ((not (idH (first lista))) nil)
        (T (host (rest lista) (append acc (list (car lista)))))))

(defun host2 (lista acc)
  (cond ((null lista) nil)
        ((not (idH (first lista))) nil)
        (T (host (rest lista) (append acc (list (car lista)))))))

(defun ip (lista count acc &optional esito)
  (cond ((and (null lista) (null acc) (eq esito 'no)) (cons acc lista))
        ((and (null lista) (null acc)) nil)
        ((and (null acc) (eq esito 'no)) (cons acc lista))
        ((null lista) nil)
        ((> count 4) (host lista acc))
        ((< (list-to-number lista) 255) 
         (ip-punto (rest (rest (rest lista))) 
                   (+ count 1) 
                   (append acc
                           (list (first lista) 
                                 (second lista) 
                                 (third lista)))))
        (T (host lista acc))))

(defun ip-punto (lista count acc)
  (cond ((and (null lista) (= count 4)) (cons acc lista))
        ((null lista) (cons acc lista))
        ((char/= (first lista) #\.) (host lista acc))
        ((and (not (null (rest lista))) (not (idH (second lista)))) nil)
        (T (ip (rest lista) count (append acc (list (first lista)))))))

(defun list-to-number (lista)
  (if (and (check-number (first lista)) 
           (check-number (second lista)) 
           (check-number (third lista)))
      (parse-integer
       (coerce (list (first lista) (second lista) (third lista)) '(string)))
    256))
(defun check-number (nchar)
  (cond ((null nchar) nil)
        ((numberp (digit-char-p nchar)) T)
        (T nil)))

(defun port (lista acc scheme)
  (cond ((special-scheme scheme) nil)
        ((null lista) (cons acc lista))
        ((char= (first lista) #\:) (port-num (rest lista) acc))
        (T (cons acc lista))))

(defun port-num (lista acc)
  (cond ((and (null lista) (null acc)) 'error)
        ((null lista) (cons acc lista))
        ((and (char= (first lista) #\/) (null acc)) 'error)
        ((char= (first lista) #\/) (cons acc (rest lista)))
        ((not(null (digit-char-p (first lista))))  
         (port-num (rest lista) (append acc (list (car lista)))))
        ((and (char= (first lista) #\?) (not (null acc))) (cons acc lista))
        ((and (char= (first lista) #\#) (not (null acc))) (cons acc lista))
        (T 'error)))

(defun path (lista acc &optional scheme)
  (cond ((null lista) (cons acc lista))
        ((equal scheme '(#\z #\o #\s)) (zos lista acc))
        ((id (first lista))
         (path-field (rest lista) (append acc (list (first lista)))))
        ((char= (first lista) #\?) (cons acc lista))
        ((char= (first lista) #\#) (cons acc lista))
        (T 'error)))

(defun path-field (lista acc)
  (cond ((null lista) (cons acc lista))
        ((char= (first lista) #\?) (cons acc lista))
        ((char= (first lista) #\#) (cons acc lista))
        ((and (char= (first lista) #\/) (not (equal (last acc) '(#\/)))) 
         (path-field (rest lista) (append acc '(#\/))))
        ((not(id (first lista))) 'error)
        (T (path-field (rest lista) (append acc (list (first lista)))))))

(defun query (lista acc)
  (cond ((null lista) (cons acc lista))
        ((char= (first lista) #\?) (query-field (rest lista) acc))
        (T (cons acc lista))))

(defun query-field (lista acc)
  (cond ((and (null lista) (null acc)) 'error)
        ((null lista) (cons acc lista))
        ((char= (first lista) #\#) (cons acc lista))
        ((not(qry (first lista))) 'error)
        (T (query-field (rest lista) (append acc (list (first lista)))))))

(defun fragment (lista)
  (cond ((null lista) nil)
        ((char= (first lista) #\#) (fragment-field (rest lista)))))

(defun fragment-field (lista)
  (cond ((null lista) 'error)
        (T lista)))

(defun mailto (lista acc)
  (cond ((null lista) (cons acc lista))
        ((and (char= (first lista) #\@) (null acc)) 'error)
        ((and (char= (first lista) #\@) (not(null acc))) (cons acc lista))
        ((not(id (first lista))) 'error)
        (T (mailto (rest lista) (append acc (list (first lista)))))))

(defun mailto-ip (lista count acc)
  (cond ((null lista) (cons acc lista))
        ((char= (first lista) #\@) (mailto-ip1 (rest lista) count acc))
        (T (cons acc lista))))

(defun mailto-ip1 (lista count acc)
  (cond ((and (null lista) (null acc)) 'error)
        ((null lista) 'error)
        ((> count 4) (mailto-host lista acc))
        ((< (list-to-number lista) 255)
         (mailto-ip-punto 
          (rest (rest (rest lista))) 
          (+ count 1) 
          (append acc
                  (list 
                   (first lista) 
                   (second lista) 
                   (third lista)))))
        (T (mailto-host lista acc))))

(defun mailto-ip-punto (lista count acc)
  (cond ((and (null lista) (= count 4)) (cons acc lista))
        ((null lista) (cons acc lista))
        ((char/= (first lista) #\.) (mailto-host lista acc))
        ((and (not (null (rest lista))) (not (idH (second lista)))) 'error)
        (T (mailto-ip1 (rest lista) count (append acc (list (first lista)))))))

(defun mailto-host (lista acc)
  (cond ((null lista) (cons acc lista))
        ((and (null lista) (null acc)) 'error)
        ((and (char= (first lista) #\.) (not (null acc))) 
         (mailto-host2 (rest lista) (append acc '(#\.))))
        ((not (idH (first lista))) 'error)
        (T (mailto-host (rest lista) (append acc (list (first lista)))))))

(defun mailto-host2 (lista acc)
  (cond ((null lista) 'error)
        ((not (idH (first lista))) 'error)
        (T (mailto-host (rest lista) (append acc (list (first lista)))))))

(defun news (lista)
  (if (null lista) lista
    (news-ip lista 0 nil)))

(defun news-ip(lista count acc)
  (cond ((and (null lista) (char= (first (last acc)) #\.)) 'error)
        ((null lista) (cons acc lista))
        ((> count 4) (news-host lista acc))
        ((< (list-to-number lista) 255)     
         (news-ip-punto 
          (rest (rest (rest lista))) 
          (+ count 1) 
          (append acc
                  (list (first lista) 
                        (second lista) 
                        (third lista)))))
        (T (news-host lista acc))))

(defun news-ip-punto (lista count acc)
  (cond ((and (null lista) (= count 4)) (cons acc lista))
        ((null lista) (cons acc lista))
        ((char/= (first lista) #\.) (news-host lista acc))
        ((and (not (null (rest lista))) (not (idH (second lista)))) 'error)
        (T (news-ip (rest lista) count (append acc
                                               (list (first lista)))))))

(defun news-host (lista acc)
  (cond ((null lista) (cons acc lista))
        ((and (null lista) (null acc)) 'error)
        ((char= (first lista) #\.) 
         (news-host2 (rest lista) (append acc '(#\.))))
        ((not (idH (first lista))) 'error)
        (T (news-host (rest lista) (append acc (list (first lista)))))))

(defun news-host2 (lista acc)
  (cond ((null lista) 'error)
        ((not (idH (first lista))) 'error)
        (T (news-host (rest lista) (append acc (list (first lista)))))))

(defun tel-fax (lista acc)
  (cond ((null lista) (cons acc lista))
        ((not(id (first lista))) 'error)
        (T (tel-fax (rest lista) (append acc (list (first lista)))))))

(defun zos (lista acc)
  (cond ((null lista) (cons acc lista))
        ((or (char= (first lista) #\?) 
             (char= (first lista) #\#)) (cons acc lista))
        ((not (alpha-char-p (first lista))) 'error)
        (T (id44 lista 1 acc))))

(defun id44 (lista count acc)
  (cond ((and (null lista) (char= (first (last acc)) #\.)) 'error)
        ((null lista) (cons acc lista))
        ((> count 44) 'error)
        ((char= (first lista) #\() 
         (id8 (rest lista) 0 (append acc (list (first lista))))) 
        ((char= (first lista) #\.)  
         (id44-point (rest lista) 
                     (+ 1 count) 
                     (append acc (list (first lista)))))
        ((or (char= (first lista) #\?) 
             (char= (first lista) #\#)) (cons acc lista))
        ((not (alphanumericp (first lista))) 'error)
        (T (id44 (rest lista) 
                 (+ count 1) 
                 (append acc (list (first lista)))))))

(defun id44-point (lista count acc)
  (cond ((null lista) 'error)
        ((> count 43) 'error)
        ((or (char= (first lista) #\() 
             (char= (first lista) #\?) 
             (char= (first lista) #\#)) 'error)
        (T (id44 (rest lista) 
                 (+ count 1) 
                 (append acc (list (first lista)))))))

(defun id8 (lista count acc)
  (cond ((and (null lista) (char/= (first (last acc)) #\)))  'error)
        ((null lista) (cons acc lista))
        ((and (= count 0) (not (alpha-char-p (first lista)))) 'error)
        ((> count 8) 'error)
        ((and (= count 8) (char/= (first lista) #\))) 'error)
        ((and (>= count 1) (char= (first lista) #\))) 
         (cons (append acc (list (car lista))) (rest lista)))
        ((not (alphanumericp (first lista))) 'error)
        (T (id8 (rest lista) 
                (+ count 1) 
                (append acc (list (first lista)))))))

(defun uri-display (qualcosa &optional stream) 
  (if (null stream)
      (appoggio qualcosa T)
    (appoggio qualcosa stream))T)

(defun appoggio (qualcosa stream)
  (format t "Scheme:     ~A
Userinfo:   ~A
Host:       ~A
Port:       ~A
Path:       ~A
Query:      ~A
Fragment:   ~A" 
(uri-scheme qualcosa) 
(uri-userinfo qualcosa) 
(uri-host qualcosa) 
(uri-port qualcosa) 
(uri-path qualcosa) 
(uri-query qualcosa) 
(uri-fragment qualcosa)))


(defun uri-scheme (uri) (first uri))

(defun uri-userinfo (uri) (second uri))

(defun uri-host (uri) (third uri))

(defun uri-port (uri) (fourth uri))

(defun uri-path (uri) (fifth uri))

(defun uri-query (uri) (sixth uri))

(defun uri-fragment (uri) (seventh uri))

(defun id (next) 
  (if (and (char/= next #\/)
           (char/= next #\?)
           (char/= next #\#)
           (char/= next #\@) 
           (char/= next #\:)) T))

(defun idH (next) 
  (if (and (char/= next #\.)
           (char/= next #\/)
           (char/= next #\?)
           (char/= next #\#)
           (char/= next #\@) 
           (char/= next #\:)) T))

(defun qry (next) 
    (if (char/= next #\#) T))

(defun digit (next) 
    (if (digit-char-p next) T))