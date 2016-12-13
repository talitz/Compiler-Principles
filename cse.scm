(define cse
    (lambda (expr)
       (let* ((replaced-mems (box (list)))
              (res-expr (cse-helper expr replaced-mems)))
          (cond ((eq? (length (unbox replaced-mems)) 0) res-expr)
                ((eq? (length (unbox replaced-mems)) 1) `(let (,(unbox replaced-mems)) ,res-expr))
                (else `(let* (,(unbox replaced-mems)) ,res-expr))))))

(define cse-helper
   (lambda (expr replaced-mems)
       (cond ((null? expr) expr)
             ((not (list? expr)) expr)
             (else
                 (let* ((x (cse-helper (car expr) replaced-mems))
                        (rec-res (cse-helper (cdr expr) replaced-mems))
                        (new-x (replace-by-vars x (unbox replaced-mems)))
                        (new-rec (cons new-x rec-res)))
                    (cse-x-handler new-x new-rec replaced-mems))))))

(define cse-x-handler
   (lambda (x expr replaced-mems)
       (cond ((null? x) expr)
             ((const? x) expr)
             ((member-rec? x (cdr expr)) (replace-mem x expr replaced-mems))
             (else (cse-x-handler (cdr x) (cse-x-handler (car x) expr replaced-mems) replaced-mems)))))

(define const? (lambda (x) (not (list? x))))

(define replace-rec
    (lambda (source target replacement)
        (if (eq? source target)
            replacement
            (if (or (null? source) (const? source))
                source
                (if (equal? target (car source))
                    (cons replacement (replace-rec (cdr source) target replacement))
                    (if (list? (car source))
                            (cons (replace-rec (car source) target replacement) (replace-rec (cdr source) target replacement))
                            (cons (car source) (replace-rec (cdr source) target replacement))))))))

(define replace-by-vars
    (lambda (x replaced-mems)
        (if (null? replaced-mems)
            x
            (let ((new-x (replace-rec x (cadar replaced-mems) (caar replaced-mems))))
                (replace-by-vars new-x (cdr replaced-mems))))))

(define replace-mem
     (lambda (elem expr replaced-mems)
         (let* ((name (gensym)))
            (set-box! replaced-mems (cons (list name elem) (unbox replaced-mems)))
            (replace-rec expr elem name))))

(define member-rec?
      (lambda(elem list)
               (cond ((null? list) #f)
                     ((equal? (car list) elem) #t)
                     ((list? (car list)) (or (member-rec? elem (car list)) (member-rec? elem (cdr list))))
                     (else (member-rec? elem (cdr list))))))
