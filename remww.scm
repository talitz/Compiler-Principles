(define remww
    (lambda (inst-list)
        (reverse (remww-helper (reverse inst-list) '()))))

(define remww-helper
    (lambda (reversed-inst-list acc)
        (if (null? reversed-inst-list)
            reversed-inst-list
            (let ((inst (car reversed-inst-list)))
                 (if (redundant-instruction? inst acc)
                      (remww-helper (cdr reversed-inst-list) acc)
                      (cons inst (remww-helper (cdr reversed-inst-list) (cons (car reversed-inst-list) acc))))))))

(define redundant-instruction?
    (lambda (inst inst-list)
        (let ((write-vars (caddr inst)))
           (if (null? write-vars)
               #t
               (redundant-instruction-helper write-vars inst-list)))))

(define redundant-instruction-helper
    (lambda (write-vars inst-list)
          (if (null? write-vars)
               #t
               (let* ((var (car write-vars))
                      (rest-list (find-first-read var inst-list '())))
                    (if (has-write? var rest-list)
                         (redundant-instruction-helper (cdr write-vars) inst-list)
                         #f)))))

(define find-first-read
    (lambda (var inst-list acc)
        (cond ((null? inst-list) acc)
              ((member var (cadr (car inst-list))) acc)
              (else (find-first-read var (cdr inst-list) (append acc (list (car inst-list))))))))

(define has-write?
    (lambda (var inst-list)
        (> (length (filter (lambda (x) (member var (caddr x))) inst-list)) 0)))