(include "src/storage.scm")

(define-record storage-accessor reader writer)

(define (path->storage-accessor path)
  (make-storage-accessor (make-directory-reader path)
                         (make-file-writer      path)))

(define (get-storage-hash-sets storage-accessors)
  (map (lambda (accessor)
         (cons accessor
               (tree->hashes (get-storage-tree (storage-accessor-reader accessor)))))
       storage-accessors))

(define (get-superset-of-storage-hash-sets storage-hash-sets)
  (fold (lambda (a b) (lset-union equal? a b))
        (list)
        (map cdr storage-hash-sets)))

(define (get-storage-missing-hash-sets storage-hash-sets
                                       superset-of-storage-hash-sets)
  (map (lambda (storage-hash-set)
         (cons (car storage-hash-set)
               (lset-difference equal?
                                superset-of-storage-hash-sets
                                (cdr storage-hash-set))))
       storage-hash-sets))

(define (get-superset-storage-accessor-map superset-of-storage-hash-sets
                                           storage-hash-sets)
  (map (lambda (hash)
         (let ((storage-hash-set (find (lambda (storage-hash-set)
                                         (list? (member hash (cdr storage-hash-set))))
                                       storage-hash-sets)))
           (cons hash
                 (car storage-hash-set))))
       superset-of-storage-hash-sets))

(define (synchronize paths)
  (let* ((storage-accessors             (map path->storage-accessor paths))
         (storage-hash-sets             (get-storage-hash-sets storage-accessors))
         (superset-of-storage-hash-sets (get-superset-of-storage-hash-sets
                                          storage-hash-sets))
         (storage-missing-hash-sets     (get-storage-missing-hash-sets
                                          storage-hash-sets
                                          superset-of-storage-hash-sets))
         (superset-storage-accessor-map (get-superset-storage-accessor-map
                                          superset-of-storage-hash-sets
                                          storage-hash-sets)))
    (for-each
      (lambda (storage-missing-hash-set)
        (let* ((target-storage-accessor  (car storage-missing-hash-set))
               (target-directory-reader  (storage-accessor-reader target-storage-accessor))
               (target-identifier-reader (make-identifier-reader target-directory-reader))
               (target-file-writer       (storage-accessor-writer target-storage-accessor))
               (target-blob-writer       (make-blob-writer target-file-writer
                                                           target-identifier-reader)))
          (for-each
            (lambda (missing-hash)
              (let* ((source-storage-accessor  (cdr (assoc missing-hash
                                                           superset-storage-accessor-map)))
                     (source-directory-reader  (storage-accessor-reader source-storage-accessor))
                     (source-identifier-reader (make-identifier-reader source-directory-reader))
                     (source-blob-reader       (make-blob-reader source-identifier-reader)))
                (target-blob-writer
                  (read-all (source-directory-reader (source-blob-reader missing-hash))))))
            (cdr storage-missing-hash-set))))
      storage-missing-hash-sets)))
