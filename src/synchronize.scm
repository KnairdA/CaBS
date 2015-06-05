(include "src/storage.scm")
(include "src/utility.scm")

(define-record storage-accessor reader writer)
(define-record storage-missing-hash-set accessor digest content)

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
         (make-storage-missing-hash-set
           (car storage-hash-set)
           (get-digest (cdr storage-hash-set))
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
  (let* ((storage-hash-sets             (get-storage-hash-sets (map path->storage-accessor paths)))
         (superset-of-storage-hash-sets (get-superset-of-storage-hash-sets storage-hash-sets))
         (superset-storage-accessor-map (make-map-accessor (get-superset-storage-accessor-map
                                                             superset-of-storage-hash-sets
                                                             storage-hash-sets))))
    (for-each
      (lambda (missing-set)
        (let* ((target-storage-accessor (storage-missing-hash-set-accessor missing-set))
               (target-directory-reader (storage-accessor-reader target-storage-accessor))
               (target-blob-writer      (make-blob-writer
                                          (storage-accessor-writer target-storage-accessor)
                                          target-directory-reader)))
          (print "Storage " (storage-missing-hash-set-digest missing-set))
          (for-each
            (lambda (missing-hash)
              (let* ((source-storage-accessor  (superset-storage-accessor-map missing-hash))
                     (source-directory-reader  (storage-accessor-reader source-storage-accessor))
                     (source-blob-reader       (make-blob-reader source-directory-reader)))
                (print "      + " missing-hash)
                (target-blob-writer
                  (read-all (source-directory-reader (source-blob-reader missing-hash))))))
            (storage-missing-hash-set-content missing-set))))
      (get-storage-missing-hash-sets storage-hash-sets
                                     superset-of-storage-hash-sets))
    (print "Consent " (get-digest superset-of-storage-hash-sets))))
