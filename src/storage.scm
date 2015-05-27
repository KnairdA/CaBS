(use srfi-1
     utils
     posix
     message-digest
     sha2)

(define hash-length   (* 2 (message-digest-primitive-digest-length (sha256-primitive))))
(define branch-length 2)
(define node-length   (- hash-length branch-length))

;; hash io

(define (blob->hash blob)
  (message-digest-object (sha256-primitive) blob))

;; file io

(define (write-file path content)
  (let ((fileno (file-open path (+ open/wronly open/creat))))
    (file-write fileno content)
    (file-close fileno)))

(define (identifier-exists? branch node)
  (regular-file? (identifier->path branch node)))

;; blob identifier

(define (hash->identifier hash)
  (let ((hash (string->list hash)))
    (values (list->string (take hash branch-length))
            (list->string (drop hash branch-length)))))

(define (identifier->path branch node)
  (conc branch "/" node))

;; blob

(define (blob-exists? hash)
  (let-values (((branch node) (hash->identifier hash)))
    (identifier-exists? branch node)))

(define (get-blob hash)
  (let-values (((branch node) (hash->identifier hash)))
    (if (identifier-exists? branch node)
      (identifier->path branch node)
      #f)))

(define (add-blob source)
  (let ((hash (blob->hash source)))
    (let-values (((branch node) (hash->identifier hash)))
      (if (not (identifier-exists? branch node))
        (begin (create-directory branch)
               (write-file (identifier->path branch node)
                           source))))
    hash))

(define (remove-blob hash)
  (let-values (((branch node) (hash->identifier hash)))
    (if (identifier-exists? branch node)
      (delete-file (identifier->path branch node)))
    (identifier-exists? branch node)))

;; storage

(define (get-branches)
  (filter (lambda (candidate) (and (directory? candidate)
                                   (= branch-length (string-length candidate))))
          (directory)))

(define (get-nodes branch)
  (filter (lambda (candidate) (and (regular-file? (conc branch "/" candidate))
                                   (= node-length (string-length candidate))))
          (directory branch)))

(define (get-invalid-nodes branch)
  (remove (lambda (node)
            (equal? (blob->hash (read-all (conc branch "/" node)))
                    (conc branch node)))
          (get-nodes branch)))

(define (get-storage)
  (concatenate (map (lambda (branch) (map (lambda (node) (conc branch node))
                                          (get-nodes branch)))
                    (get-branches))))

(define (get-storage-digest)
  (blob->hash (fold conc
                    (string)
                    (sort (get-storage) string<?))))
