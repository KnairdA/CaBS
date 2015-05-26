(use srfi-1
     utils
     posix
     message-digest
     sha2)

(define branch-length 2)

;; hash io

(define (blob->hash blob)
  (message-digest-object (sha256-primitive) blob))

;; file io

(define (write-file path content)
  (let ((fileno (file-open path (+ open/wronly open/creat))))
    (file-write fileno content)
    (file-close fileno)))

(define (identifier-exists? branch node)
  (file-exists? (identifier->path branch node)))

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
