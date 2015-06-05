(use srfi-1
     utils
     posix
     filepath
     message-digest
     sha2)

(define hash-length   (* 2 (message-digest-primitive-digest-length (sha256-primitive))))
(define branch-length 2)
(define node-length   (- hash-length branch-length))

;; hash

(define (blob->hash blob)
  (message-digest-object (sha256-primitive) blob))

;; file io

(define (make-file-writer context)
  (lambda (path content)
    (let ((full-path (filepath:combine context path)))
      (create-directory (filepath:drop-file-name full-path) #t)
      (let ((fileno (file-open full-path (+ open/wronly open/creat))))
        (file-write fileno content)
        (file-close fileno)))))

(define (make-directory-reader context)
  (lambda (path)
    (let ((full-path (filepath:combine context path)))
      (cond ((regular-file? full-path) full-path)
            ((directory?    full-path) (directory full-path))
            (else                      #f)))))

;; identifier

(define-record identifier branch node)

(define (hash->identifier hash)
  (let ((hash (string->list hash)))
    (make-identifier (list->string (take hash branch-length))
                     (list->string (drop hash branch-length)))))

(define (identifier->path identifier)
  (filepath:combine (identifier-branch identifier)
                    (identifier-node identifier)))

(define (make-identifier-reader directory-reader)
  (lambda (identifier)
    (let* ((identifier-path (identifier->path identifier))
           (actual-path     (directory-reader identifier-path)))
      (if (string? actual-path)
        identifier-path
        #f))))

;; blob

(define (make-blob-reader directory-reader)
  (lambda (hash)
    ((make-identifier-reader directory-reader) (hash->identifier hash))))

(define (make-blob-writer file-writer directory-reader)
  (lambda (source)
    (let* ((identifier-reader (make-identifier-reader directory-reader))
           (hash              (blob->hash source))
           (identifier        (hash->identifier hash)))
      (if (not (identifier-reader identifier))
        (file-writer (identifier->path identifier)
                     source))
      (identifier-reader identifier))))

;; storage

(define (get-storage-tree directory-reader)
  (let ((is-branch? (lambda (candidate)
                      (and (list? (directory-reader candidate))
                           (= branch-length (string-length candidate)))))
        (is-node?   (lambda (candidate)
                      (and (string? candidate)
                           (= node-length (string-length candidate))))))
    (map (lambda (branch) (list (car branch)
                                (filter is-node? (cdr branch))))
         (map (lambda (branch) (cons branch (directory-reader branch)))
              (directory-reader ".")))))

(define (tree->hashes tree)
  (concatenate (map (lambda (branch) (map (lambda (node) (conc (car branch) node))
                                          (second branch)))
                    tree)))

(define (get-invalid-nodes directory-reader blob-reader)
  (remove (lambda (hash)
            (equal? (blob->hash (read-all (directory-reader (blob-reader hash))))
                    hash))
          (tree->hashes (get-storage-tree directory-reader))))

(define (get-digest hashes)
  (blob->hash (fold conc
                    (string)
                    (sort hashes string<?))))

(define (get-storage-digest directory-reader)
  (get-digest (tree->hashes (get-storage-tree directory-reader))))
