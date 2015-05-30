(include "src/utility.scm")
(include "src/storage.scm")
(include "src/synchronize.scm")

(define-record documentation arguments description)
(define-record command       name implementation documentation)

(define (perform-unrecognized-command command)
  (print "CaBS: '" command "' is not a CaBS command. See 'cabs help'.")
  (exit 1))

(define (perform-help)
  (let ((printer (make-kv-printer 0 30)))
    (for-each print (map (lambda (command)
                          (let ((documentation (command-documentation command)))
                            (printer (documentation-arguments   documentation)
                                     (documentation-description documentation))))
                         commands))))

(define (perform-add #!optional sources)
  (let ((blob-writer (make-blob-writer
                       (make-file-writer (current-directory))
                       (make-identifier-reader (make-directory-reader
                                                 (current-directory))))))
    (if (not sources)
      (print (blob-writer (read-all (current-input-port))))
      (for-each (lambda (source) (print (blob-writer (read-all source))))
                sources))))

(define (perform-get #!optional hashes)
  (if (not hashes)
    (exit 1)
    (let* ((blob-reader (make-blob-reader
                          (make-identifier-reader
                            (make-directory-reader (current-directory)))))
           (paths       (delete #f
                                (map blob-reader hashes))))
      (for-each print paths)
      (exit (boolean->exit (= (length paths)
                              (length hashes)))))))

(define (perform-check #!optional hashes)
  (if (not hashes)
    (exit 1)
    (let ((blob-reader (make-blob-reader
                         (make-identifier-reader
                           (make-directory-reader (current-directory))))))
      (exit (boolean->exit (every string? (map blob-reader hashes)))))))

(define (perform-ls-blobs)
  (for-each print
            (tree->hashes (get-storage-tree
                            (make-directory-reader (current-directory))))))

(define (perform-digest)
  (print (get-storage-digest (make-directory-reader (current-directory)))))

(define (perform-validate)
  (let* ((directory-reader  (make-directory-reader (current-directory)))
         (identifier-reader (make-identifier-reader directory-reader))
         (blob-reader       (make-blob-reader       identifier-reader))
         (invalid-nodes     (get-invalid-nodes directory-reader blob-reader)))
    (if (null? invalid-nodes)
      (exit 0)
      (begin (for-each print invalid-nodes)
             (exit 1)))))

(define (perform-synchronize #!optional paths)
  (if (not paths)
    (exit 1)
    (synchronize (append (list (current-directory)) paths))))
