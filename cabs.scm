(include "src/commands.scm")

(define commands
  (list (make-command "help"
                      perform-help
                      (make-documentation "help"
                                          "display this message"))
        (make-command "add"
                      perform-add
                      (make-documentation "add [FILE]"
                                          "add a blob from FILE or stdin"))
        (make-command "check"
                      perform-check
                      (make-documentation "check HASH"
                                          "check if a blob exists for HASH"))
        (make-command "get"
                      perform-get
                      (make-documentation "get HASH"
                                          "get path to blob of HASH"))
        (make-command "synchronize"
                      perform-synchronize
                      (make-documentation "synchronize PATH"
                                          "synchronize storage with storage at PATH"))
        (make-command "ls-blobs"
                      perform-ls-blobs
                      (make-documentation "ls-blobs"
                                          "list all blob hashes"))
        (make-command "digest"
                      perform-digest
                      (make-documentation "digest"
                                          "get hash of all blob hashes"))
        (make-command "validate"
                      perform-validate
                      (make-documentation "validate"
                                          "validate storage integrity"))))

(define (name->command-implementation name)
  (let ((command (find (lambda (command)
                         (string=? name (command-name command)))
                       commands)))
    (if (not command)
      (perform-unrecognized-command name)
      (command-implementation command))))

(define (perform-operation arguments)
  (cond ((null-list? arguments)   ((name->command-implementation "help")))
        ((= 1 (length arguments)) ((name->command-implementation (car arguments))))
        (else ((name->command-implementation (car arguments)) (cdr arguments)))))

(perform-operation (command-line-arguments))
