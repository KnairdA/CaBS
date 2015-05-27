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
        (make-command "validate"
                      perform-validate
                      (make-documentation "validate"
                                          "validate storage integrity"))))

(define (name->command-implementation name)
  (let ((command (find (lambda (command)
                         (string=? name (command-name command)))
                       commands)))
    (command-implementation command)))

(define (perform-operation arguments)
  (if (null-list? arguments)
    ((name->command-implementation "help")          (list))
    ((name->command-implementation (car arguments)) (cdr arguments))))

(perform-operation (command-line-arguments))
