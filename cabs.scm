(include "src/commands.scm")

(define commands
  (list (make-command "help"
                      perform-help
                      (make-documentation "help"
                                          "displays this message"))
        (make-command "add"
                      perform-add
                      (make-documentation "add [FILE]"
                                          "adds a blob from FILE or stdin"))))

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
