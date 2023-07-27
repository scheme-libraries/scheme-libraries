#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries)
  (export
    construct-name
    define-auxiliary-syntax
    define/who
    define-syntax/who
    define-values
    ellipsis?
    make-parameter
    make-thread-parameter
    parameterize
    symbolic-identifier=?
    with-implicit

    ;; (scheme-libraries atoms)
    atom?

    ;; (scheme-libraries basic-format-strings)
    format

    ;; (scheme-libraries boxes)
    box
    box?
    unbox
    set-box!

    ;; (scheme-libraries counters)
    make-counter

    ;; (scheme-libraries display-condition)
    display-condition

    ;; (scheme-libraries equality)
    make-equal?

    ;; (scheme-libraries exceptions)
    assertion-violationf
    errorf

    ;; (scheme-libraries filenames)
    filename?

    ;; (scheme-libraries format-conditions)
    &format
    make-format-condition
    format-condition?

    ;; (scheme-libraries gensyms)
    gensym
    gensym-suffix
    gensym-marker

    ;; (scheme-libraries graph-coloring)
    color?
    node
    node?
    node=?
    node-color
    make-graph
    graph?
    graph-colored?
    graph-add-interference!
    graph-add-move!
    color-graph!

    ;; (scheme-libraries heaps)
    make-heap
    heap?
    heap-ordering-predicate
    heap-push!
    heap-pop!
    heap-top
    heap-size
    heap-empty?

    ;; (scheme-libraries impure)
    increment!
    prepend!

    ;; (scheme-libraries lists)
    iota
    make-list
    length+
    split-at

    ;; (scheme-libraries match)
    match
    unquote
    ...
    _
    ->
    guard

    ;; (scheme-libraries numbers)
    exact-integer?
    exact-positive-integer?
    exact-nonnegative-integer?
    nonnegative-fixnum?
    int32?
    int64?

    ;; (scheme-libraries ports)
    textual-input-port?
    textual-output-port?

    ;; (scheme-libraries random-numbers)
    random
    random-seed

    ;; (scheme-libraries reading annotated-datums)
    annotated-datum?
    annotated-datum-source-location
    annotated-datum-value
    make-annotated-atom
    annotated-atom?
    make-annotated-pair
    annotated-pair?
    annotated-pair-car
    annotated-pair-cdr
    make-annotated-list
    make-annotated-dotted-list
    make-annotated-vector
    annotated-vector?
    annotated-vector-ref

    ;; (scheme-libraries reading lexemes)
    lexeme?
    lexeme-start
    lexeme-end
    make-end-of-input
    end-of-input?
    make-atomic
    atomic?
    atomic-value
    make-left-parenthesis
    left-parenthesis?
    make-right-parenthesis
    right-parenthesis?
    make-left-bracket
    left-bracket?
    make-right-bracket
    right-bracket?
    make-vector-prefix
    vector-prefix?
    make-bytevector-prefix
    bytevector-prefix?
    make-abbreviation
    abbreviation?
    abbreviation-symbol
    make-dot
    dot?

    ;; (scheme-libraries reading positions)
    make-position
    position?
    position-line
    position-column
    position-lines
    position-columns
    position-tabulator

    ;; (scheme-libraries reading readers)
    make-reader
    reader?
    reader-get-annotated-datum

    ;; (scheme-libraries reading source-locations)
    make-source-location
    source-location?
    source-location-filename
    source-location-start
    source-location-end
    &source-location-condition
    make-source-location-condition
    source-location-condition?
    condition-source-location
    display-source-location

    ;; (scheme-libraries reading tokenizers)
    make-tokenizer
    tokenizer?
    tokenizer-get-lexeme
    tokenizer-lexical-error
    &lexical-error
    make-lexical-error
    lexical-error?

    ;; (scheme-libraries rec)
    rec

    ;; (scheme-libraries record-writer)
    record-writer

    ;; (scheme-libraries repl)
    repl
    repl-prompt-string
    console-input-port
    console-output-port

    ;; (scheme-libraries strings)
    string-split

    ;; (scheme-libraries trees)
    tree->list

    ;; (scheme-libraries unicode)
    unicode-scalar-value?
    unicode-width

    ;; (scheme-libraries union-find)
    union-find

    ;; (scheme-libraries uuid)
    random-uuid
    uuid->string
    uid

    ;; (scheme-libraries void)
    void

    ;; (scheme-libraries worklists)
    element
    element?
    element-worklist
    make-worklist
    worklist?
    worklist-empty?
    worklist-first
    worklist-add!
    worklist-for-each
    element-remove!
    worklist->list)
  (import
    (scheme-libraries atoms)
    (scheme-libraries basic-format-strings)
    (scheme-libraries boxes)
    (scheme-libraries counters)
    (scheme-libraries define-auxiliary-syntax)
    (scheme-libraries define-who)
    (scheme-libraries define-values)
    (scheme-libraries display-condition)
    (scheme-libraries equality)
    (scheme-libraries exceptions)
    (scheme-libraries filenames)
    (scheme-libraries format-conditions)
    (scheme-libraries gensyms)
    (scheme-libraries graph-coloring)
    (scheme-libraries heaps)
    (scheme-libraries helpers)
    (scheme-libraries impure)
    (scheme-libraries lists)
    (scheme-libraries match)
    (scheme-libraries numbers)
    (scheme-libraries ports)
    (scheme-libraries random-numbers)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries reading lexemes)
    (scheme-libraries reading positions)
    (scheme-libraries reading readers)
    (scheme-libraries reading source-locations)
    (scheme-libraries reading tokenizers)
    (scheme-libraries rec)
    (scheme-libraries record-writer)
    (scheme-libraries repl)
    (scheme-libraries parameters)
    (scheme-libraries strings)
    (scheme-libraries thread-parameters)
    (scheme-libraries trees)
    (scheme-libraries unicode)
    (scheme-libraries union-find)
    (scheme-libraries uuid)
    (scheme-libraries void)
    (scheme-libraries with-implicit)
    (scheme-libraries worklists)))
