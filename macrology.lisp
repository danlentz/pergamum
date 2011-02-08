;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)


(defmacro with-ignored-names (pastable-ignore-form-var (ignorelist-var &rest specs) &body body)
  "Each SPEC must be either a NAME, or a (NAME THING), with plain
NAME producing a plain, unprefixed gensym in the expansion.

Evaluates FORMS with ignored names rebound to gensyms, and tracks the
ignored names, so that PASTABLE-IGNORE-FORM-VAR contains a pastable list,
which contains an ignore declaration in the case there were any symbols
to ignore.

Example:
  (defmacro ignorably-let (((var value-form)) &body body)
    (with-ignored-names (ignores var)
      `(let ((,var ,value-form))
         ,@ignores
         ,@body)))
  (ignorably-let ((_ 123))
    (foo)) =>
  (let ((#:g42 123))
    (declare (ignore #:g42))
    (foo))"
  (check-type pastable-ignore-form-var symbol)
  (check-type ignorelist-var symbol)
  (let ((final-names (make-gensym-list (length specs) "FINAL-NAMES"))
        (ignore-name-p (gensym "IGNORE-CRITERIA"))
        (symbol-selector-name (gensym "SYMBOL-SELECTOR"))
        (bind-ignore-form-p (not (string= "_" (symbol-name pastable-ignore-form-var))))
        (bind-ignorelist-p  (not (string= "_" (symbol-name ignorelist-var))))
        (names-and-things (mapcar (lambda (spec)
                                    (etypecase spec
                                      (list
                                       (destructuring-bind (name thing) spec
                                         (list name thing)))
                                      (symbol
                                       (list spec nil))))
                                  specs)))
    (assert (or bind-ignore-form-p bind-ignorelist-p))
    (let ((ignore-tracker (if bind-ignorelist-p
                              ignorelist-var
                              (gensym "IGNORE-TRACKER"))))
      (multiple-value-bind (names things) (apply #'values (apply #'mapcar #'list names-and-things))
        `(labels ((,ignore-name-p (x) (string= (symbol-name x) "_"))
                  (,symbol-selector-name (x &optional (thing "G"))
                    (if (,ignore-name-p x)
                        (gensym thing)
                        x)))
           (let* (,@(mapcar (lambda (g n tn) (list g `(,symbol-selector-name ,n ,@(when tn (list tn)))))
                            final-names names things)
                  (,ignore-tracker (remove nil (mapcar (lambda (n f)
                                                         (when (,ignore-name-p n) f))
                                                       (list ,@names)
                                                       (list ,@final-names))))
                  ,@(when bind-ignore-form-p
                          `((,pastable-ignore-form-var (when ,ignore-tracker
                                                         `((declare (ignore ,@,ignore-tracker)))))))
                  ,@(mapcar (lambda (n g) `(,n ,g))
                            names final-names))
             ,@body))))))

(defmacro with-symbols-packaged-with ((&rest symbols) as &body body)
  "Execute BODY with SYMBOLS bound to the symbols of the corresponding
name, but contained within the package containing the symbol AS."
  (with-gensyms (package)
    `(let* ((,package (symbol-package ,as))
            ,@(mapcar (lambda (sym)
                        (list sym `(intern ,(symbol-name sym) ,package)))
                      symbols))
       ,@body)))

(defmacro pass-&key* (&rest symbols)
  "Given a list of SYMBOLS, return a plist, containing properties with
keywords named after corresponding SYMBOLS, and values being the
results of evaluation of SYMBOLS, without, however, those properties,
which have a corresponding predicate symbol bound to NIL.

The predicate symbol is named FOO-BAR-P, whenever a symbol name
contains a dash, and FOOBARP, whenever it doesn't."
  `(mapcan (lambda (pred-key-val)
             (when (first pred-key-val)
               (rest pred-key-val)))
           (list ,@(mapcar (lambda (sym &aux (name (symbol-name sym)))
                             `(list ,(find-symbol (format nil (if (find #\- name) "~A-P" "~AP") name))
                                    ,(make-keyword name)
                                    ,sym))
                           symbols))))

(defmacro defmacro* (name lambda-list &body body)
  "Define a macro wrt. NAME, LAMBDA-LIST and BODY, as if by DEFMACRO,
while binding key-passing macros around BODY to forms, which all
expand to a PASS-&KEY form, itself passed some or all names of full
&KEY specifications (regardless of where the latter are found within
the macro lambda list).

The following key-passing macro forms are defined:

  PASS-ALL-&KEYS

Passes names of all those keyword parameters, which have full,
three-element specifications.

  PASS-&KEYS &rest NAMES

Like PASS-ALL-&KEYS, but intersects the set of passed names with
NAMES.

  PASS-&KEYS-SANS &rest WITHOUT-NAMES

Like PASS-ALL-&KEYS, but subtracts WITHOUT-NAMES from the set of
passed names.

Example:

 (defmacro* with-fooage ((&key (foo nil foop) (bar nil barp) (qu-ux t qu-ux-p))
                         (&key (drat t dratp)) &body body)
   (let ((processed-body (process-body body drat)))
    `(invoke-with-fooage (lambda () ,@body) ,@(pass-&keys-sans drat))))"
  (with-symbols-packaged-with (pass-all-&keys pass-&keys pass-&keys-sans limit) name
    (multiple-value-bind (body decls doc) (parse-body body :documentation t)
      (labels ((keyspec-passable-p (spec)
                 (and (consp spec) (= 3 (length spec))))
               (check-passable-keyspec-valid (spec &aux (pred-name (symbol-name (third spec))))
                 (unless (or (not (find #\- pred-name))
                             (and (>= (count #\- pred-name) 2)
                                  (ends-with-subseq "-P" pred-name)))
                   (error "~@<In DEFMACRO* ~S: complete keyword argument specifications be idiomatic: ~
                            either FOO-BAR-P or FOOBARP, but not FOO-BARP or FOOBAR-P.~:@>" name)))
               (scour-single-list (list)
                 (remove-if-not #'keyspec-passable-p (rest (member '&key list))))
               (scour-tree-keyspecs (list)
                 (let ((&key-tail (member '&key list)))
                   (append (scour-single-list &key-tail)
                           (mapcan #'scour-tree-keyspecs (remove-if-not #'consp (ldiff list &key-tail)))))))
        (let* ((passable-keyspecs (scour-tree-keyspecs lambda-list))
               (passable-names (mapcar #'first passable-keyspecs)))
          (mapc #'check-passable-keyspec-valid passable-keyspecs)
          `(defmacro ,name ,lambda-list
             ,@(ensure-list doc)
             ,@decls
             (macrolet ((,pass-&keys (&rest ,limit)
                          `(pass-&key* ,@(remove-if-not (rcurry #'member ,limit) ',passable-names)))
                        (,pass-&keys-sans (&rest ,limit)
                          `(pass-&key* ,@(remove-if (rcurry #'member ,limit) ',passable-names)))
                        (,pass-all-&keys ()
                          '(,pass-&keys-sans)))
               ,@body)))))))
