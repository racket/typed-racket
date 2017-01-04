#lang racket/base

;; implied-atomic? was improved for disjunctions
;; these tests make sure those improvements
;; are still working (these used to take
;; an astronomical amount of time)

(require racket/sandbox)

(call-with-limits
 120
 500
 (λ () (eval '(begin (module a typed/racket
                       (provide baz)
                       (: baz : (Any -> Any))
                       (define (baz v)
                         (match v
                           [`(VARREF ,(? symbol? x)) 'return]
                           [`(Lambda ,(app baz fs) ,e ...) 'return]
                           [`(CaseLambda ,lc ...) 'return]
                           [`(If ,(app baz cond) ,(app baz then) ,(app baz else)) 'return]
                           [`(Begin ,e ...) 'return]
                           [`(Begin0 ,(app baz e1) ,e ...) 'return]
                           [`(LetValues (,lvs ...) ,e ...) 'return]
                           [`(LetrecValues (,lvs ...) ,e ...) 'return]
                           [`(SetBang ,(? symbol? x) ,(app baz e)) 'return]
                           [`(Quote ,(app baz d)) 'return]
                           [`(QuoteSyntax ,(app baz d)) 'return]
                           [`(WithContMark ,(app baz e1) ,(app baz e2) ,(app baz e3)) 'return]
                           [`(App ,e ...) 'return]
                           [`(Top ,(? symbol? x)) 'return]
                           [`(VariableReference ,(? symbol? x)) 'return]
                           [`(VariableReferenceTop ,(? symbol? x)) 'return]
                           [`(VariableReference1 ,(? symbol? x)) 'return]
                           [`(VariableReferenceTop2 ,(? symbol? x)) 'return]
                           [`(Quote2 ,(app baz d)) 'return]
                           [`(QuoteSyntax3 ,(app baz d)) 'return]
                           [`(VARREF2 ,(? symbol? x)) 'return]
                           [`(Lambda2 ,(app baz fs) ,e ...) 'return]
                           [`(CaseLambda2 ,lc ...) 'return]
                           [`(If2 ,(app baz cond) ,(app baz then) ,(app baz else)) 'return]
                           [`(Begin2 ,e ...) 'return]
                           [`(Begin02 ,(app baz e1) ,e ...) 'return]
                           [`(VariableReference3 ,(? symbol? x)) 'return]
                           [`(VariableReferenceTop3 ,(? symbol? x)) 'return]
                           [`(Quote3 ,(app baz d)) 'return]
                           [`(QuoteSyntax4 ,(app baz d)) 'return]
                           [`(VARREF3 ,(? symbol? x)) 'return]
                           [`(Lambda3 ,(app baz fs) ,e ...) 'return]
                           [`(CaseLambda3 ,lc ...) 'return]
                           [`(If3 ,(app baz cond) ,(app baz then) ,(app baz else)) 'return]
                           [`(Begin3 ,e ...) 'return]
                           [`(Begin03 ,(app baz e1) ,e ...) 'return]
                           [`(Begin11 ,e ...) 'return]
                           [`(Begin011 ,(app baz e1) ,e ...) 'return]
                           [`(VariableReference11 ,(? symbol? x)) 'return]
                           [`(VariableReferenceTop11 ,(? symbol? x)) 'return]
                           [`(Quote11 ,(app baz d)) 'return]
                           [`(QuoteSyntax11 ,(app baz d)) 'return]
                           [`(VARREF11 ,(? symbol? x)) 'return]
                           [`(Lambda11 ,(app baz fs) ,e ...) 'return]
                           [`(CaseLambda11 ,lc ...) 'return]
                           [`(If11 ,(app baz cond) ,(app baz then) ,(app baz else)) 'return]
                           [`(Begin11 ,e ...) 'return]
                           [`(Begin11 ,(app baz e1) ,e ...) 'return]
                           [`(Begin12 ,e ...) 'return]
                           [`(Begin012 ,(app baz e1) ,e ...) 'return]
                           [`(VariableReference12 ,(? symbol? x)) 'return]
                           [`(VariableReferenceTop12 ,(? symbol? x)) 'return]
                           [`(Quote12 ,(app baz d)) 'return]
                           [`(QuoteSyntax12 ,(app baz d)) 'return]
                           [`(VARREF12 ,(? symbol? x)) 'return]
                           [`(Lambda12 ,(app baz fs) ,e ...) 'return]
                           [`(CaseLambda12 ,lc ...) 'return]
                           [`(If12 ,(app baz cond) ,(app baz then) ,(app baz else)) 'return]
                           [`(Begin12 ,e ...) 'return]
                           [`(Begin12 ,(app baz e1) ,e ...) 'return]
                           [`(Begin13 ,e ...) 'return]
                           [`(Begin013 ,(app baz e1) ,e ...) 'return]
                           [`(VariableReference13 ,(? symbol? x)) 'return]
                           [`(VariableReferenceTop13 ,(? symbol? x)) 'return]
                           [`(Quote13 ,(app baz d)) 'return]
                           [`(QuoteSyntax13 ,(app baz d)) 'return]
                           [`(VARREF13 ,(? symbol? x)) 'return]
                           [`(Lambda13 ,(app baz fs) ,e ...) 'return]
                           [`(CaseLambda13 ,lc ...) 'return]
                           [`(If13 ,(app baz cond) ,(app baz then) ,(app baz else)) 'return]
                           [`(Begin13 ,e ...) 'return]
                           [`(Begin13 ,(app baz e1) ,e ...) 'return]
                           [`(Begin14 ,e ...) 'return]
                           [`(Begin014 ,(app baz e1) ,e ...) 'return]
                           [`(VariableReference14 ,(? symbol? x)) 'return]
                           [`(VariableReferenceTop14 ,(? symbol? x)) 'return]
                           [`(Quote14 ,(app baz d)) 'return]
                           [`(QuoteSyntax14 ,(app baz d)) 'return]
                           [`(VARREF14 ,(? symbol? x)) 'return]
                           [`(Lambda14 ,(app baz fs) ,e ...) 'return]
                           [`(CaseLambda14 ,lc ...) 'return]
                           [`(If14 ,(app baz cond) ,(app baz then) ,(app baz else)) 'return]
                           [`(Begin14 ,e ...) 'return]
                           [`(Begin14 ,(app baz e1) ,e ...) 'return]
                           )))
                     (require 'a)
                     baz)
             (make-base-namespace))))
