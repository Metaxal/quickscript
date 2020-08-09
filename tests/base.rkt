#lang racket/base
(require "../base.rkt"
         rackunit
         racket/runtime-path)


(define-runtime-path test-compile_rkt--7.7.0.901.zo
  "test-compile_rkt--7.7.0.901.zo")
(define-runtime-path test-compile_rkt--7.8.0.6_cs.zo
  "test-compile_rkt--7.8.0.6_cs.zo")

(check-equal? (zo-version test-compile_rkt--7.8.0.6_cs.zo)
              '(#"7.8.0.6" #"chez-scheme"))
(check-equal? (zo-version test-compile_rkt--7.7.0.901.zo)
              '(#"7.7.0.901" #"racket"))
