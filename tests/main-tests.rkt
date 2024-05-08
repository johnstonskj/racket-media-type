#lang racket/base

(require rackunit
         rackunit/text-ui
         "../main.rkt")

(provide media-type-test-suite)

(define (check-round-trip s)
  (check-equal? s (media-type->string (string->media-type s))))

(define media-type-test-suite
  (test-suite
   "Module media-type"

   ;; https://www.iana.org/assignments/media-types/media-types.xhtml
   (test-case
       "string->media-type round-trip"
     (check-round-trip "application/json")
     (check-round-trip "application/foobar+json")
     (check-round-trip "application/vnd.foobar+json")
     (check-round-trip "text/plain;charset=UTF-8"))

   ;; https://www.iana.org/assignments/media-type-sub-parameters/media-type-sub-parameters.xhtml
   (test-case
       "check parameters"
     (check-round-trip "video/mpeg4-generic;mode=CELP-cbr"))

   )) ;; << test-suite

(run-tests media-type-test-suite)
