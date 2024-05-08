#lang info
(define collection "media-type")
(define deps '("base" "rx"))
(define build-deps '("scribble-lib" "racket-doc" "sandbox-lib" "rackunit-lib"))
(define scribblings '(("scribblings/media-type.scrbl" (multi-page))))
(define pkg-desc "Type and predicates for testing internet media types")
(define version "1.0")
(define pkg-authors '(johnstonskj))
(define license 'Apache-2.0)
