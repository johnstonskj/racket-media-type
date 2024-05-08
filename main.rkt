#lang racket/base

(require racket/bool
         racket/contract
         racket/function
         racket/list
         racket/string
         ;; --------------------------------------
         rx)

(provide media-type-string?
         media-type-type-string?
         media-type-subtype-tree-string?
         media-type-subtype-facet-string?
         media-type-subtype-suffix-string?
         media-type-subtype-string?
         media-type-parameter-name-string?
         media-type-parameter-value-string?
         ;; --------------------------------------
         (struct-out media-type)
         string->media-type
         media-type-subtype-tree
         media-type-subtype-facets
         ;; --------------------------------------
         media-type-vendor-tree?
         media-type-personal-tree?
         media-type-private-tree?
         media-type-experimental-tree?
         media-type-has-facets?
         media-type-has-suffix?
         media-type-has-parameters?
         ;; --------------------------------------
         media-type->string)

;; =============================================================================
;; Internal
;; =============================================================================

(define restricted-name-first
  (rx/match
   (rx/range #\A #\Z)
   (rx/range #\a #\z)
   (rx/range #\0 #\9)))

(define restricted-name-chars
  (rx/match
   (rx/range #\A #\Z)
   (rx/range #\a #\z)
   (rx/range #\0 #\9)
   #\! #\# #\$ #\& #\- #\^ #\_))

(define restricted-name
  (rx/and
   restricted-name-first
   (rx/repeat restricted-name-chars #:lower 0 #:upper 63)))

(define mt-type-regexp (rx/group restricted-name))

(define mt-subtype-tree (rx/group restricted-name))

(define mt-subtype-facets
  (rx/nc-group
   (rx/escape #\. 'outside-match)
   (rx/and-group
    restricted-name
    (rx/nc-group
     (rx/escape #\. 'outside-match)
     restricted-name
     #:repeat 'zero-or-more))
   #:repeat 'optional))

(define mt-subtype-suffix
  (rx/nc-group
   (rx/and
    (rx/escape #\+ 'outside-match)
    (rx/group restricted-name))
   #:repeat 'optional))

(define mt-subtype-regexp
  (rx/nc-group
   mt-subtype-tree
   mt-subtype-facets
   mt-subtype-suffix))

(define mt-parameter-name restricted-name)


(define mt-parameter-value
  (rx/and
   restricted-name-first
   (rx/repeat
    (rx/match
     (rx/range #\A #\Z)
     (rx/range #\a #\z)
     (rx/range #\0 #\9)
     #\! #\: #\# #\$ #\& #\- #\^ #\_ #\+ #\.)
    #:lower 0 #:upper 63)))

(define mt-parameter
  (rx/nc-group (rx/and mt-parameter-name #\= mt-parameter-value)))

(define mt-parameter-separator
  (rx/and (rx/* rx/cclass-whitespace) #\; (rx/* rx/cclass-whitespace)))

(define mt-parameter-list
  (rx/nc-group
  mt-parameter-separator
   (rx/and-group
    mt-parameter
    (rx/nc-group
     mt-parameter-separator
     mt-parameter
     #:repeat 'zero-or-more))
   #:repeat 'optional))

(define media-type-all
  (rx/string-exactly
   (rx/and
    mt-type-regexp
    (rx/escape #\/ 'outside-match)
    mt-subtype-regexp
    mt-parameter-list)))

;; =============================================================================
;; String predicates
;; =============================================================================

(define media-type-regexp (pregexp media-type-all))

(define (media-type-string? v)
  (and (string? v)
       (not (false? (regexp-match media-type-regexp v)))))

(define restricted-name-regexp (pregexp (rx/string-exactly restricted-name)))

(define (media-type-type-string? v)
  (and (string? v)
       (not (false? (regexp-match restricted-name-regexp v)))))

(define (media-type-subtype-tree-string? v)
  (and (string? v)
       (not (false? (regexp-match restricted-name-regexp v)))))

(define (media-type-subtype-facet-string? v)
  (and (string? v)
       (not (false? (regexp-match restricted-name-regexp v)))))

(define (media-type-subtype-suffix-string? v)
  (and (string? v)
       (not (false? (regexp-match restricted-name-regexp v)))))

(define media-subtype-regexp (pregexp (rx/string-exactly mt-subtype-regexp)))

(define (media-type-subtype-string? v)
  (and (string? v)
       (not (false? (regexp-match media-subtype-regexp v)))))

(define (media-type-parameter-name-string? v)
  (and (string? v)
       (not (false? (regexp-match restricted-name-regexp v)))))

(define parameter-value-regexp (pregexp (rx/string-exactly mt-parameter-value)))

(define (media-type-parameter-value-string? v)
  (and (string? v)
       (not (false? (regexp-match parameter-value-regexp v)))))

;; =============================================================================
;; Structure
;; =============================================================================

(struct media-type (type subtype suffix parameters)
  #:transparent
  #:guard (struct-guard/c
           media-type-type-string?
           (listof media-type-subtype-facet-string?)
           (or/c media-type-subtype-suffix-string? #f)
           (hash/c media-type-parameter-name-string?
                   media-type-parameter-value-string?)))

(define (media-type-subtype-tree mt)
  (car (media-type-subtype mt)))

(define (media-type-subtype-facets mt)
  (cdr (media-type-subtype mt)))

;; =============================================================================
;; Predicates
;; =============================================================================

(define (media-type-vendor-tree? mt)
  (string=? (media-type-subtype-tree mt) "vnd"))

(define (media-type-personal-tree? mt)
  (string=? (media-type-subtype-tree mt) "prs"))

(define (media-type-private-tree? mt)
  (string=? (media-type-subtype-tree mt) "x"))

(define (media-type-experimental-tree? mt)
  (or (string-prefix? (media-type-subtype-tree mt) "x-")
      (string-prefix? (media-type-subtype-tree mt) "X-")))

(define (media-type-has-facets? mt)
  (> (length (media-type-subtype mt)) 1))

(define (media-type-has-suffix? mt)
  (not (empty? (media-type-suffix mt))))

(define (media-type-has-parameters? mt)
  (not (empty? (media-type-parameters mt))))

;; =============================================================================
;; To/from String
;; =============================================================================

(define (string->media-type v)
  (let ((matches (regexp-match media-type-regexp v)))
    (if (false? matches) #f
        (begin
          (media-type
           (second matches)
           (cons
            (third matches)
            (if (fourth matches)
                (string-split (fourth matches) ".")
                '()))
           (or (fifth matches) #f)
           (make-hash
            (if (sixth matches)
                (map (λ (param) (let ((name+value (string-split param "=")))
                                  (cons (car name+value) (cadr name+value))
                                  ))
                     (string-split (sixth matches) ";"))
                '())))))))

(define (media-type->string mt)
  (format "~a/~a~a~a"
          (media-type-type mt)
          (string-join (media-type-subtype mt) ".")
          (if (media-type-suffix mt) (string-append "+" (media-type-suffix mt)) "")
          (string-append* (hash-map (media-type-parameters mt)
                                    (λ (key val) (format ";~a=~a" key val))))))
