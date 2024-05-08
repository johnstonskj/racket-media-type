#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          media-type
          (for-label racket/base
                     media-type))

@;{============================================================================}

@(define example-eval (make-base-eval '(require media-type)))

@;{============================================================================}
@;{============================================================================}

@title[#:version  "1.0"]{Internet Media Types}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]
@defmodule[media-type]

This package provides types and predicates for testing internet media types.

@nested[#:style 'inset]{
@italic{A media type (formerly known as a MIME type) is a two-part identifier for file formats and format contents
transmitted on the Internet. Their purpose is somewhat similar to file extensions in that they identify the intended
data format. The Internet Assigned Numbers Authority (IANA) is the official authority for the standardization and
publication of these classifications.}
}

@nested[#:style 'code-inset]{
@verbatim|{
mime-type := type "/" sub-type ["+" suffix] [";" parameter]*
sub-type  := [tree "."] subtype
parameter := param-name "=" param-value

type      := restricted-name
tree      := restricted-name
subtype   := restricted-name + "."
param-name := restricted-name + ":" "+" "."
}|
}


@defstruct*[media-type
            ([type media-type-type-string?]
             [subtype (listof media-type-subtype-facet-string?)]
             [suffix (or/c media-type-subtype-suffix-string? #f)]
             [parameters (hash/c media-type-parameter-name-string?
                                 media-type-parameter-value-string?)])]{
TBD
}

@defproc[#:kind "constructor"
         (string->media-type
          [str string?])
         media-type?]{
TBD

@examples[
  #:eval example-eval
(let ((mt (string->media-type "application/vnd.3gpp.crs+xml")))
  (displayln mt)
  (displayln (media-type-type mt))
  (displayln (media-type-subtype mt))
  (displayln (media-type-suffix mt))
  (displayln (media-type-parameters mt)))
]
}

@defproc[(media-type-subtype-tree
          [mt media-type?])
         media-type-subtype-tree-string?]{
TBD

@examples[
  #:eval example-eval
(let ((mt (string->media-type "application/vnd.3gpp.crs+xml")))
  (displayln (media-type-subtype mt))
  (displayln (media-type-subtype-tree mt)))
]
}

@defproc[(media-type-subtype-facets
          [mt media-type?])
         (listof media-type-subtype-facet-string?)]{
TBD

@examples[
  #:eval example-eval
(let ((mt (string->media-type "application/vnd.3gpp.crs+xml")))
  (displayln (media-type-subtype mt))
  (displayln (media-type-subtype-facets mt)))
]
}

@;{============================================================================}

@defproc[#:kind "predicate"
         (media-type-vendor-tree?
          [mt media-type?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(let ((mt (string->media-type "application/vnd.3gpp.crs+xml")))
  (displayln (media-type-vendor-tree? mt)))
]
}

@defproc[#:kind "predicate"
         (media-type-personal-tree?
          [mt media-type?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(let ((mt (string->media-type "application/prs.implied-object+json")))
  (displayln (media-type-personal-tree? mt)))
]
}

@defproc[#:kind "predicate"
         (media-type-private-tree?
          [mt media-type?])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (media-type-experimental-tree?
          [mt media-type?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(let ((mt (string->media-type "multipart/x-mixed-replace")))
  (displayln (media-type-experimental-tree? mt)))
]
}

@defproc[#:kind "predicate"
         (media-type-has-facets?
          [mt media-type?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(let ((mt (string->media-type "application/prs.implied-object+json")))
  (displayln (media-type-has-facets? mt)))
]
}

@defproc[#:kind "predicate"
         (media-type-has-suffix?
          [mt media-type?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(let ((mt (string->media-type "application/prs.implied-object+json")))
  (displayln (media-type-has-suffix? mt)))
]
}

@defproc[#:kind "predicate"
         (media-type-has-parameters?
          [mt media-type?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(let ((mt (string->media-type "text/plain;charset=UTF-8")))
  (displayln (media-type-has-parameters? mt)))
]
}

@;{============================================================================}

@defproc[(media-type->string
          [mt media-type?])
         string?]{
TBD

@examples[
  #:eval example-eval
(displayln
 (media-type->string
  (string->media-type "application/prs.implied-object+json")))
]
}

@;{============================================================================}

@section[]{String Predicates}

TBD

@defproc[#:kind "predicate"
         (media-type-string?
          [str string?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(displayln (media-type-string? "text/plain;charset=UTF-8"))
(displayln (media-type-string? "text plain"))
]
}

@defproc[#:kind "predicate"
         (media-type-type-string?
          [str string?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(displayln (media-type-type-string? "text"))
(displayln (media-type-type-string? "text+pictures"))
]
}

@defproc[#:kind "predicate"
         (media-type-subtype-tree-string?
          [str string?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(displayln (media-type-subtype-tree-string? "plain"))
(displayln (media-type-subtype-tree-string? "vnd"))
(displayln (media-type-subtype-tree-string? "plain+style"))
]
}

@defproc[#:kind "predicate"
         (media-type-subtype-facet-string?
          [str string?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(displayln (media-type-subtype-facet-string? "implied-object"))
(displayln (media-type-subtype-facet-string? "implied.object"))
]
}

@defproc[#:kind "predicate"
         (media-type-subtype-suffix-string?
          [str string?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(displayln (media-type-subtype-suffix-string? "xml"))
(displayln (media-type-subtype-suffix-string? "binary json"))
]
}

@defproc[#:kind "predicate"
         (media-type-subtype-string?
          [str string?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(displayln (media-type-subtype-string? "prs.implied-object+json"))
]
}

@defproc[#:kind "predicate"
         (media-type-parameter-name-string?
          [str string?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(displayln (media-type-parameter-name-string? "charset"))
]
}

@defproc[#:kind "predicate"
         (media-type-parameter-value-string?
          [str string?])
         boolean?]{
TBD

@examples[
  #:eval example-eval
(displayln (media-type-parameter-value-string? "UTF-8"))
]
}

@;{============================================================================}

@section[]{Appendix - Grammar}

From @cite["RFC6838"]:

@nested[#:style 'inset]{
Type and subtype names MUST conform to the following ABNF:

@nested[#:style 'code-inset]{
@verbatim|{
  type-name = restricted-name
  subtype-name = restricted-name

  restricted-name = restricted-name-first *126restricted-name-chars
  restricted-name-first  = ALPHA / DIGIT
  restricted-name-chars  = ALPHA / DIGIT / "!" / "#" /
                           "$" / "&" / "-" / "^" / "_"
  restricted-name-chars =/ "." ; Characters before first dot always
                               ; specify a facet name
  restricted-name-chars =/ "+" ; Characters after last plus always
                               ; specify a structured syntax suffix
}|
}

Note that this syntax is somewhat more restrictive than what is
allowed by the ABNF in Section 5.1 of [RFC2045] or Section 4.2 of
[RFC4288].  Also note that while this syntax allows names of up to
127 characters, implementation limits may make such long names
problematic.  For this reason, <type-name> and <subtype-name> SHOULD
be limited to 64 characters.

Although the name syntax treats "." as equivalent to any other
character, characters before any initial "." always specify the
registration facet.  Note that this means that facet-less standards-
tree registrations cannot use periods in the subtype name.

Similarly, the final "+" in a subtype name introduces a structured
syntax specifier suffix.  Structured syntax suffix requirements are
specified in Section 4.2.8.
}

Also, ...

@nested[#:style 'inset]{
Parameter names have the syntax as media type names and values:

@nested[#:style 'code-inset]{
@verbatim|{
  parameter-name = restricted-name
}|
}
}

@nested[#:style 'inset]{
Parameter names have the syntax as media type names and values:

@nested[#:style 'code-inset]{
@verbatim|{
  parameter-name = restricted-name
}|
}
}

@nested[#:style 'inset]{
There is no defined syntax for parameter values.  Therefore,
registrations MUST specify parameter value syntax.  Additionally,
some transports impose restrictions on parameter value syntax, so
care needs be taken to limit the use of potentially problematic
syntaxes; e.g., pure binary valued parameters, while permitted in
some protocols, are best avoided.

Note that a protocol can impose further restrictions on parameter
value syntax, depending on how it chooses to represent parameters.
Both MIME [RFC2045] [RFC2231] and HTTP [RFC2045] [RFC5987] allow
binary parameters as well as parameter values expressed in a specific
charset, but other protocols may be less flexible.
}

From @cite["RFC4288"]:

@nested[#:style 'inset]{
Type and subtype names MUST conform to the following ABNF:

@nested[#:style 'code-inset]{
@verbatim|{
  type-name = reg-name
  subtype-name = reg-name

  reg-name = 1*127reg-name-chars
  reg-name-chars = ALPHA / DIGIT / "!" /
                   "#" / "$" / "&" / "." /
                   "+" / "-" / "^" / "_"
}|
}

Note that this syntax is somewhat more restrictive than what is
allowed by the ABNF in [RFC2045].
}

From @cite["RFC2045"]:

@nested[#:style 'inset]{
In the Augmented BNF notation of RFC 822, a Content-Type header field
value is defined as follows:

@nested[#:style 'code-inset]{
@verbatim|{
  content := "Content-Type" ":" type "/" subtype
             *(";" parameter)
             ; Matching of media type and subtype
             ; is ALWAYS case-insensitive.

  type := discrete-type / composite-type

  discrete-type := "text" / "image" / "audio" / "video" /
                   "application" / extension-token

  composite-type := "message" / "multipart" / extension-token

  extension-token := ietf-token / x-token

  ietf-token := <An extension token defined by a
                 standards-track RFC and registered
                 with IANA.>

  x-token := <The two characters "X-" or "x-" followed, with
              no intervening white space, by any token>

  subtype := extension-token / iana-token

  iana-token := <A publicly-defined extension token. Tokens
                 of this form must be registered with IANA
                 as specified in RFC 2048.>

  parameter := attribute "=" value

  attribute := token
               ; Matching of attributes
               ; is ALWAYS case-insensitive.

  value := token / quoted-string

  token := 1*<any (US-ASCII) CHAR except SPACE, CTLs,
              or tspecials>

  tspecials :=  "(" / ")" / "<" / ">" / "@" /
                "," / ";" / ":" / "\" / <">
                "/" / "[" / "]" / "?" / "="
                ; Must be in quoted-string,
                ; to use within parameter values
}|
}
}

@;{============================================================================}
@;{============================================================================}

@(bibliography

  (bib-entry #:key "RFC2045"
             #:title "Multipurpose Internet Mail Extensions (MIME) Part One: Format of Internet Message Bodies"
             #:author ". Freed, and . Borenstein"
             #:location "RFC"
             #:url "http://www.ietf.org/rfc/rfc2045.txt"
             #:date "ovember 1996")

  (bib-entry #:key "RFC2231"
             #:title "MIME Parameter Value and Encoded Word Extensions: Character Sets, Languages, and Continuations"
             #:author "N. Freed, and  Moore"
             #:location "RFC"
             #:url "http://www.ietf.org/rfc/rfc2231.txt"
             #:date "November 1997")

  (bib-entry #:key "RFC4288"
             #:title "Media Type Specifications and Registration Procedures"
             #:author "N. Freed, and J. Klensin"
             #:location "RFC"
             #:url "http://www.ietf.org/rfc/rfc4288.txt"
             #:date "December 2005")

  (bib-entry #:key "RFC5987"
             #:title "Character Set and Language Encoding for Hypertext Transfer Protocol (HTTP) Header Field Parameters"
             #:author "J. Reschke"
             #:location "RFC"
             #:url "http://www.ietf.org/rfc/rfc5987.txt"
             #:date "August 2010")

  (bib-entry #:key "RFC6838"
             #:title "Media Type Specifications and Registration Procedures"
             #:author "N. Freed, J. Klensin, and T. Hansen"
             #:location "RFC"
             #:url "http://www.ietf.org/rfc/rfc6838.txt"
             #:date "January 2013")
)

@;{============================================================================}
@;{============================================================================}

@index-section[]
