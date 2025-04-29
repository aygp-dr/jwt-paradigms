#lang racket

(require json)

(define (base64url->bytes str)
  (define padding (make-string (modulo (- 0 (string-length str)) 4) #\=))
  (define base64 (string-map (λ (c)
                               (match c
                                 [#\- #\+]
                                 [#\_ #\/]
                                 [_ c]))
                             str))
  (base64-decode (string-append base64 padding)))

(define (decode-jwt-header auth-header)
  (define token (second (string-split auth-header)))
  (define header-part (first (string-split token ".")))
  (define decoded-bytes (base64url->bytes header-part))
  (define decoded-str (bytes->string/utf-8 decoded-bytes))
  (string->jsexpr decoded-str))

(define auth-header "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U")
(displayln (decode-jwt-header auth-header))
