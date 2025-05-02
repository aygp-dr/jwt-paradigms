#lang racket

(require json)

;; WARNING: This example demonstrates JWT header parsing WITHOUT signature verification.
;; NEVER use this approach for authentication or authorization in production.
;; ALWAYS verify the JWT signature before trusting any data in the token.

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
  (with-handlers ([exn:fail? (λ (e) (error 'decode-jwt-header "Invalid JWT header: ~a" (exn-message e)))])
    (define token (second (string-split auth-header)))
    (define header-part (first (string-split token ".")))
    (define decoded-bytes (base64url->bytes header-part))
    (define decoded-str (bytes->string/utf-8 decoded-bytes))
    (define header (string->jsexpr decoded-str))
    
    ;; Validate structure
    (unless (hash? header)
      (error 'decode-jwt-header "JWT header is not a JSON object"))
    
    ;; Verify required fields
    (unless (hash-has-key? header 'alg)
      (error 'decode-jwt-header "JWT header missing 'alg' field"))
    (unless (hash-has-key? header 'typ)
      (error 'decode-jwt-header "JWT header missing 'typ' field"))
    
    ;; Return valid header
    header))

(define auth-header "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U")
(displayln (decode-jwt-header auth-header))
