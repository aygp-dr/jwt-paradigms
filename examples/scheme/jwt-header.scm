(use-modules (ice-9 iconv)
             (json)
             (rnrs bytevectors)
             (srfi srfi-60))

;; WARNING: This example demonstrates JWT header parsing WITHOUT signature verification.
;; NEVER use this approach for authentication or authorization in production.
;; ALWAYS verify the JWT signature before trusting any data in the token.

(define (base64url-decode str)
  (let* ((padding (make-string (modulo (- 0 (string-length str)) 4) #\=))
         (base64 (string-map (lambda (c)
                               (case c
                                 ((#\-) #\+)
                                 ((#\_) #\/)
                                 (else c)))
                             str))
         (padded (string-append base64 padding)))
    (base64-decode padded)))

(define (decode-jwt-header auth-header)
  (catch #t
    (lambda ()
      (let* ((token (cadr (string-split auth-header #\ )))
             (header-part (car (string-split token #\.)))
             (decoded-bytes (base64url-decode header-part))
             (decoded-str (bytevector->string decoded-bytes "UTF-8"))
             (header (json-string->scm decoded-str)))
        
        ;; Validate structure
        (unless (hash-table? header)
          (throw 'jwt-error "JWT header is not a JSON object"))
        
        ;; Verify required fields
        (unless (hash-ref header "alg")
          (throw 'jwt-error "JWT header missing 'alg' field"))
        (unless (hash-ref header "typ")
          (throw 'jwt-error "JWT header missing 'typ' field"))
        
        ;; Return valid header
        header))
    (lambda (key . args)
      (throw 'jwt-error (format #f "Invalid JWT header: ~a" (car args))))))

(define auth-header "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U")
(display (decode-jwt-header auth-header))
(newline)
