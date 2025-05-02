(import base64 json)

;; WARNING: This example demonstrates JWT header parsing WITHOUT signature verification.
;; NEVER use this approach for authentication or authorization in production.
;; ALWAYS verify the JWT signature before trusting any data in the token.

(defn decode-base64url [segment]
  "Decode a base64url-encoded string to bytes"
  (let [padding-needed (% (len segment) 4)]
    ;; Add padding if needed
    (when padding-needed
      (setv segment (+ segment (* "=" (- 4 padding-needed)))))
    
    ;; Convert from URL-safe base64 to standard base64
    (setv segment (.replace (.replace segment "-" "+") "_" "/"))
    
    ;; Decode and return
    (base64.b64decode segment)))

(defn decode-jwt-header [auth-header]
  "Decode a JWT header from an Authorization header"
  (try
    (let [token (get (.split auth-header " ") 1)
          header-part (get (.split token ".") 0)
          
          ;; Decode base64url
          decoded-bytes (decode-base64url header-part)
          decoded-str (.decode decoded-bytes "utf-8")
          
          ;; Parse JSON
          header (json.loads decoded-str)]
          
      ;; Validate structure
      (unless (isinstance header dict)
        (raise (ValueError "JWT header is not a JSON object")))
      
      ;; Verify required fields
      (unless (in "alg" header)
        (raise (ValueError "JWT header missing 'alg' field")))
      (unless (in "typ" header)
        (raise (ValueError "JWT header missing 'typ' field")))
        
      ;; Return valid header
      header)
    (except [e Exception]
      (raise (ValueError f"Invalid JWT header: {(str e)}")))))

;; Usage
(print (decode-jwt-header "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U"))
