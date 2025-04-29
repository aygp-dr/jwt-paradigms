(import base64 json)

(defn decode-jwt-header [auth-header]
  (let [[token (get (.split auth-header " ") 1)]
        [header-part (get (.split token ".") 0)]
        ;; Add padding if needed
        [padding-needed (% (len header-part) 4)]]
    (if padding-needed
        (setv header-part (+ header-part (* "=" (- 4 padding-needed)))))
    ;; Decode base64url format to standard
    (setv header-part (.replace (.replace header-part "-" "+") "_" "/"))
    ;; Decode and parse
    (let [[decoded-bytes (base64.b64decode header-part)]
          [decoded-str (.decode decoded-bytes "utf-8")]]
      (json.loads decoded-str))))

;; Usage
(print (decode-jwt-header "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U"))
