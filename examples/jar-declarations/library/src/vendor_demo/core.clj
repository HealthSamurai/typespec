(ns vendor-demo.core)

;; Test-only tripwire: real runtime calls work normally. During static checking
;; this property is true, so evaluating this namespace would fail immediately.
(when (Boolean/getBoolean "vendor-demo.check-only")
  (throw (ex-info "VENDOR_DEMO_LIBRARY_EXECUTED" {:check-only true})))

(defn identity-value [x] x)

(defn greet [name]
  (str "Hello, " name "!"))
