;; One small case per feature. A line with a marker comment must be reported; every other line must be clean.
(ns f.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str])
  (:import [java.time LocalDate]))

(s/def :u/id string?)
(s/def :u/name string?)
(s/def :u/age int?)
(s/def :u/role #{:admin :user})
(s/def ::user (s/keys :req [:u/id :u/role] :opt [:u/name :u/age]))
(s/def ::users (s/coll-of ::user))

(s/fdef find-user :args (s/cat :id :u/id) :ret (s/nilable ::user))
(defn find-user [id] nil)

;; --- narrowing
(s/fdef n1 :args (s/cat :u (s/nilable ::user)) :ret string?)
(defn n1 [u]
  (cond
    (nil? u) "none"
    (:u/name u) (str/upper-case (:u/id u))
    :else (:u/id u)))

(s/fdef n2 :args (s/cat :u ::user) :ret string?)
(defn n2 [u]
  (let [n (:u/name u)]
    (if (and n (not (str/blank? n))) (str/trim n) "anonymous")))

(s/fdef n3 :args (s/cat :u ::user) :ret string?)
(defn n3 [u]
  (or (:u/name u) "anonymous"))

(s/fdef n4 :args (s/cat :id :u/id) :ret string?)
(defn n4 [id]
  (when-let [u (find-user id)]                    ; ERR may return nil
    (:u/id u)))

(s/fdef n5 :args (s/cat :id :u/id) :ret (s/nilable string?))
(defn n5 [id]
  (some-> (find-user id) :u/name str/upper-case))

(s/fdef n6 :args (s/cat :id :u/id) :ret string?)
(defn n6 [id]
  (-> (find-user id) :u/id str/upper-case))       ; ERR may be nil

(s/fdef n7 :args (s/cat :x any?) :ret string?)
(defn n7 [x]
  (cond (string? x) (str/trim x)
        (s/valid? ::user x) (:u/id x)
        (instance? LocalDate x) (str (.getYear x))   ; WARN reflective: the compiler does not narrow
        :else "?"))

(s/fdef n8 :args (s/cat :u ::user) :ret string?)
(defn n8 [u]
  (case (:u/role u)
    :admin "Administrator"
    :user "User"
    :guest "Guest"))                              ; ERR :guest is not a role

;; --- maps
(s/fdef m1 :args (s/cat :u ::user) :ret ::user)
(defn m1 [u]
  (-> u
      (assoc :u/name "Ann")
      (update :u/age inc)                         ; ERR age may be nil
      (dissoc :u/name)))

(s/fdef m2 :args (s/cat :u ::user) :ret ::user)
(defn m2 [u]
  (dissoc u :u/id))                               ; ERR required key is gone

(s/fdef m3 :args (s/cat :u ::user) :ret (s/keys :req [:u/id]))
(defn m3 [u]
  (select-keys u [:u/id :u/nmae]))                ; ERR typo

(s/fdef m4 :args (s/cat :u ::user) :ret ::user)
(defn m4 [u]
  (merge u {:u/age "42"}))                        ; ERR a string for :u/age

(s/fdef m5 :args (s/cat :u ::user) :ret int?)
(defn m5 [u]
  (get u :u/age 0))

(s/fdef m6 :args (s/cat :u ::user) :ret string?)
(defn m6 [{:u/keys [id] :as u}]
  (str id (:u/role u) (:u/email u)))              ; ERR no spec for :u/email

;; --- collections
(s/fdef c1 :args (s/cat :us ::users) :ret (s/coll-of string?))
(defn c1 [us]
  (->> us
       (filter #(= :admin (:u/role %)))
       (map :u/id)))

(s/fdef c2 :args (s/cat :us ::users) :ret (s/coll-of string?))
(defn c2 [us]
  (map :u/name us))                               ; ERR names may be nil

(s/fdef c3 :args (s/cat :us ::users) :ret (s/coll-of string?))
(defn c3 [us]
  (keep :u/name us))

(s/fdef c4 :args (s/cat :us ::users) :ret int?)
(defn c4 [us]
  (reduce + 0 (map (fn [u] (count (:u/id u))) us)))

(s/fdef c5 :args (s/cat :us ::users) :ret (s/coll-of string? :kind vector?))
(defn c5 [us]
  (vec (for [{:u/keys [id nme]} us] (str id nme)))) ; ERR typo in for

(s/fdef c6 :args (s/cat :us ::users) :ret (s/nilable ::user))
(defn c6 [[u & more]]
  (doseq [m more] (println (:u/id m)))
  u)

;; --- functions
(s/fdef f1 :args (s/cat :a int? :b (s/? int?)) :ret int?)
(defn f1
  ([a] (f1 a 1))
  ([a b] (+ a b)))

(defn f2 []
  (f1 1 2 3))                                     ; ERR too many arguments

(defn f3
  {:t/sig '[string? :-> int?]}
  [s]
  (count s))

(s/fdef f4 :args (s/cat :s string?) :ret string?)
(defn f4 {:t/sig '[int? :-> string?]} [s] (str s)) ; ERR t/sig and s/fdef disagree

(defn f5 {:t/sig '[strng? :-> int?]} [s] 1)        ; ERR not a type

(defn f6 [^long a ^long b ^long c ^long d ^long e] ; ERR more than 4 primitives
  (+ a b c d e))

(defmacro unless [test & body] `(if ~test nil (do ~@body)))

(defn f7 [x]
  (unless x (str/upper-case 42)))                  ; WARN macro is not described

;; --- java
(defn j1 ^long [^String s]
  (Long/parseLong s))

(defn j2 ^long []
  (.length (System/getenv "PORT")))               ; ERR getenv may return null

(defn j3 ^String []
  (str (LocalDate/of 2024 1 "2")))                ; ERR a string for the day

(defn j4 ^String [^LocalDate d]
  (try
    (str (.plusDays d 1))
    (catch Exception e (.getMessage e))))

(defn j5 [^String s]
  (.lenght s))                                    ; ERR no such method

;; --- escape hatches
(defn e1 [x]
  (str/upper-case #_(t/any "plugin result") (first x)))

(defn e2 [x]
  (str/upper-case #_(t/cast string?) (:name x)))

;; --- when and some?
(s/fdef n9 :args (s/cat :u ::user) :ret (s/nilable string?))
(defn n9 [u]
  (let [n (:u/name u)]
    (when (some? n) (str/trim n))))

(s/fdef n10 :args (s/cat :u ::user) :ret string?)
(defn n10 [u]
  (let [n (:u/name u)]
    (when n (println n))
    (str/trim n)))                                ; ERR n may be nil after the when
