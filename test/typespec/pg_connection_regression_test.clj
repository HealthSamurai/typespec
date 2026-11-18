(ns typespec.pg-connection-regression-test
  "Regression from context-clj/pg aa8ea0eb565e7f70f80f3248e6a83198628e1047,
  pg.clj:56-58. Uses JDBC DataSource instead of Hikari to avoid a test dependency.
  This checks the extracted body, not the full upstream namespace or live database."
  (:require [clojure.test :refer [deftest is]]
            [typespec.externs-regression-test :refer [project]]))

(def contracts
  "{pg/datasource [map? :-> javax.sql.DataSource]
    pg/connection [map? :-> java.sql.Connection]}")

(defn source [acquire]
  (str "(ns app (:require [pg]) (:import [java.sql Connection]))
        (defn with-connection {:t/sig '[map? [java.sql.Connection :-> string?] :-> string?]}
          [ctx f]
          (with-open [^Connection conn (" acquire " ctx)]
            (f conn)))"))

(deftest datasource-is-not-a-connection
  (project (source "pg/datasource") contracts {}
           (fn [result]
             (is (some #(and (= "TSP2322" (:code %))
                             (= :error (:severity %))
                             (re-find #"hint Connection contradicts" (:message %)))
                       (:diagnostics result))
                 (pr-str (:diagnostics result))))))

(deftest acquiring-connection-satisfies-contract
  (project (source "pg/connection") contracts {}
           (fn [result]
             (is (empty? (:diagnostics result)) (pr-str (:diagnostics result))))))

(deftest reader-is-not-a-byte-stream
  ;; pg.clj:204-206 creates a Reader, but copy-ndjson-stream requires InputStream.
  ;; Pure static fixture: no files, sockets, PG functions or constructors executed.
  (doseq [[value valid?] [["(BufferedReader. (StringReader. \"{}\"))" false]
                         ["(java.io.ByteArrayInputStream. (byte-array 0))" true]]]
    (project
     (str "(ns app (:require [pg]) (:import [java.io BufferedReader StringReader]))
           (defn copy-data [ctx] (pg/copy-ndjson-stream ctx \"_resource\" " value "))")
     "{pg/copy-ndjson-stream [t/unknown string? java.io.InputStream :-> t/unknown]}" {}
     (fn [result]
       (if valid?
         (is (empty? (:diagnostics result)) (pr-str (:diagnostics result)))
         (is (some #(and (= "TSP2345" (:code %)) (= :error (:severity %))
                         (re-find #"BufferedReader" (:message %)))
                   (:diagnostics result)) (pr-str (:diagnostics result))))))))
