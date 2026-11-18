(ns bad-lifecycle
  (:require [system :as sys] [todo.repo :as repo]))

;; Static-only fixture: the contract must bind ctx and inspect the macro body.
;; Expected TSP2345 for numeric title, not merely an unknown-context warning.
(sys/defstart [ctx config]
  (repo/create-todo ctx 42)
  {})
