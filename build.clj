(ns build
  (:require [clojure.tools.build.api :as b]))

(defn uber [_]
  (let [basis (b/create-basis {:project "deps.edn" :user nil})
        class-dir "target/uber-classes"]
    (b/delete {:path class-dir})
    (b/compile-clj {:basis basis :class-dir class-dir :ns-compile '[typespec.main]})
    (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
    (b/uber {:basis basis :class-dir class-dir :main 'typespec.main
             :uber-file "target/typespec.jar"})))
