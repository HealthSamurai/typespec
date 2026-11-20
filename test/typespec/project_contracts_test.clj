(ns typespec.project-contracts-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [typespec.main :as main]))

(defn- fixture-file [root relative source]
  (let [file (io/file root relative)]
    (io/make-parents file)
    (spit file source)))

(deftest project-index-links-public-vars-to-specs-and-uses
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "typespec-project" (make-array java.nio.file.attribute.FileAttribute 0)))
        data-file "src/catalog/schema.clj"
        api-file "src/catalog/api.clj"
        client-file "src/view/page.clj"
        data (fn [field map-spec]
               (str "(ns catalog.schema (:require [clojure.spec.alpha :as s]))\n"
                    "(s/def ::id " field ")\n(s/def ::label string?)\n"
                    "(s/def ::account " map-spec ")"))
        client "(ns view.page (:require [catalog.api :as c :refer [account] :rename {account build}]
                                [clojure.string :as str]))
(str/upper-case (:id (c/account \"raw\")))
(build \"direct\")
(map build [\"row\"])
(partial c/account)
(apply build [\"tail\"])
#'c/account
(let [build identity] (build \"local\"))
'(c/account \"data\")"
        api "(ns catalog.api (:require [catalog.schema :as schema] [clojure.spec.alpha :as s]))
(s/fdef account :args (s/cat :id string?) :ret ::schema/account)
(defn account [id] {:id id})
(defn- hidden [] 1)"
        run #(main/run-check (str root) nil {:track-uses true})]
    (try
      (fixture-file root data-file (data "string?" "(s/keys :req-un [::id])"))
      (fixture-file root api-file api)
      (fixture-file root client-file client)
      (let [r (run)
            entry (first (:contracts r))]
        (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))
        (is (= ["catalog.api/account"] (mapv :var (:contracts r))))
        (is (= {:var "catalog.api/account" :source :fdef :signature "[string? :-> :catalog.schema/account]"}
               (select-keys entry [:var :source :signature])))
        (is (= [api-file 2] [(get-in entry [:declaration :file]) (get-in entry [:declaration :pos :row])]))
        (is (= [":catalog.schema/account" ":catalog.schema/id"] (mapv :name (:specs entry))))
        (is (= "string?" (get-in entry [:specs 1 :type])))
        (is (= [data-file data-file] (mapv #(get-in % [:declaration :file]) (:specs entry))))
        (is (= (repeat 6 client-file) (map :file (:uses entry))))
        (is (= [:call :call :value :value :value :var] (mapv :kind (:uses entry))))
        (is (= [3 4 5 6 7 8] (mapv (comp :row :pos) (:uses entry))))
        (is (= (:contracts r) (:contracts (run)))))
      (fixture-file root data-file (data "int?" "(s/keys :req-un [::id])"))
      (let [r (run)]
        (is (= #{[client-file "TSP2345"] [api-file "TSP2322"]}
               (set (map (juxt :file :code) (:diagnostics r)))) (pr-str (:diagnostics r)))
        (is (= "[string? :-> :catalog.schema/account]" (get-in r [:contracts 0 :signature])))
        (is (= "int?" (get-in r [:contracts 0 :specs 1 :type]))))
      (fixture-file root data-file (data "string?" "(s/keys :req-un [::label])"))
      (let [r (run)]
        (is (some #(= [api-file "TSP2741"] ((juxt :file :code) %)) (:diagnostics r)))
        (is (some #(= [client-file "TSP2339"] ((juxt :file :code) %)) (:diagnostics r))))
      (fixture-file root data-file (data "string?" "(s/keys :req-un [::id])"))
      (fixture-file root api-file
                    "(ns catalog.api (:require [catalog.schema :as schema] [clojure.spec.alpha :as s]))
(s/fdef account :args (s/cat :id string? :label string?) :ret ::schema/account)
(defn account [id label] {:id id})")
      (is (some #(= [client-file "TSP2554"] ((juxt :file :code) %)) (:diagnostics (run))))
      (fixture-file root "src/view/override.clj"
                    "(ns view.override (:require [clojure.spec.alpha :as s]))
(s/def :catalog.schema/id int?)")
      (is (some #(= ["src/view/override.clj" "TSP722-01"] ((juxt :file :code) %)) (:diagnostics (run))))
      (finally
        (doseq [file (reverse (file-seq root))] (io/delete-file file true))))))
