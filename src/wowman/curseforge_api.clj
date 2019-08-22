(ns wowman.curseforge-api
  (:require
   [wowman
    [http :as http]
    [specs :as sp]
    [utils :as utils :refer [to-int to-json fmap join from-epoch to-uri]]]
   [slugify.core :refer [slugify]]
   [clojure.spec.alpha :as s]
   [orchestra.spec.test :as st]
   [orchestra.core :refer [defn-spec]]
   [taoensso.timbre :as log :refer [debug info warn error spy]]))

(def curseforge-api "https://addons-ecs.forgesvc.net/api/v2")

(defn-spec api-uri ::sp/uri
  [path string?, & args (s/* any?)]
  (str curseforge-api (apply format path args)))

(defn-spec expand-summary (s/or :ok ::sp/addon, :error nil?)
  "given a summary, adds the remaining attributes that couldn't be gleaned from the summary page. one additional look-up per ::addon required"
  [addon-summary ::sp/addon-summary]
  (let [pid (-> addon-summary :source-id)
        uri (api-uri "/addon/%s" pid)
        result (-> uri http/download utils/from-json)
        ;; TODO: this is no longer good enough. we now need to differentiate between regular ('retail') and classic
        ;; see :gameVersionFlavor
        ;;latest-release (-> result :latestFiles first) ;; this list isn't sorted!
        ;; also, :latestFiles seems to be a selection of all files.
        ;; these files (I think) are selected by release type (alpha/beta/released etc), padded out with the last N proper releases
        latest-release (->> result :latestFiles (sort-by :fileDate) last)

        ;; api value is empty in some cases (carbonite, improved loot frames, skada damage meter)
        ;; this value overrides the one found in .toc files, so if it can't be scraped, use the .toc version
        interface-version (some-> latest-release :gameVersion first utils/game-version-to-interface-version)
        interface-version (when interface-version {:interface-version interface-version})

        details {:download-uri (:downloadUrl latest-release)
                 :version (:displayName latest-release)}]
    (merge addon-summary details interface-version)))

(defn-spec extract-addon-summary ::sp/addon-summary
  "converts addon data extracted from a listing into an ::sp/addon-summary"
  [snippet map?] ;; TODO: spec out curseforge results? eh.
  {:uri (:websiteUrl snippet)
   :label (:name snippet)
   :name (:slug snippet)
   ;; to be removed. helps with fuzzy matching toc data but should be generated by the catalog if we want to keep it
   :alt-name (-> snippet :name (slugify ""))
   :description (:summary snippet)
   ;; I don't think order is significant. it certainly differs between website and api
   ;; this cuts down on noise in diffs
   :category-list (sort (mapv :name (:categories snippet)))
   :created-date (:dateCreated snippet) ;; omg *yes*. perfectly formed dates
   ;; we now have :dateModified and :dateReleased to pick from
   ;; :dateReleased (:fileDate of latest release) appears to be closest to what was being scraped
   ;;:updated-date (:dateModified snippet)
   :updated-date (:dateReleased snippet)
   :download-count (-> snippet :downloadCount int) ;; I got a '511.0' ...?
   :source-id (:id snippet) ;; I imagine wowinterface will have its own as well
   })

(defn-spec download-summary-page-alphabetically (s/or :ok (s/coll-of map?), :error nil?)
  "downloads a page of results from the curseforge API, sorted A to Z"
  [page int? page-size pos-int?]
  (info "downloading" page-size "results from api, page" page)
  (let [index (* page-size page) ;; +1 ?
        game-id 1 ;; WoW
        sort-by 3 ;; alphabetically, asc (a-z)
        results (http/download (api-uri "/addon/search?gameId=%s&index=%s&pageSize=%s&searchFilter=&sort=%s" game-id index page-size sort-by))
        results (utils/from-json results)]
    (mapv extract-addon-summary results)))

(defn-spec download-all-summaries-alphabetically (s/or :ok ::sp/addon-summary-list, :error nil?)
  []
  (loop [page 0
         accumulator []]
    (let [page-size 255
          results (download-summary-page-alphabetically page page-size)
          num-results (count results)]
      (if (< num-results page-size)
        (into accumulator results) ;; short page, exit loop
        (recur (inc page)
               (into accumulator results))))))

(st/instrument)
