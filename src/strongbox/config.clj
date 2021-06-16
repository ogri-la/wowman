(ns strongbox.config
  (:require
   [clojure.spec.alpha :as s]
   [clojure.set]
   [orchestra.core :refer [defn-spec]]
   [taoensso.timbre :as timbre :refer [debug info warn error spy]]
   [clojure.string]
   [strongbox
    [specs :as sp]
    [utils :as utils]]))

;; if the user provides their own catalogue list in their config file, it will override these defaults entirely
;; if the `:catalogue-location-list` entry is *missing* in the user config file, these will be used instead.
;; to use strongbox with no catalogues at all, use `:catalogue-location-list []` (empty list)
(def -default-catalogue-list
  [{:name :short :label "Short (default)" :source "https://raw.githubusercontent.com/ogri-la/strongbox-catalogue/master/short-catalogue.json"}
   {:name :full :label "Full" :source "https://raw.githubusercontent.com/ogri-la/strongbox-catalogue/master/full-catalogue.json"}
   ;; ---
   {:name :tukui :label "Tukui" :source "https://raw.githubusercontent.com/ogri-la/strongbox-catalogue/master/tukui-catalogue.json"}
   {:name :curseforge :label "Curseforge" :source "https://raw.githubusercontent.com/ogri-la/strongbox-catalogue/master/curseforge-catalogue.json"}
   {:name :wowinterface :label "WoWInterface" :source "https://raw.githubusercontent.com/ogri-la/strongbox-catalogue/master/wowinterface-catalogue.json"}])

(def default-cfg
  {:addon-dir-list []
   :selected-addon-dir nil
   :catalogue-location-list -default-catalogue-list
   :selected-catalogue :short
   :gui-theme :light
   :preferences {;; nil: keep all zips (default)
                 ;; 0:   keep no zips
                 ;; 1:   keep 1 zip
                 ;; N:   keep N zips
                 :addon-zips-to-keep nil
                 ;; true:  begin updating addons immediately
                 ;; false: user must hit 'update all' button
                 :automatic-update-all false
                 }})

(defn handle-install-dir
  "`:install-dir` was once supported in the user configuration but is now only supported in the command line options.
  this function expands `:install-dir` to an `::sp/addon-dir-map`, if present, and drops the `:install-dir` key"
  [cfg]
  (let [install-dir (:install-dir cfg)
        addon-dir-list (->> cfg :addon-dir-list (mapv :addon-dir))
        stub {:addon-dir install-dir :game-track :retail}
        ;; add stub to addon-dir-list if install-dir isn't nil and doesn't match anything already present
        cfg (if (and install-dir
                     (not (utils/in? install-dir addon-dir-list)))
              (update-in cfg [:addon-dir-list] conj stub)
              cfg)]
    ;; finally, ensure :install-dir is absent from whatever we return
    (dissoc cfg :install-dir)))

(defn-spec convert-compound-game-track ::sp/game-track
  "takes any game track, new or old, and returns the new version.
  for example: `:classic` => `:classic` and `:classic-retail` => `:classic`"
  [game-track (s/or :new-game-track ::sp/game-track, :old-game-track ::sp/old-game-track)]
  (if (some #{game-track} sp/old-game-tracks)
    (-> game-track name (clojure.string/split #"\-") first keyword)
    game-track))

(defn handle-compound-game-tracks
  "addon game track leniency is now handled through the flag `strict?` rather than encoded into the game track.
  default strictness is `true`."
  [cfg]
  (if-let [addon-dir-list (:addon-dir-list cfg)]
    (let [updater (fn [addon-dir-map]
                    (merge addon-dir-map
                           ;; a :game-track will always be present. see `handle-install-dir`
                           {:game-track (convert-compound-game-track (:game-track addon-dir-map))
                            :strict? (get addon-dir-map :strict?
                                          (not (utils/in? (:game-track addon-dir-map) sp/old-game-tracks)))}))]
      (assoc cfg :addon-dir-list (mapv updater addon-dir-list)))
    cfg))

(defn-spec valid-catalogue-location? boolean?
  "returns true if given `catalogue-location` is a valid `:catalogue/location`"
  [catalogue-location any?]
  (let [valid (s/valid? :catalogue/location catalogue-location)]
    (when-not valid
      (warn "invalid catalogue source, discarding:" catalogue-location)
      (debug (s/explain-str :catalogue/location catalogue-location)))
    valid))

(defn remove-invalid-catalogue-location-entries
  "removes invalid `:catalogue-location-map` entries in `:catalogue-location-list`"
  [cfg]
  (if-let [csl (:catalogue-location-list cfg)]
    (if (not (vector? csl))
      ;; we have something, but whatever we were given it wasn't a vector. non-starter
      (assoc cfg :catalogue-location-list [])

      ;; strip anything that isn't valid
      (assoc cfg :catalogue-location-list (filterv valid-catalogue-location? csl)))

    ;; key not present, return config as-is
    cfg))

(defn remove-invalid-addon-dirs
  "removes any `addon-dir-map` items from the given configuration whose directories do not exist"
  [cfg]
  (assoc cfg :addon-dir-list (filterv #(s/valid? ::sp/addon-dir-map %) (:addon-dir-list cfg []))))

(defn handle-selected-addon-dir
  "ensures the `:selected-addon-dir` value is valid and present in the list of addon directories"
  [cfg]
  (let [;; it shouldn't happen but ensure the default addon dir is valid or nil
        default-selected-addon-dir (->> cfg :addon-dir-list first :addon-dir (sp/valid-or-nil ::sp/addon-dir))
        selected-addon-dir (:selected-addon-dir cfg)
        selected-addon-dir (and
                            ;; dir exists
                            (sp/valid-or-nil ::sp/addon-dir selected-addon-dir)
                            ;; dir is present in available addon dirs
                            (some #{selected-addon-dir} (map :addon-dir (:addon-dir-list cfg))))]
    (assoc cfg :selected-addon-dir (or selected-addon-dir default-selected-addon-dir))))

;; todo: rather than removing keys before validation (wtf?),
;; create a different or sub-spec where these values are optional
(defn strip-unspecced-keys
  "removes any keys from the given configuration that are not in the spec"
  [cfg]
  ;;(spec-tools/coerce ::sp/user-config cfg spec-tools/strip-extra-keys-transformer))
  ;; `select-keys` is not as good as the above `spec-tools/coerce` approach, but:
  ;; * saves about 1.5MB of dependencies
  ;; * it wasn't doing validation, just stripping extra keys
  ;; * it wasn't doing any conforming of values (like strings to integers or keywords)
  ;; * it didn't support :opt(ional) keysets
  (select-keys cfg [:addon-dir-list :selected-addon-dir
                    :gui-theme
                    :catalogue-location-list :selected-catalogue
                    :preferences]))

;; https://dnaeon.github.io/recursively-merging-maps-in-clojure/
(defn deep-merge
  "Recursively merges maps."
  [& maps]
  (letfn [(m [& xs]
            (if (some #(and (map? %) (not (record? %))) xs)
              (apply merge-with m xs)
              (last xs)))]
    (reduce m maps)))

(defn-spec -merge-with ::sp/user-config
  "merges `cfg-b` over `cfg-a`, returning the result if valid else `cfg-a`"
  [cfg-a map?, cfg-b map?, msg string?]
  (debug "loading config:" msg)
  (let [cfg (-> cfg-a
                ;;(merge cfg-b)
                (deep-merge cfg-b)
                handle-install-dir
                handle-compound-game-tracks
                remove-invalid-addon-dirs
                handle-selected-addon-dir
                remove-invalid-catalogue-location-entries
                strip-unspecced-keys)
        message (format "configuration from '%s' is invalid and will be ignored: %s"
                        msg (s/explain-str ::sp/user-config cfg))]
    (if (s/valid? ::sp/user-config cfg)
      cfg
      (do (warn message) cfg-a))))

(defn-spec merge-config ::sp/user-config
  "merges the default user configuration with settings in the user config file and any CLI options"
  [file-opts map?, cli-opts map?]
  (-> default-cfg
      (-merge-with file-opts "user settings")
      (-merge-with cli-opts "command line options")))

;;

(defn -load-settings
  "returns a map of user configuration settings that can be merged over `core/state`"
  [cli-opts file-opts etag-db]
  (let [cfg (merge-config file-opts cli-opts)]
    {:cfg cfg
     :cli-opts cli-opts
     :file-opts file-opts
     :etag-db etag-db}))

(defn-spec load-settings-file map?
  "reads application settings from the given file.
  returns an empty map if file is missing or malformed."
  [cfg-file ::sp/file]
  (let [opts {:no-file? (fn [] (warn "configuration file not found: " cfg-file) {})
              :bad-data? (fn [] (error "configuration file malformed: " cfg-file) {})
              :transform-map {:selected-catalog keyword ;; becomes `:selected-catalogue`, if present
                              :selected-catalogue keyword
                              :gui-theme keyword
                              :game-track keyword
                              ;; too general, not great :(
                              :name keyword}}
        config (utils/load-json-file-safely cfg-file opts)]
    ;; legacy, 0.10 to 0.12 included a key that needs renaming in 1.0
    (clojure.set/rename-keys config {:selected-catalog :selected-catalogue})))

(defn-spec load-etag-db-file map?
  "reads etag database from given file.
  if file is missing or malformed, returns an empty map"
  [etag-db-file ::sp/file]
  (utils/load-json-file-safely etag-db-file {:no-file? {} :bad-data? {}}))

(defn load-settings
  "reads config files and returns a map of configuration settings that can be merged over `core/state`"
  [cli-opts cfg-file etag-db-file]
  (let [file-opts (load-settings-file cfg-file)
        etag-db (load-etag-db-file etag-db-file)]
    (-load-settings cli-opts file-opts etag-db)))
