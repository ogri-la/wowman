(ns strongbox.wowinterface-api
  (:require
   [clojure.spec.alpha :as s]
   [orchestra.core :refer [defn-spec]]
   [strongbox
    [utils :as utils]
    [specs :as sp]
    [http :as http]]
   [taoensso.timbre :as log :refer [debug info warn error spy]]))

(def wowinterface-api "https://api.mmoui.com/v3/game/WOW")

(defn-spec expand-summary (s/or :ok :addon/release-list, :error nil?)
  "given a summary, adds the remaining attributes that couldn't be gleaned from the summary page. one additional look-up per addon required"
  [addon-summary :addon/expandable, game-track ::sp/game-track]
  ;; this check is a little different to the others.
  ;; the `game-track-list` is stored in the catalogue for wowinterface because it's available at creation time.
  ;; however! we know this information isn't good and doesn't always match what we see on the website.
  ;; until wowinterface improve, and short of doing more scraping of html, this is the best we can do.
  (when-not (:game-track-list addon-summary)
    (error "given addon-summary has no game track list! please report this if you're not a developer.")
    (error addon-summary))
  (if (some #{game-track} (:game-track-list addon-summary))
    (let [url (str wowinterface-api "/filedetails/" (:source-id addon-summary) ".json")
          result-list (some-> url http/download http/sink-error utils/from-json)
          result (first result-list)]
      (when result
        (when (> (count result-list) 1)
          ;; has this happened before? can we find an example?
          (warn "wowinterface api returned more than one result for addon with id:" (:source-id addon-summary)))
        [{:download-url (str "https://cdn.wowinterface.com/downloads/getfile.php?id=" (:source-id addon-summary))
          :version (:UIVersion result)
          :game-track game-track}]))))

(defn-spec parse-user-string (s/or :ok :addon/source-id :error nil?)
  "extracts the addon ID from the given `url`"
  [url ::sp/url]
  (some->> url java.net.URL. .getPath (re-find #"/(?:info|download){1}(\d+)") second Integer/valueOf))
