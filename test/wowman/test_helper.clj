(ns wowman.test-helper
  (:require
   [envvar.core :refer [env with-env]]
   [taoensso.timbre :as timbre :refer [debug info warn error spy]]
   [me.raynes.fs :as fs :refer [with-cwd]]
   [clj-http.fake :refer [with-fake-routes-in-isolation]]
   [wowman
    [main :as main]
    [utils :as utils]]))

(def fixture-dir (-> "test/fixtures" fs/absolute fs/normalized str))

(def data-dir "data/wowman")

(def config-dir "config/wowman")

(defn fixture-path
  [filename]
  (utils/join fixture-dir filename))

(defn fixture-tempcwd
  "each test is executed in a new and self-contained location, accessible as fs/*cwd*
  if the app is started:
  * an empty catalog is downloaded
  * fake wowman version data is downloaded"
  [f]
  (let [temp-dir-path (fs/temp-dir "wowman.main-test.")
        fake-routes {;; catalog
                     ;; return dummy data. we can do this because the catalog isn't loaded/parsed/validated
                     ;; until the UI (gui or cli) tells it to via a later call to `refresh`
                     "https://raw.githubusercontent.com/ogri-la/wowman-data/master/short-catalog.json"
                     {:get (fn [req] {:status 200 :body "{}"})}

                     "https://raw.githubusercontent.com/ogri-la/wowman-data/master/full-catalog.json"
                     {:get (fn [req] {:status 200 :body "{}"})}

                     ;; latest wowman version
                     "https://api.github.com/repos/ogri-la/wowman/releases/latest"
                     {:get (fn [req] {:status 200 :body "{\"tag_name\": \"0.0.0\"}"})}}]
    (try
      (debug "stopping application if it hasn't already been stopped")
      (main/stop)

      (with-fake-routes-in-isolation fake-routes
        (with-env [:xdg-data-home (utils/join temp-dir-path data-dir)
                   :xdg-config-home (utils/join temp-dir-path config-dir)]
          (with-cwd temp-dir-path
            (debug "created temp working directory" fs/*cwd*)
            (f))))
      (finally
        (debug "destroying temp working directory" temp-dir-path) ;; "with contents" (vec (file-seq fs/*cwd*)))
        (fs/delete-dir temp-dir-path)))))

(defmacro with-running-app
  [& form]
  `(try
     (main/start)
     ~@form
     (finally
       (main/stop))))

;; usage:
;; (:require [wowman.helper :as helper])
;; (use-fixtures :each helper/fixture-tempcwd)
