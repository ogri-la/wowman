(defproject ogri-la/strongbox "2.0.1"
  :description "World Of Warcraft Addon Manager"
  :url "https://github.com/ogri-la/strongbox"
  :license {:name "GNU Affero General Public License (AGPL)"
            :url "https://www.gnu.org/licenses/agpl-3.0.en.html"}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/spec.alpha "0.2.176"]
                 [org.clojure/tools.cli "1.0.194"] ;; cli arg parsing
                 [org.clojure/tools.namespace "1.0.0"] ;; reload code
                 [org.clojure/data.json "1.0.0"] ;; json handling
                 [orchestra "2018.12.06-2"] ;; improved clojure.spec instrumentation
                 ;; see lein deps :tree
                 [com.taoensso/timbre "4.10.0"] ;; logging
                 [enlive "1.1.6"] ;; html parsing
                 [clj-http "3.10.1"] ;; better http slurping
                 [seesaw "1.5.0"] ;; swing
                 [clj-commons/fs "1.5.2"] ;; file system wrangling
                 [slugify "0.0.1"]
                 [trptcolin/versioneer "0.2.0"] ;; version number wrangling. it's more involved than you might suspect
                 [org.flatland/ordered "1.5.9"] ;; better ordered map
                 [clojure.java-time "0.3.2"] ;; date/time handling library, https://github.com/dm3/clojure.java-time
                 [envvar "1.1.0"] ;; environment variable wrangling
                 [gui-diff "0.6.7"] ;; pops up a graphical diff for test results
                 [com.taoensso/tufte "2.1.0"]
                 
                 ;; remember to update the LICENCE.txt
                 ;; remember to update pom file (`lein pom`)

                 ]

  ;; java 11 , java-time localisation issue 
  ;;:jvm-opts ["-Djava.locale.providers=COMPAT,CLDR"]

  :profiles {:dev {:dependencies [;; fake http responses for testing
                                  [clj-http-fake "1.0.3"]
                                  ]}
             :uberjar {:aot :all}}

  :main strongbox.main

  :plugins [[lein-cljfmt "0.6.4"]
            [jonase/eastwood "0.3.11"]
            [lein-cloverage "1.1.1"]]
  :eastwood {:exclude-linters [:constant-test]
             ;; linters that are otherwise disabled
             :add-linters [:unused-namespaces
                           :unused-private-vars
                           ;;:unused-locals ;; prefer to keep for readability
                           ;;:unused-fn-args ;; prefer to keep for readability
                           ;;:keyword-typos ;; bugged with spec?
                           ]}
  )
