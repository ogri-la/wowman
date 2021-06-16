(ns strongbox.config-test
  (:require
   [clojure.test :refer [deftest testing is use-fixtures]]
   [me.raynes.fs :as fs]
   ;;[taoensso.timbre :as log :refer [debug info warn error spy]]
   [strongbox
    [utils :as utils]
    [test-helper :as helper :refer [fixture-path]]
    [config :as config]]))

(use-fixtures :each helper/fixture-tempcwd)

(defn temp-addon-dirs-fixture
  "creates a set of addon directories that match those in the `user-config-x.x.json` fixtures"
  [f]
  (let [abs-path-list (mapv #(utils/join (fs/tmpdir) (str ".strongbox-" %)) ["foo" "bar"])
        ;; linting complains if results are unused
        _ (mapv fs/mkdir abs-path-list)]
    (try
      (f)
      (finally
        (mapv fs/delete-dir abs-path-list)))))

(use-fixtures :once temp-addon-dirs-fixture)

;;

(deftest handle-install-dir
  (testing "`:install-dir` in user config is converted to an `:addon-dir`"
    (let [install-dir (str fs/*cwd*)
          cfg {:install-dir install-dir :addon-dir-list []}
          expected {:addon-dir-list [{:addon-dir install-dir :game-track :retail}]}]
      (is (= expected (config/handle-install-dir cfg)))))

  (testing "if both `:install-dir` and `:addon-dir-list` exist, `:install-dir` is appended to `:addon-dir-list` and then dropped"
    (let [install-dir1 "/foo"
          install-dir2 "/bar"

          addon-dir1 {:addon-dir install-dir1 :game-track :retail}
          addon-dir2 {:addon-dir install-dir2 :game-track :retail}

          cfg {:install-dir install-dir2
               :addon-dir-list [addon-dir1]}

          expected {:addon-dir-list [addon-dir1 addon-dir2]}]
      (is (= expected (config/handle-install-dir cfg))))))

(deftest merge-config
  (testing "when called with no overrides, `merge-config` gives us whatever is in the state template"
    (let [expected config/default-cfg
          file-opts {}
          cli-opts {}]
      (is (= expected (config/merge-config file-opts cli-opts)))))

  (testing "config file overrides in the file are preserved and unknown keys are removed"
    (let [cli-opts {}
          file-opts {:foo "bar" ;; unknown
                     :selected-catalogue :full} ;; known
          expected (assoc config/default-cfg :selected-catalogue :full)]
      (is (= expected (config/merge-config file-opts cli-opts)))))

  (testing "cli overrides are preserved and unknown keys are removed"
    (let [cli-opts {:foo "bar"
                    :selected-catalogue :full}
          file-opts {}
          expected (assoc config/default-cfg :selected-catalogue :full)]
      (is (= expected (config/merge-config file-opts cli-opts)))))

  (testing "cli options override file options"
    (let [cli-opts {:selected-catalogue :short}
          file-opts {:selected-catalogue :full}
          expected (assoc config/default-cfg :selected-catalogue :short)]
      (is (= expected (config/merge-config file-opts cli-opts))))))

(deftest catalogue-location-list
  ;; this test is replicated all over the place but is here explicitly just for clarity
  (testing "default catalogue source list used if none defined in user config"
    (let [cli-opts {}
          file-opts {}
          expected config/default-cfg]
      (is (= expected (config/merge-config file-opts cli-opts)))))

  (testing "empty catalogue source list in user config is preserved"
    (let [cli-opts {}
          file-opts {:catalogue-location-list []}
          expected (assoc config/default-cfg :catalogue-location-list [])]
      (is (= expected (config/merge-config file-opts cli-opts)))))

  (testing "invalid catalogue-location entries are removed"
    (let [cli-opts {}
          ;; all invalid cases result in an empty catalogue-location-list
          expected (assoc config/default-cfg :catalogue-location-list [])
          cases [:foo ;; non-list
                 {} ;; non-list
                 [:foo] ;; non-map in list
                 [{}] ;; empty map in list
                 [{:foo :bar}]] ;; invalid map in list
          ]
      (doseq [bad-catalogue-location-list cases]
        (is (= expected (config/merge-config
                         {:catalogue-location-list bad-catalogue-location-list}
                         cli-opts))))))

  (testing "valid catalogue-location entries are preserved"
    (let [cli-opts {}
          valid-source-location {:name :short :label "Short" :source "https://example.org/foo/bar"}
          expected (assoc config/default-cfg :catalogue-location-list [valid-source-location])
          mixed-catalogue-location-list [{}
                                         :foo
                                         valid-source-location
                                         :bar]]
      (is (= expected (config/merge-config
                       {:catalogue-location-list mixed-catalogue-location-list}
                       cli-opts))))))

(deftest invalid-addon-dirs-in-cfg
  (testing "missing directories don't nuke entire config"
    (let [cli-opts {}
          file-opts {:addon-dir-list [{:addon-dir "/does/not/exist"
                                       :game-track :retail}
                                      {:addon-dir (str fs/*cwd*) ;; exists
                                       :game-track :classic}]}
          expected (assoc config/default-cfg
                          :addon-dir-list [{:addon-dir (str fs/*cwd*)
                                            :game-track :classic
                                            :strict? true}]
                          :selected-addon-dir (str fs/*cwd*))]
      (is (= expected (config/merge-config file-opts cli-opts))))))

(deftest handle-selected-addon-dir
  (testing "all cases are invalid and return nil unless there is an `:addon-dir-list` to test membership"
    (let [invalid-cases
          [[nil nil]
           ["" nil]
           [:foo nil]
           [{} nil]
           [[] nil]
           ;; valid ::addon-dir, but no addon-dir-list to check membership
           [(str fs/*cwd*) nil]]]
      (doseq [[given expected] invalid-cases]
        (is (= {:selected-addon-dir expected} (config/handle-selected-addon-dir {:selected-addon-dir given}))))))

  (testing "`:selected-addon-dir` should be `nil` if `addon-dir-list` is present but empty"
    (let [given {:addon-dir-list []
                 :selected-addon-dir (str fs/*cwd*)}
          expected {:addon-dir-list []
                    :selected-addon-dir nil}]
      (is (= expected (config/handle-selected-addon-dir given)))))

  (testing "`:selected-addon-dir` must still be valid, even if the addon dir in the `:addon-dir-list` exists"
    (let [given {:addon-dir-list [{:addon-dir "/does/not/exist"
                                   :game-track :retail}
                                  {:addon-dir (str fs/*cwd*) ;; exists
                                   :game-track :classic}]
                 :selected-addon-dir "/does/not/exist"}

          expected {:addon-dir-list [{:addon-dir "/does/not/exist"
                                      :game-track :retail}
                                     {:addon-dir (str fs/*cwd*) ;; exists
                                      :game-track :classic}]
                    :selected-addon-dir nil}]
      (is (= expected (config/handle-selected-addon-dir given))))))

(deftest convert-compound-game-track
  (let [cases [[:retail :retail]
               [:classic :classic]
               [:retail-classic :retail]
               [:classic-retail :classic]
               [:classic-tbc :classic-tbc]]]
    (doseq [[given expected] cases]
      (is (= expected (config/convert-compound-game-track given))))))

;; ---

(deftest load-settings-0.9
  (testing "a standard config file circa 0.9 is loaded and parsed as expected"
    (let [cli-opts {}
          cfg-file (fixture-path "user-config-0.9.json")
          etag-db-file (fixture-path "empty-map.json")

          expected {:cfg {:gui-theme :light ;; new in 0.11
                          :selected-catalogue :short ;; new in 0.10
                          ;; :debug? true ;; removed in 0.12
                          ;; new in 0.12
                          :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail, :strict? true}
                                           {:addon-dir "/tmp/.strongbox-foo", :game-track :classic, :strict? true}]
                          ;; new in 1.0
                          :catalogue-location-list (:catalogue-location-list config/default-cfg)

                          ;; new in 0.12
                          ;; moved to :cfg in 1.0
                          :selected-addon-dir "/tmp/.strongbox-bar" ;; defaults to first entry

                          ;; new in 3.1.0
                          :preferences {;; new in 3.1.0
                                        :addon-zips-to-keep nil
                                        ;; new in 4.2.0
                                        :automatic-update-all false}}

                    :cli-opts {}
                    :file-opts {:debug? true
                                :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail}
                                                 {:addon-dir "/tmp/.strongbox-foo", :game-track :classic}]}
                    :etag-db {}}]

      (is (= expected (config/load-settings cli-opts cfg-file etag-db-file))))))

(deftest load-settings-0.10
  (testing "a standard config file circa 0.10 is loaded and parsed as expected"
    (let [cli-opts {}
          cfg-file (fixture-path "user-config-0.10.json")
          etag-db-file (fixture-path "empty-map.json")

          expected {:cfg {:gui-theme :light ;; new in 0.11
                          :selected-catalogue :full ;; new in 0.10
                          ;; :debug? true ;; removed in 0.12
                          :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail, :strict? true}
                                           {:addon-dir "/tmp/.strongbox-foo", :game-track :classic, :strict? true}]
                          ;; new in 1.0
                          :catalogue-location-list (:catalogue-location-list config/default-cfg)

                          ;; new in 0.12
                          ;; moved to :cfg in 1.0
                          :selected-addon-dir "/tmp/.strongbox-bar"

                          ;; new in 3.1.0
                          :preferences {;; new in 3.1.0
                                        :addon-zips-to-keep nil
                                        ;; new in 4.2.0
                                        :automatic-update-all false}}

                    :cli-opts {}
                    :file-opts {:selected-catalogue :full
                                :debug? true
                                :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail}
                                                 {:addon-dir "/tmp/.strongbox-foo", :game-track :classic}]}
                    :etag-db {}}]

      (is (= expected (config/load-settings cli-opts cfg-file etag-db-file))))))

(deftest load-settings-0.11
  (testing "a standard config file circa 0.11 is loaded and parsed as expected"
    (let [cli-opts {}
          cfg-file (fixture-path "user-config-0.11.json")
          etag-db-file (fixture-path "empty-map.json")

          expected {:cfg {:gui-theme :dark ;; new in 0.11
                          :selected-catalogue :full ;; new in 0.10
                          ;;:debug? true ;; removed in 0.12
                          :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail, :strict? true}
                                           {:addon-dir "/tmp/.strongbox-foo", :game-track :classic, :strict? true}]
                          ;; new in 1.0
                          :catalogue-location-list (:catalogue-location-list config/default-cfg)

                          ;; new in 0.12
                          ;; moved to :cfg in 1.0
                          :selected-addon-dir "/tmp/.strongbox-bar"

                          ;; new in 3.1.0
                          :preferences {;; new in 3.1.0
                                        :addon-zips-to-keep nil
                                        ;; new in 4.2.0
                                        :automatic-update-all false}}

                    :cli-opts {}
                    :file-opts {:gui-theme :dark
                                :selected-catalogue :full
                                :debug? true
                                :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail}
                                                 {:addon-dir "/tmp/.strongbox-foo", :game-track :classic}]}
                    :etag-db {}}]
      (is (= expected (config/load-settings cli-opts cfg-file etag-db-file))))))

(deftest load-settings-0.12
  (testing "a standard config file circa 0.12 is loaded and parsed as expected"
    (let [cli-opts {}
          cfg-file (fixture-path "user-config-0.12.json")
          etag-db-file (fixture-path "empty-map.json")

          expected {:cfg {:gui-theme :dark ;; new in 0.11
                          :selected-catalogue :full ;; new in 0.10
                          ;;:debug? true ;; removed in 0.12
                          :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail, :strict? true}
                                           {:addon-dir "/tmp/.strongbox-foo", :game-track :classic, :strict? true}]
                          ;; new in 1.0
                          :catalogue-location-list (:catalogue-location-list config/default-cfg)

                          ;; new in 0.12
                          ;; moved to :cfg in 1.0
                          :selected-addon-dir "/tmp/.strongbox-bar"

                          ;; new in 3.1.0
                          :preferences {;; new in 3.1.0
                                        :addon-zips-to-keep nil
                                        ;; new in 4.2.0
                                        :automatic-update-all false}}

                    :cli-opts {}
                    :file-opts {:gui-theme :dark
                                :selected-catalogue :full
                                :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail}
                                                 {:addon-dir "/tmp/.strongbox-foo", :game-track :classic}]}
                    :etag-db {}}]
      (is (= expected (config/load-settings cli-opts cfg-file etag-db-file))))))

(deftest load-settings-1.0
  (testing "a standard config file circa 1.0 is loaded and parsed as expected"
    (let [cli-opts {}
          cfg-file (fixture-path "user-config-1.0.json")
          etag-db-file (fixture-path "empty-map.json")

          expected {:cfg {:gui-theme :dark ;; new in 0.11
                          :selected-catalogue :full ;; new in 0.10
                          ;;:debug? true ;; removed in 0.12
                          :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail, :strict? true}
                                           {:addon-dir "/tmp/.strongbox-foo", :game-track :classic, :strict? true}]

                          ;; new in 1.0
                          :catalogue-location-list (:catalogue-location-list config/default-cfg)

                          ;; new in 0.12
                          ;; moved to :cfg in 1.0
                          :selected-addon-dir "/tmp/.strongbox-foo"

                          ;; new in 3.1.0
                          :preferences {;; new in 3.1.0
                                        :addon-zips-to-keep nil
                                        ;; new in 4.2.0
                                        :automatic-update-all false}}

                    :cli-opts {}
                    :file-opts {:gui-theme :dark
                                :selected-catalogue :full
                                :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail}
                                                 {:addon-dir "/tmp/.strongbox-foo", :game-track :classic}]
                                :selected-addon-dir "/tmp/.strongbox-foo"
                                :catalogue-location-list (:catalogue-location-list config/default-cfg)}
                    :etag-db {}}]
      (is (= expected (config/load-settings cli-opts cfg-file etag-db-file))))))

(deftest load-settings-3.1
  (testing "a standard config file circa 3.1 is loaded and parsed as expected"
    (let [cli-opts {}
          cfg-file (fixture-path "user-config-3.1.json")
          etag-db-file (fixture-path "empty-map.json")

          expected {:cfg {:gui-theme :dark ;; new in 0.11
                          :selected-catalogue :full ;; new in 0.10
                          ;;:debug? true ;; removed in 0.12
                          :addon-dir-list [;;{:addon-dir "/tmp/.strongbox-bar", :game-track :retail-classic} ;; compound game tracks added in 3.1
                                           {:addon-dir "/tmp/.strongbox-bar", :game-track :retail :strict? false} ;; compound game tracks removed in 4.1
                                           {:addon-dir "/tmp/.strongbox-foo", :game-track :classic :strict? true}]

                          ;; new in 1.0
                          :catalogue-location-list (:catalogue-location-list config/default-cfg)

                          ;; new in 0.12
                          ;; moved to :cfg in 1.0
                          :selected-addon-dir "/tmp/.strongbox-foo"

                          ;; new in 3.1.0
                          :preferences {;; new in 3.1.0
                                        :addon-zips-to-keep 3
                                        ;; new in 4.2.0
                                        :automatic-update-all false}}

                    :cli-opts {}
                    :file-opts {:gui-theme :dark
                                :selected-catalogue :full
                                :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail-classic}
                                                 {:addon-dir "/tmp/.strongbox-foo", :game-track :classic}]
                                :selected-addon-dir "/tmp/.strongbox-foo"
                                :catalogue-location-list (:catalogue-location-list config/default-cfg)

                                :preferences {:addon-zips-to-keep 3}}

                    :etag-db {}}]
      (is (= expected (config/load-settings cli-opts cfg-file etag-db-file))))))

(deftest load-settings-3.2
  (testing "a standard config file circa 3.2 is loaded and parsed as expected"
    (let [cli-opts {}
          cfg-file (fixture-path "user-config-3.2.json")
          etag-db-file (fixture-path "empty-map.json")

          expected {:cfg {:gui-theme :dark-green ;; new in 0.11, `:dark-green` new in 3.2.0
                          :selected-catalogue :full ;; new in 0.10
                          ;;:debug? true ;; removed in 0.12
                          :addon-dir-list [;;{:addon-dir "/tmp/.strongbox-bar", :game-track :retail-classic} ;; compound game tracks added in 3.1
                                           {:addon-dir "/tmp/.strongbox-bar", :game-track :retail :strict? false} ;; compound game tracks removed in 4.1
                                           {:addon-dir "/tmp/.strongbox-foo", :game-track :classic :strict? true}]

                          ;; new in 1.0
                          :catalogue-location-list (:catalogue-location-list config/default-cfg)

                          ;; new in 0.12
                          ;; moved to :cfg in 1.0
                          :selected-addon-dir "/tmp/.strongbox-foo"

                          ;; new in 3.1.0
                          :preferences {;; new in 3.1.0
                                        :addon-zips-to-keep 3
                                        ;; new in 4.2.0
                                        :automatic-update-all false}}

                    :cli-opts {}
                    :file-opts {:gui-theme :dark-green
                                :selected-catalogue :full
                                :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :retail-classic}
                                                 {:addon-dir "/tmp/.strongbox-foo", :game-track :classic}]
                                :selected-addon-dir "/tmp/.strongbox-foo"
                                :catalogue-location-list (:catalogue-location-list config/default-cfg)
                                :preferences {:addon-zips-to-keep 3}}
                    :etag-db {}}]
      (is (= expected (config/load-settings cli-opts cfg-file etag-db-file))))))

(deftest load-settings-4.1
  (testing "a standard config file circa 4.1 is loaded and parsed as expected"
    (let [cli-opts {}
          cfg-file (fixture-path "user-config-4.1.json")
          etag-db-file (fixture-path "empty-map.json")

          expected {:cfg {:gui-theme :dark-green ;; new in 0.11, `:dark-green` new in 3.2.0
                          :selected-catalogue :full ;; new in 0.10
                          ;;:debug? true ;; removed in 0.12
                          :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :classic-tbc :strict? true} ;; `:classic-tbc` and `:strict?` added in 4.1
                                           {:addon-dir "/tmp/.strongbox-foo", :game-track :retail :strict? false}]

                          ;; new in 1.0
                          :catalogue-location-list (:catalogue-location-list config/default-cfg)

                          ;; new in 0.12
                          ;; moved to :cfg in 1.0
                          :selected-addon-dir "/tmp/.strongbox-foo"

                          ;; new in 3.1.0
                          :preferences {;; new in 3.1.0
                                        :addon-zips-to-keep 3
                                        ;; new in 4.2.0
                                        :automatic-update-all false}}

                    :cli-opts {}
                    :file-opts {:gui-theme :dark-green
                                :selected-catalogue :full
                                :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :classic-tbc, :strict? true}
                                                 {:addon-dir "/tmp/.strongbox-foo", :game-track :retail, :strict? false}]
                                :selected-addon-dir "/tmp/.strongbox-foo"
                                :catalogue-location-list (:catalogue-location-list config/default-cfg)
                                :preferences {:addon-zips-to-keep 3}}
                    :etag-db {}}]
      (is (= expected (config/load-settings cli-opts cfg-file etag-db-file))))))

(deftest load-settings-4.2
  (testing "a standard config file circa 4.2 is loaded and parsed as expected"
    (let [cli-opts {}
          cfg-file (fixture-path "user-config-4.2.json")
          etag-db-file (fixture-path "empty-map.json")

          expected {:cfg {:gui-theme :dark-green ;; new in 0.11, `:dark-green` new in 3.2.0
                          :selected-catalogue :full ;; new in 0.10
                          ;;:debug? true ;; removed in 0.12
                          :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :classic-tbc :strict? true} ;; `:classic-tbc` and `:strict?` added in 4.1
                                           {:addon-dir "/tmp/.strongbox-foo", :game-track :retail :strict? false}]

                          ;; new in 1.0
                          :catalogue-location-list (:catalogue-location-list config/default-cfg)

                          ;; new in 0.12
                          ;; moved to :cfg in 1.0
                          :selected-addon-dir "/tmp/.strongbox-foo"

                          ;; new in 3.1.0
                          :preferences {;; new in 3.1.0
                                        :addon-zips-to-keep 3
                                        ;; new in 4.2.0
                                        :automatic-update-all true}}

                    :cli-opts {}
                    :file-opts {:gui-theme :dark-green
                                :selected-catalogue :full
                                :addon-dir-list [{:addon-dir "/tmp/.strongbox-bar", :game-track :classic-tbc, :strict? true}
                                                 {:addon-dir "/tmp/.strongbox-foo", :game-track :retail, :strict? false}]
                                :selected-addon-dir "/tmp/.strongbox-foo"
                                :catalogue-location-list (:catalogue-location-list config/default-cfg)

                                :preferences {:addon-zips-to-keep 3
                                              :automatic-update-all true}}

                    :etag-db {}}]
      (is (= expected (config/load-settings cli-opts cfg-file etag-db-file))))))
