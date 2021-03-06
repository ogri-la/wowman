(ns strongbox.constants)

;; https://wow.gamepedia.com/Wowpedia
(def release-of-previous-expansion
  "'Battle for Azeroth (BfA)', released August 14th 2018. Used to shorten the 'full' catalogue.
  https://en.wikipedia.org/wiki/World_of_Warcraft#Expansions"
  "2018-08-14T00:00:00Z")

;; used as a placeholder for an addon's supported version when we're forced to guess.
;; don't fret too much about patch versions. These values don't affect much.
;; https://wowpedia.fandom.com/wiki/Public_client_builds
(def latest-retail-game-version "9.0.5")
(def latest-classic-game-version "1.13.7")
(def latest-classic-tbc-game-version "2.5.1")

;; interface version to use if .toc file is missing one.
;; assume addon is compatible with the most recent version of retail (see above).
(def default-interface-version 90500)
(def default-interface-version-classic 11300)

(def bullet "\u2022") ;; •
