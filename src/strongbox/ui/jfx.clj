(ns strongbox.ui.jfx
  (:require
   [me.raynes.fs :as fs]
   [clojure.pprint]
   [clojure.string :refer [lower-case join capitalize replace] :rename {replace str-replace}]
   ;; logging in the gui should be avoided as it can lead to infinite loops
   [taoensso.timbre :as timbre :refer [spy]] ;; info debug warn error]] 
   [cljfx.ext.table-view :as fx.ext.table-view]
   [cljfx.lifecycle :as fx.lifecycle]
   [cljfx.component :as fx.component]
   [cljfx.api :as fx]
   [cljfx.ext.node :as fx.ext.node]
   [cljfx.css :as css]
   [clojure.spec.alpha :as s]
   [orchestra.core :refer [defn-spec]]
   [strongbox.ui.cli :as cli]
   [strongbox
    [logging :as logging]
    [addon :as addon]
    [specs :as sp]
    [utils :as utils :refer [no-new-lines]]
    [core :as core]])
  (:import
   [java.util List Calendar Locale]
   [javafx.util Callback]
   [javafx.scene.control TableRow TextInputDialog Alert Alert$AlertType ButtonType]
   [javafx.scene.input KeyCode]
   [javafx.stage FileChooser FileChooser$ExtensionFilter DirectoryChooser Window WindowEvent]
   [javafx.application Platform]
   [javafx.scene Node]
   [java.text NumberFormat]))

;; javafx hack, fixes combobox that sometimes goes blank:
;; https://github.com/cljfx/cljfx/issues/76#issuecomment-645563116
(def ext-recreate-on-key-changed
  "Extension lifecycle that recreates its component when lifecycle's key is changed
  
  Supported keys:
  - `:key` (required) - a value that determines if returned component should be recreated
  - `:desc` (required) - a component description with additional lifecycle semantics"
  (reify fx.lifecycle/Lifecycle
    (create [_ {:keys [key desc]} opts]
      (with-meta {:key key
                  :child (fx.lifecycle/create fx.lifecycle/dynamic desc opts)}
        {`fx.component/instance #(-> % :child fx.component/instance)}))
    (advance [this component {:keys [key desc] :as this-desc} opts]
      (if (= (:key component) key)
        (update component :child #(fx.lifecycle/advance fx.lifecycle/dynamic % desc opts))
        (do (fx.lifecycle/delete this component opts)
            (fx.lifecycle/create this this-desc opts))))
    (delete [_ component opts]
      (fx.lifecycle/delete fx.lifecycle/dynamic (:child component) opts))))

(def user-locale (Locale/getDefault))
(def number-formatter (NumberFormat/getNumberInstance user-locale))

(def blt "\u2022") ;; • bullet

(def major-theme-map
  {:light
   {:base "#ececec"
    :accent "lightsteelblue"
    :table-border "#bbb"
    :row "-fx-control-inner-background"
    :row-hover "derive(-fx-control-inner-background,-10%)"
    :row-selected "lightsteelblue"
    :unsteady "lightsteelblue"
    :row-updateable "lemonchiffon"
    :row-updateable-selected "#fdfd96" ;; "Lemon Meringue" (brighter yellow)
    :row-updateable-text "black"
    :row-warning "lemonchiffon"
    :row-warning-text "black"
    :row-error "tomato"
    :row-error-text "black"
    :row-report-text "blue"
    :hyperlink "blue"
    :hyperlink-updateable "blue"
    :hyperlink-weight "normal"
    :table-font-colour "-fx-text-base-color"
    :row-alt "-fx-control-inner-background-alt"
    :uber-button-tick "darkseagreen"
    :uber-button-warn "orange"
    :uber-button-error "red"}

   :dark ;; "'dracula' theme: https://github.com/dracula/dracula-theme"
   {:base "#1e1f29"
    :accent "#44475a"
    :table-border "#333"
    :row "#1e1f29" ;; same as :base
    :row-hover "derive(-fx-control-inner-background,-50%)"
    :row-selected "derive(-fx-control-inner-background,-30%)"
    :unsteady "#bbb"
    :row-updateable "#6272a4" ;; (blue)
    :row-updateable-selected "#6272c3" ;; (brighter blue) ;; todo: can this be derived from :row-updateable?
    :row-updateable-text "white"
    :row-warning "#ffb86c"
    :row-warning-text "black"
    :row-error "#ff5555"
    :row-error-text "black"
    :row-report-text "#bd93f9"
    :hyperlink "#f8f8f2"
    :hyperlink-updateable "white"
    :hyperlink-weight "bold"
    :table-font-colour "-fx-text-base-color"
    :row-alt "#22232e"
    :uber-button-tick "aquamarine"
    :uber-button-warn "#ffb86c"
    :uber-button-error "red"}})

(def sub-theme-map
  {:dark
   {:green
    {:row-updateable "#50a67b" ;; (green)
     :row-updateable-selected "#40c762" ;; (brighter green)
     :row-updateable-text "black"
     :hyperlink-updateable "black"}

    :orange
    {:row-updateable "#df8750" ;; (orange)
     :row-updateable-selected "#df722e" ;; (brigher orange)
     :row-updateable-text "black"
     :hyperlink-updateable "black"
     :uber-button-error "brown"}}})

(def themes
  (into major-theme-map
        (for [[major-theme-key sub-theme-val] sub-theme-map
              [sub-theme-key sub-theme] sub-theme-val
              :let [major-theme (get major-theme-map major-theme-key)
                    ;; "dark-green", "dark-orange"
                    theme-name (keyword (format "%s-%s" (name major-theme-key) (name sub-theme-key)))]]
          {theme-name (merge major-theme sub-theme)})))

(defn-spec style map?
  "generates javafx css definitions for the different themes.
  if editor is connected to a running repl session then modifying
  the css will reload the running GUI for immediate feedback."
  []
  (css/register
   ::style
   (let [generate-style
         (fn [theme-kw]
           (let [colour-map (get themes theme-kw)
                 colour #(name (get colour-map % "pink"))]
             {;;
              ;; 'about' dialog
              ;; lives outside of main styling for some reason
              ;;

              ".dialog-pane"
              {:-fx-min-width "500px"}

              ".dialog-pane .content"
              {:-fx-line-spacing "3"}

              "#about-dialog #about-pane-hyperlink"
              {:-fx-font-size "1.1em"
               :-fx-padding "0 0 4 -1"}

              "#about-dialog #about-pane-hyperlink:hover"
              {:-fx-text-fill "blue"}

              "#about-dialog #about-pane-title"
              {:-fx-font-size "1.6em"
               :-fx-font-weight "bold"
               :-fx-padding ".5em 0"}

              ;;
              ;; main app styling
              ;; 

              (format "#%s.root " (name theme-kw))
              {:-fx-padding 0
               :-fx-base (colour :base)
               :-fx-accent (colour :accent) ;; selection colour of backgrounds

               "#main-menu"
               {:-fx-background-color (colour :base)} ;; removes gradient from 'File' menu

               ".combo-box-base"
               {:-fx-background-radius "0"
                ;; truncation now happens from the left. thanks to:
                ;; https://stackoverflow.com/questions/36264656/scalafx-javafx-how-can-i-change-the-overrun-style-of-a-combobox
                " > .list-cell" {:-fx-text-overrun "leading-ellipsis"}}

               ".button"
               {:-fx-background-radius "0"
                :-fx-padding "5px 17px" ;; makes buttons same height as dropdowns
                }


               ;;
               ;; hyperlinks
               ;;


               ".hyperlink"
               {:-fx-underline "false"
                :-fx-font-weight (colour :hyperlink-weight)
                :-fx-text-fill (colour :hyperlink)}


               ;;
               ;; common tables
               ;;


               ".table-view"
               {:-fx-table-cell-border-color (colour :table-border)
                :-fx-font-size ".9em"}

               ".table-view .hyperlink"
               {:-fx-padding "-2 0 0 0"}

               ".table-view .table-placeholder-text"
               {:-fx-font-size "3em"}

               ".table-view .column-header"
               {;;:-fx-background-color "#ddd" ;; flat colour vs gradient
                :-fx-font-size "1em"}

               ".table-view .table-row-cell"
               {:-fx-border-insets "-1 -1 0 -1"
                :-fx-border-color (colour :table-border)

                " .table-cell"
                {:-fx-text-fill (colour :table-font-colour)}

                ;; even
                :-fx-background-color (colour :row)

                ":hover"
                {:-fx-background-color (colour :row-hover)}

                ":selected"
                {:-fx-background-color (colour :row-selected)

                 " .table-cell"
                 {:-fx-text-fill "-fx-focused-text-base-color"}
                 :-fx-table-cell-border-color (colour :table-border)}

                ":selected:hover"
                {:-fx-background-color (colour :row-hover)}

                ":odd"
                {:-fx-background-color (colour :row)}

                ":odd:hover"
                {:-fx-background-color (colour :row-hover)}

                ":odd:selected"
                {:-fx-background-color (colour :row-selected)}

                ":odd:selected:hover"
                {:-fx-background-color (colour :row-hover)}

                ".unsteady"
                {;; '!important' so that it takes precedence over .updateable addons
                 :-fx-background-color (str (colour :unsteady) " !important")}}

               ;; ignored 
               ".table-view .ignored .table-cell"
               {:-fx-opacity "0.5"
                :-fx-font-style "italic"}

               ;; ignored 'install' button gets slightly different styling
               ".table-view .ignored .install-button-column.table-cell"
               {:-fx-opacity "1" ;; a disabled button already has some greying effect applied
                :-fx-font-style "normal"}


               ;;
               ;; tables with alternating row colours (just add the '.odd-rows' class)
               ;; 


               ".table-view.odd-rows .table-row-cell:odd"
               {:-fx-background-color (colour :row-alt)
                ":hover"
                {:-fx-background-color (colour :row-hover)}}

               ;; 'the above overwrites the pseudo class as well apparently.
               ;; this 'resets' it so we don't get selected rows with alternating blanks
               ".table-view.odd-rows .table-row-cell:odd:selected"
               {:-fx-background-color (colour :row-selected)
                ":hover"
                {:-fx-background-color (colour :row-hover)}}

               ".table-view .install-button-column.table-cell"
               {:-fx-padding "0px"
                :-fx-alignment "center"}

               ".table-view .install-button-column .button"
               {:-fx-pref-width 100
                :-fx-padding "2px 0"
                :-fx-background-radius "4"}


               ;;
               ;; tabber
               ;;


               ".tab-pane > .tab-header-area"
               {:-fx-padding ".7em 0 0 .6em"}

               ;; tabs
               ".tab-pane > .tab-header-area > .headers-region > .tab"
               {:-fx-background-radius "0"
                :-fx-padding ".25em 1em"
                :-fx-focus-color "transparent" ;; disables the 'blue box' of selected widgets
                :-fx-faint-focus-color "transparent" ;; literally, a very faint box remains
                }


               ;;
               ;; installed-addons tab
               ;;


               ".table-view #welcome-screen "
               {:-fx-alignment "center"

                ".big-welcome-text"
                {:-fx-font-size "5em"
                 :-fx-font-weight "bold"
                 :-fx-padding ".3em 1em"
                 :-fx-spacing "1em"}

                ".big-welcome-subtext"
                {:-fx-font-size "1.8em"
                 :-fx-font-family "monospace"
                 :-fx-padding ".8em 0 1em 0"}}

               "#update-all-button"
               {:-fx-min-width "110px"}

               "#game-track-container "
               {:-fx-alignment "center"

                "#game-track-check-box"
                {:-fx-padding "0 0 0 .65em"
                 :-fx-min-width "70px"}

                "#game-track-combo-box"
                {:-fx-min-width "121px"}}

               ".table-view#installed-addons "
               {".wow-column"
                {:-fx-alignment "center"}

                ".more-column"
                {:-fx-padding 0
                 :-fx-alignment "top-center"}

                ".more-column > .button"
                {:-fx-padding 0
                 :-fx-pref-width 100
                 :-fx-background-color nil
                 :-fx-font-size "1.5em"
                 ;; green tick
                 :-fx-text-fill (colour :uber-button-tick)
                 :-fx-font-weight "bold"}

                ".table-row-cell.warnings .more-column > .button"
                {;; orange bar
                 :-fx-text-fill (colour :uber-button-warn)}

                ".table-row-cell.errors .more-column > .button"
                {;; red cross
                 :-fx-text-fill (colour :uber-button-error)}}

               ".table-view#installed-addons .updateable"
               {:-fx-background-color (colour :row-updateable)

                " .table-cell"
                {:-fx-text-fill (colour :row-updateable-text)}

                " .hyperlink"
                {:-fx-text-fill (colour :hyperlink-updateable)}

                ;; selected+updateable addons look *slightly* different
                ":selected"
                {;; !important so that hovering over a selected+updateable row doesn't change it's colour
                 :-fx-background-color (str (colour :row-updateable-selected) " !important")}}

               ".table-view#installed-addons .installed-column"
               {:-fx-alignment "center-right"
                :-fx-text-overrun "leading-ellipsis"}

               ".table-view#installed-addons .available-column"
               {:-fx-alignment "center-right"
                :-fx-text-overrun "leading-ellipsis"}


               ;;
               ;; notice-logger
               ;;


               ".table-view#notice-logger "
               {:-fx-font-family "monospace"

                ".warn .table-cell"
                {:-fx-text-fill (colour :row-warning-text)
                 :-fx-background-color (colour :row-warning)}

                ".warn:selected"
                {:-fx-background-color "-fx-selection-bar"}

                ".error .table-cell"
                {:-fx-text-fill (colour :row-error-text)
                 :-fx-background-color (colour :row-error)}

                ".error:selected"
                {:-fx-background-color "-fx-selection-bar"}

                ".report .table-cell"
                {:-fx-text-fill (colour :row-report-text)}

                ".report #message"
                {:-fx-font-style "italic"}

                "#level"
                {:-fx-alignment "center"}

                "#source"
                {:-fx-alignment "center"}

                "#time"
                {:-fx-alignment "center"}

                "#message"
                {:-fx-padding "0 0 0 .5em"}

                "#message.column-header .label"
                {:-fx-alignment "center-left"}}


               ;;
               ;; notice-logger-nav
               ;;


               "#notice-logger-nav"
               {:-fx-padding "1.1em .75em" ;; 1.1em so installed, search and log pane tables all start at the same height
                :-fx-font-size ".9em"

                " .radio-button"
                {:-fx-padding "0 .5em"}}


               ;;
               ;; search
               ;;


               "#search-install-button"
               {:-fx-min-width "90px"}

               "#search-random-button"
               {:-fx-min-width "80px"}

               "#search-prev-button"
               {:-fx-min-width "80px"}

               "#search-next-button"
               {:-fx-min-width "70px"}

               "#search-text-field "
               {:-fx-min-width "100px"
                :-fx-text-fill (colour :table-font-colour)}

               ".table-view#search-addons .downloads-column"
               {:-fx-alignment "center-right"}

               ".table-view#search-addons .updated-column"
               {:-fx-alignment "center"}


               ;;
               ;; status bar (bottom of app)
               ;; 


               "#status-bar"
               {:-fx-font-size ".9em"
                :-fx-padding "0"
                :-fx-alignment "center-left"}

               "#status-bar-left"
               {:-fx-padding "0 10"
                :-fx-alignment "center-left"
                :-fx-pref-width 9999.0

                " > .text"
                {;; omg, wtf does 'fx-fill' work and not 'fx-text-fill' ???
                 :-fx-fill (colour :table-font-colour)}}

               "#status-bar-right"
               {:-fx-min-width "130px" ;; long enough to render "warnings (999)"

                :-fx-padding "5px 12px 5px 0"
                :-fx-alignment "center-right"}

               "#status-bar-right .button"
               {:-fx-padding "4 10"
                ;; doesn't look right when button is coloured.
                ;;:-fx-background-radius "4"
                :-fx-font-size "11px"

                ;; this isn't great but it's better than nothing. revisit when it makes more sense.
                ":armed"
                {:-fx-background-insets "1 1 0 0,  1,  2,  3"}}

               ".button.with-warning"
               {:-fx-background-insets "0 0 -1 0,  0,  1,  2"
                :-fx-background-color (str "-fx-shadow-highlight-color, -fx-outer-border, -fx-inner-border, " (colour :row-warning))
                :-fx-text-fill (colour :row-warning-text)}

               ".button.with-error"
               {:-fx-background-insets "0 0 -1 0,  0"
                :-fx-background-color (str "-fx-shadow-highlight-color, -fx-inner-border, " (colour :row-error))
                :-fx-text-fill (colour :row-error-text)}

               ;;
               ;; addon-detail
               ;;


               ".addon-detail "
               {".title"
                {:-fx-font-size "2.5em"
                 :-fx-padding "1em 0 .5em 1em"
                 :-fx-text-fill "-fx-text-base-color"}

                ".subtitle"
                {:-fx-font-size "1.1em"
                 :-fx-text-fill "-fx-text-base-color"
                 :-fx-padding "0 0 .5em 1.75em"}

                ".subtitle .installed-version"
                {:-fx-text-fill "-fx-text-base-color"
                 :-fx-padding "0 1em 0 0"}

                ".subtitle .version"
                {:-fx-text-fill (colour :row-updateable-text)
                 :-fx-background-color (colour :row-updateable-selected)
                 :-fx-padding "0 .75em"
                 :-fx-background-radius ".4em"}

                ".subtitle .hyperlink"
                {:-fx-padding "0 .5em .1em .5em"
                 :-fx-font-size ".9em"}

                ".section-title"
                {:-fx-font-size "1.3em"
                 :-fx-padding "1em 0 .5em 1em"
                 :-fx-min-width "200px"
                 :-fx-text-fill "-fx-text-base-color"}

                ".disabled-text"
                {:-fx-opacity "0.3"}

                ".description"
                {:-fx-font-size "1.4em"
                 :-fx-padding "0 0 1.5em 1em"
                 :-fx-wrap-text true
                 :-fx-font-style "italic"
                 :-fx-text-fill "-fx-text-base-color"}

                ;; keep the ignore and delete buttons very separate from the others
                ".separator"
                {:-fx-padding "0 1em"}

                ".table-view#notice-logger"
                {:-fx-pref-height "10pc"}

                ;; hide column headers in notice-logger in addon-detail pane
                ".table-view#notice-logger .column-header-background"
                {:-fx-max-height 0
                 :-fx-pref-height 0
                 :-fx-min-height 0}

                "#notice-logger-nav"
                {:-fx-padding "0 0 .5em 0"
                 :-fx-alignment "bottom-right"
                 :-fx-pref-width 9999.0}

                ".table-view#key-vals .column-header .label"
                {:-fx-font-style "normal"} ;; column *values*, not the column *header* should be italic

                ".table-view#key-vals .key-column"
                {:-fx-alignment "center-right"
                 :-fx-padding "0 1em 0 0"
                 :-fx-font-style "italic"}} ;; ends .addon-detail

               ;; ---
               }}))]

     ;; return a single map with all themes in it.
     ;; themes are separated by their top-level 'root' key.


     (into {} (for [[theme-key _] themes]
                (generate-style theme-key))))))

(def INSTALLED-TAB 0)
(def SEARCH-TAB 1)
(def LOG-TAB 2)

(def NUM-STATIC-TABS 3)

;;

(defn get-window
  "returns the application `Window` object."
  []
  (first (Window/getWindows)))

(defn set-icon
  "adds the strongbox icon to the application window"
  []
  @(fx/on-fx-thread
    (.add (.getIcons (get-window))
          (javafx.scene.image.Image. (.openStream (clojure.java.io/resource "strongbox.png"))))))

(defn select
  [node-id]
  (-> (get-window) .getScene .getRoot (.lookupAll node-id)))

(defn-spec tab-index int?
  "returns the index of the currently selected tab"
  []
  (-> (select "#tabber") first .getSelectionModel .getSelectedIndex))

(defn-spec tab-list-tab-index int?
  "returns the index of the currently selected tab within `:tab-list`, which doesn't include the static tabs"
  []
  (- (tab-index) NUM-STATIC-TABS))

(defn extension-filter
  [x]
  ;; taken from here:
  ;; - https://github.com/cljfx/cljfx/pull/40/files
  (cond
    (instance? FileChooser$ExtensionFilter x) x
    (map? x) (FileChooser$ExtensionFilter. ^String (:description x) ^List (seq (:extensions x)))
    :else (throw (Exception. (format "cannot coerce '%s' to `FileChooser$ExtensionFilter`" (type x))))))

(defn file-chooser
  "prompt user to select a file"
  [event & [opt-map]]
  (let [opt-map (or opt-map {})
        default-open-type :open
        open-type (get opt-map :type default-open-type)
        ;; valid for a menu-item
        ;;window (-> event .getTarget .getParentPopup .getOwnerWindow .getScene .getWindow)
        window (get-window)
        chooser (doto (FileChooser.)
                  (.setTitle "Open File"))]
    (when-let [ext-filters (:filters opt-map)]
      (-> chooser .getExtensionFilters (.addAll (mapv extension-filter ext-filters))))
    (when-let [file-obj @(fx/on-fx-thread
                          (case open-type
                            :save (.showSaveDialog chooser window)
                            (.showOpenDialog chooser window)))]
      (-> file-obj .getAbsolutePath str))))

(defn dir-chooser
  "prompt user to select a directory"
  [event]
  (let [;; valid for a menu-item
        window (-> event .getTarget .getParentPopup .getOwnerWindow .getScene .getWindow)
        chooser (doto (DirectoryChooser.)
                  (.setTitle "Select Directory"))]
    (when-let [dir @(fx/on-fx-thread
                     (.showDialog chooser window))]
      (-> dir .getAbsolutePath str))))

(defn-spec text-input (s/or :ok string? :noop nil?)
  "prompt user to enter text"
  [prompt string?]
  @(fx/on-fx-thread
    (let [widget (doto (TextInputDialog.)
                   (.setTitle prompt)
                   (.setHeaderText nil)
                   (.setContentText prompt)
                   (.initOwner (get-window)))
          optional-val (.showAndWait widget)]
      (when (and (.isPresent optional-val)
                 (not (empty? (.get optional-val))))
        (.get optional-val)))))

(defn alert
  "displays an alert dialog to the user with varying button combinations they can press.
  the result object is a weirdo `java.util.Optional` https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html
  whose `.get` return value is equal to the button type clicked"
  [alert-type-key msg & [opt-map]]
  @(fx/on-fx-thread
    (let [alert-type-map {:warning Alert$AlertType/WARNING
                          :error Alert$AlertType/ERROR
                          :confirm Alert$AlertType/CONFIRMATION
                          :info Alert$AlertType/INFORMATION}
          alert-type (get alert-type-map alert-type-key)
          widget (doto (Alert. alert-type)
                   (.setTitle (:title opt-map))
                   (.setHeaderText (:header opt-map))
                   (.setContentText msg)
                   (.initOwner (get-window)))
          wait? (get opt-map :wait? true)]
      (when (:content opt-map)
        (.setContent (.getDialogPane widget) (:content opt-map)))
      (if wait?
        (.showAndWait widget)
        (.show widget)))))

(defn-spec confirm boolean?
  "displays a confirmation prompt with the given `heading` and `message`.
  `heading` may be `nil`.
  returns `true` if the 'ok' button is clicked, else `false`."
  [heading (s/nilable string?), message string?]
  (let [result (alert :confirm message {:title "confirm" :header heading})]
    (= (.get result) ButtonType/OK)))

(defn-spec confirm->action nil?
  "displays a confirmation prompt with the given `heading` and `message` and then calls given `callback` on success"
  [heading (s/nilable string?), message string?, callback fn?]
  (when (confirm heading message)
    (callback)
    nil))

;; https://github.com/cljfx/cljfx/blob/babc2f09e4827efb29f859a442a1658d82169a62/examples/e25_radio_buttons.clj
(defn radio-group
  [{:keys [options value on-action label-coercer container-id container-type]}]
  {:fx/type fx/ext-let-refs
   :refs {::toggle-group {:fx/type :toggle-group}}
   :desc {:fx/type (or container-type :h-box)
          :id (or container-id (utils/unique-id))
          :children (for [option options]
                      {:fx/type :radio-button
                       :toggle-group {:fx/type fx/ext-get-ref
                                      :ref ::toggle-group}
                       :selected (= option value)
                       :text ((or label-coercer str) option)
                       :on-action (partial on-action option)})}})

(defn-spec component-instance :javafx/node
  "given a cljfx component `description` map, returns a JavaFX instance of it."
  [description map?]
  (-> description fx/create-component fx/instance))

;;

(defn button
  "generates a simple button with a means to check to see if it should be disabled and an optional tooltip"
  [label on-action & [{:keys [disabled? tooltip tooltip-delay style-class]}]]
  (let [btn (cond->
             {:fx/type :button
              :text label
              :on-action on-action}

              (boolean? disabled?)
              (merge {:disable disabled?})

              (some? style-class)
              (merge {:style-class ["button" style-class]}))]

    (if (some? tooltip)
      {:fx/type fx.ext.node/with-tooltip-props
       :props {:tooltip {:fx/type :tooltip
                         :text tooltip
                         :show-delay (or tooltip-delay 200)}}
       :desc btn}

      btn)))

(defn menu-item
  [label handler & [opt-map]]
  (merge
   {:fx/type :menu-item
    :text label
    :mnemonic-parsing true
    :on-action handler}
   (when-let [key (:key opt-map)]
     {:accelerator key})
   (dissoc opt-map :key)))

(defn menu
  [label items & [opt-map]]
  (merge
   {:fx/type :menu
    :text label
    :mnemonic-parsing true
    :items items}
   (when-let [key (:key opt-map)]
     {:accelerator key})
   (dissoc opt-map :key)))

(defn async
  "execute given function and it's optional argument list asynchronously.
  for example: `(async println [1 2 3])` prints \"1 2 3\" on another thread."
  ([f]
   (async f []))
  ([f arg-list]
   (future
     (try
       (apply f arg-list)
       (catch RuntimeException re
         ;;(error re "unhandled exception in thread"))))))
         (println "unhandled exception in thread" re))))))

;; handlers

(defn-spec async-event-handler fn?
  "wraps `f`, calling it with any given `args` later"
  [f fn?]
  (fn [& args]
    (async f args)))

(defn-spec async-handler fn?
  "same as `async-handler` but calls `f` and ignores `args`.
  useful for calling functions asynchronously that don't accept an `event` object."
  [f fn?]
  (fn [& _]
    (async f)))

(defn-spec event-handler fn?
  "wraps `f`, calling it with any given `args`.
  useful for debugging, otherwise just use the function directly"
  [f fn?]
  (fn [& args]
    (apply f args)))

(defn-spec handler fn?
  "wraps `f`, calling it but ignores any args.
  useful for calling functions that don't accept an `event` object."
  [f fn?]
  (fn [& _]
    (f)))

(def donothing
  "accepts any args, does nothing, returns nil.
  good for placeholder event handlers."
  (constantly nil))
(def do-nothing donothing)

(defn wow-dir-picker
  "prompts the user to select an addon dir. 
  if a valid addon directory is selected, calls `cli/set-addon-dir!`"
  [event]
  (when-let [dir (dir-chooser event)]
    (when (fs/directory? dir)
      ;; unlike swing, it doesn't appear possible to select a non-directory with javafx (good)
      (cli/set-addon-dir! dir))))

(defn exit-handler
  "exit the application. if running while testing or within a repl, it just closes the window"
  [& [_]]
  (cond
    ;; fresh repl => (restart) => (:in-repl? @state) => nil
    ;; because the app hasn't been started and there is no state yet, the app will exit.
    ;; when hitting ctrl-c while the gui is running, `(utils/in-repl?) => false` because it's running on the JavaFX thread,
    ;; and so will exit again there :( the double-check here seems to work though.
    (or (:in-repl? @core/state)
        (utils/in-repl?)) (swap! core/state assoc :gui-showing? false)
    core/testing? (swap! core/state assoc :gui-showing? false)
    ;; 2020-08-08: `ss/invoke-later` was keeping the old window around when running outside of repl.
    ;; `ss/invoke-soon` seems to fix that.
    ;;  - http://daveray.github.io/seesaw/seesaw.invoke-api.html
    ;; 2020-09-27: similar issue in javafx
    :else (Platform/runLater (fn []
                               (Platform/exit)
                               (System/exit 0)))))

(defn-spec switch-tab nil?
  "switches the tab-pane to the tab at the given index"
  [tab-idx int?]
  (-> (select "#tabber") first .getSelectionModel (.select tab-idx))
  nil)

(defn-spec switch-tab-idx nil?
  "switches the tab-pane to the tab at the given index *on the JavaFX event thread*.
  the dynamic tabs seem to require a `runLater` unlike static tabs.
  multiple instances of strongbox will still interfere with this behaviour and the switching won't occur"
  [idx int?]
  (Platform/runLater
   (fn []
     (switch-tab idx))))

(defn-spec switch-tab-latest nil?
  "switches the tab-pan to the furthest-right tab"
  []
  (switch-tab-idx (-> (core/get-state :tab-list) count (+ NUM-STATIC-TABS) dec)))

(defn-spec switch-tab-event-handler fn?
  "returns an event handler that switches to the given `tab-idx` when called."
  [tab-idx int?]
  (fn [_]
    (switch-tab tab-idx)))

(defn import-addon-handler
  "imports an addon by parsing a URL"
  [event]
  (let [addon-url (text-input "Enter URL of addon")

        fail-msg ["Failed. URL must be:"
                  "valid"
                  "originate from github.com"
                  "addon uses 'releases'"
                  "latest release has a packaged 'asset'"
                  "asset must be a .zip file"
                  "zip file must be structured like an addon"]
        fail-msg (clojure.string/join (format "\n %s " blt) fail-msg)

        failure #(alert :error fail-msg)
        warn-msg "Failed. Addon successfully added to catalogue but could not be installed."
        warning #(alert :warning warn-msg)]
    (when addon-url
      (if-let [result (core/add+install-user-addon! addon-url)]
        (when-not (contains? result :download-url)
          (warning))
        (failure))))
  nil)

(def json-files-extension-filters
  [{:description "JSON files" :extensions ["*.json"]}])

(defn import-addon-list-handler
  "prompts user with a file selection dialogue then imports a list of addons from the selected file"
  [event]
  (when-let [abs-path (file-chooser event {:filters json-files-extension-filters})]
    (core/import-exported-file abs-path)
    (core/refresh))
  nil)

(defn export-addon-list-handler
  "prompts user with a file selection dialogue then writes the current directory of addons to the selected file"
  [event]
  (when-let [abs-path (file-chooser event {:type :save
                                           :filters json-files-extension-filters})]
    (core/export-installed-addon-list-safely abs-path))
  nil)

(defn export-user-catalogue-handler
  "prompts user with a file selection dialogue then writes the user catalogue to selected file"
  [event]
  (when-let [abs-path (file-chooser event {:type :save
                                           :filters json-files-extension-filters})]
    (core/export-user-catalogue-addon-list-safely abs-path))
  nil)

(defn-spec -about-strongbox-dialog map?
  "returns a description of the 'about' dialog contents.
  separated here for testing and test coverage."
  []
  {:fx/type :v-box
   :id "about-dialog"
   :children [{:fx/type :label
               :id "about-pane-title"
               :text "strongbox"}
              {:fx/type :text
               :text (format "version %s" (core/strongbox-version))}
              {:fx/type :text
               :text (format "version %s is now available to download!" (core/latest-strongbox-release))
               :managed (not (core/latest-strongbox-version?))
               :visible (not (core/latest-strongbox-version?))}
              {:fx/type :hyperlink
               :text "https://github.com/ogri-la/strongbox"
               :on-action (handler #(utils/browse-to "https://github.com/ogri-la/strongbox"))
               :id "about-pane-hyperlink"}
              {:fx/type :text
               :text "AGPL v3"}]})

(defn about-strongbox-dialog
  "displays an informational dialog to the user about strongbox"
  [event]
  (alert :info "" {:content (component-instance (-about-strongbox-dialog))})
  nil)

(defn delete-selected-confirmation-handler
  "prompts the user to confirm if they *really* want to delete those addons they just selected and clicked 'delete' on"
  [event]
  (when-let [selected (core/get-state :selected-addon-list)]
    (if (utils/any (mapv :ignore? selected))
      (alert :error "Selection contains ignored addons. Stop ignoring them and then delete.")
      (let [msg (str (format "Deleting %s:\n %s " (count selected) blt)
                     (clojure.string/join (format "\n %s " blt) (map :label selected)))
            result (alert :confirm msg)]
        (when (= (.get result) ButtonType/OK)
          (cli/delete-selected)))))
  nil)

(defn search-results-install-handler
  "this switches to the 'installed' tab, then, for each addon selected, expands summary, installs addon, calls load-installed-addons and finally refreshes;
  this presents as a plodding step-wise update but is better than a blank screen and apparent hang"
  [addon-list]
  (switch-tab INSTALLED-TAB)
  (doseq [selected addon-list]
    (let [error-messages
          (logging/buffered-log
           :warn
           (some-> selected core/expand-summary-wrapper vector cli/-install-update-these))]
      (if (empty? error-messages)
        (core/load-installed-addons)
        (let [msg (str (format "warnings/errors while installing \"%s\":\n %s " (:label selected) blt)
                       (clojure.string/join (format "\n %s " blt) error-messages))]
          (alert :warning msg {:wait? false}))))
    (core/refresh)))

;;

(defn remove-addon-dir
  []
  (when (confirm "Confirm" ;; soft touch here, it's not a big deal.
                 "You can add this directory back at any time.")
    (cli/remove-addon-dir!)))

;;

(def separator
  "horizontal rule to logically separate menu items"
  {:fx/type fx/ext-instance-factory
   :create #(javafx.scene.control.SeparatorMenuItem.)})

(defn-spec build-catalogue-menu (s/or :ok ::sp/list-of-maps, :no-catalogues nil?)
  "returns a list of radio button descriptions that can toggle through the available catalogues"
  [selected-catalogue :catalogue/name, catalogue-location-list :catalogue/location-list]
  (when catalogue-location-list
    (let [rb (fn [{:keys [label name]}]
               {:fx/type :radio-menu-item
                :text label
                :selected (= selected-catalogue name)
                :toggle-group {:fx/type fx/ext-get-ref
                               :ref ::catalogue-toggle-group}
                :on-action (async-handler #(cli/set-catalogue-location! name))})]
      (mapv rb catalogue-location-list))))

(defn-spec build-theme-menu ::sp/list-of-maps
  "returns a list of radio button descriptions that can toggle through the available themes defined in `themes`"
  [selected-theme ::sp/gui-theme, theme-map map?]
  (let [rb (fn [theme-key]
             {:fx/type :radio-menu-item
              ;; "Light theme", "Dark green theme"
              :text (format "%s theme" (-> theme-key name (str-replace #"-" " ") capitalize))
              :selected (= selected-theme theme-key)
              :toggle-group {:fx/type fx/ext-get-ref
                             :ref ::theme-toggle-group}
              :on-action (fn [_]
                           (swap! core/state assoc-in [:cfg :gui-theme] theme-key)
                           (core/save-settings))})]
    (mapv rb (keys theme-map))))

(defn-spec build-addon-detail-menu ::sp/list-of-maps
  "returns a menu of dynamic tabs with a 'close all' button at the bottom"
  [tab-list :ui/tab-list]
  (let [addon-detail-menuitem
        (fn [idx tab]
          (let [tab-idx (+ idx NUM-STATIC-TABS)]
            (menu-item (:label tab) (async-handler #(switch-tab tab-idx)))))
        close-all (menu-item "Close all" (async-handler cli/remove-all-tabs))]
    (concat (map-indexed addon-detail-menuitem tab-list)
            [separator close-all])))

(defn menu-item--num-zips-to-keep
  "returns a checkbox menuitem that affects the user preference `addon-zips-to-keep`"
  [{:keys [fx/context]}]
  (let [num-addon-zips-to-keep (fx/sub-val context get-in [:app-state :cfg :preferences :addon-zips-to-keep])
        selected? (not (nil? num-addon-zips-to-keep)) ;; `nil` is 'keep all zips', see `config.clj`
        ]
    {:fx/type :check-menu-item
     :text "Remove addon zip after installation" ;; (global)" 
     :selected selected?
     :on-action (fn [ev]
                  (cli/set-preference :addon-zips-to-keep (if (.isSelected (.getSource ev)) 0 nil)))}))

(defn menu-item--automatic-update-all
  [{:keys [fx/context]}]
  (let [selected? (fx/sub-val context get-in [:app-state :cfg :preferences :automatic-update-all])]
    {:fx/type :check-menu-item
     :text "Update addons immediately"
     :selected selected?
     :on-action (fn [ev]
                  (cli/set-preference :automatic-update-all (.isSelected (.getSource ev))))}))

(defn menu-bar
  "returns a description of the menu at the top of the application"
  [{:keys [fx/context]}]

  (let [no-addon-dir? (nil? (fx/sub-val context get-in [:app-state :cfg :selected-addon-dir]))
        file-menu [(menu-item "_New addon directory" (async-event-handler wow-dir-picker) {:key "Ctrl+N"})
                   (menu-item "Remove addon directory" (async-handler remove-addon-dir)
                              {:disable no-addon-dir?})
                   separator
                   (menu-item "_Update all" (async-handler cli/update-all)
                              {:key "Ctrl+U", :disable no-addon-dir?})
                   (menu-item "Re-install all" (async-handler cli/re-install-or-update-all)
                              {:disable no-addon-dir?})
                   separator
                   (menu-item "Import list of addons" (async-event-handler import-addon-list-handler)
                              {:disable no-addon-dir?})
                   (menu-item "Export list of addons" (async-event-handler export-addon-list-handler)
                              {:disable no-addon-dir?})
                   (menu-item "Import addon from Github" (async-event-handler import-addon-handler)
                              {:disable no-addon-dir?})
                   (menu-item "Export Github addon list" (async-event-handler export-user-catalogue-handler)
                              {:disable no-addon-dir?})
                   separator
                   (menu-item "E_xit" exit-handler {:key "Ctrl+Q"})]

        prefs-menu [{:fx/type menu-item--num-zips-to-keep}
                    {:fx/type menu-item--automatic-update-all}
                    ]

        view-menu (into
                   [(menu-item "Refresh" (async-handler cli/hard-refresh) {:key "F5"})
                    separator
                    (menu-item "_Installed" (switch-tab-event-handler INSTALLED-TAB) {:key "Ctrl+I"})
                    (menu-item "Searc_h" (switch-tab-event-handler SEARCH-TAB)
                               {:key "Ctrl+H" :disable no-addon-dir?})
                    (menu-item "_Log" (switch-tab-event-handler LOG-TAB) {:key "Ctrl+L"})
                    (let [tab-list (fx/sub-val context get-in [:app-state :tab-list])]
                      (menu "Ad_don detail" (build-addon-detail-menu tab-list)
                            {:disable (empty? tab-list)}))
                    separator]
                   (build-theme-menu
                    (fx/sub-val context get-in [:app-state :cfg :gui-theme])
                    themes))

        catalogue-menu (into (build-catalogue-menu
                              (fx/sub-val context get-in [:app-state :cfg :selected-catalogue])
                              (fx/sub-val context get-in [:app-state :cfg :catalogue-location-list]))
                             [separator
                              (menu-item "Refresh user catalogue" (async-handler core/refresh-user-catalogue))])

        cache-menu [(menu-item "Clear http cache" (async-handler core/delete-http-cache!))
                    (menu-item "Clear addon zips" (async-handler core/delete-downloaded-addon-zips!)
                               {:disable no-addon-dir?})
                    (menu-item "Clear catalogues" (async-handler (juxt core/db-reload-catalogue core/delete-catalogue-files!)))
                    (menu-item "Clear log files" (async-handler core/delete-log-files!))
                    (menu-item "Clear all" (async-handler core/clear-all-temp-files!))
                    separator
                    (menu-item "Delete WowMatrix.dat files" (async-handler core/delete-wowmatrix-dat-files!)
                               {:disable no-addon-dir?})
                    (menu-item "Delete .wowman.json files" (async-handler (juxt core/delete-wowman-json-files! core/refresh))
                               {:disable no-addon-dir?})
                    (menu-item "Delete .strongbox.json files" (async-handler (juxt core/delete-strongbox-json-files! core/refresh))
                               {:disable no-addon-dir?})]

        help-menu [(menu-item "About strongbox" about-strongbox-dialog)]]

    {:fx/type fx/ext-let-refs
     :refs {::catalogue-toggle-group {:fx/type :toggle-group}
            ::theme-toggle-group {:fx/type :toggle-group}}
     :desc {:fx/type :menu-bar
            :id "main-menu"
            :menus [(menu "_File" file-menu)
                    (menu "_View" view-menu)
                    (menu "Catalogue" catalogue-menu)
                    (menu "_Preferences" prefs-menu)
                    (menu "Cache" cache-menu)
                    (menu "Help" help-menu)]}}))

(defn wow-dir-dropdown
  [{:keys [fx/context]}]
  (let [config (fx/sub-val context get-in [:app-state :cfg])
        selected-addon-dir (:selected-addon-dir config)
        addon-dir-map-list (get config :addon-dir-list [])]
    {:fx/type ext-recreate-on-key-changed
     :key (sort-by :addon-dir addon-dir-map-list)
     :desc {:fx/type :combo-box
            :id "addon-dir-dropdown"
            :value selected-addon-dir
            :on-value-changed (async-event-handler
                               (fn [new-addon-dir]
                                 (cli/set-addon-dir! new-addon-dir)))
            :items (mapv :addon-dir addon-dir-map-list)
            :disable (empty? addon-dir-map-list)}}))

(defn game-track-dropdown
  [{:keys [fx/context]}]
  (let [selected-addon-dir (fx/sub-val context get-in [:app-state :cfg :selected-addon-dir])
        addon-dir-map (core/addon-dir-map selected-addon-dir)
        game-track (:game-track addon-dir-map)
        strict? (get addon-dir-map :strict? core/default-game-track-strictness)
        tooltip "restrict or relax the installation of addons for specific WoW versions"]

    {:fx/type :h-box
     :id "game-track-container"
     :children [{:fx/type :combo-box
                 :id "game-track-combo-box"
                 :value (get sp/game-track-labels-map game-track)
                 :on-value-changed (async-event-handler
                                    (fn [new-game-track]
                                      ;; todo: push to cli
                                      (core/set-game-track! (get sp/game-track-labels-map-inv new-game-track))
                                      (core/refresh)))
                 :items (mapv second sp/game-track-labels)
                 :disable (nil? selected-addon-dir)}

                {:fx/type fx.ext.node/with-tooltip-props
                 :props {:tooltip {:fx/type :tooltip
                                   :text tooltip
                                   :show-delay 200}}
                 :desc {:fx/type :check-box
                        :id "game-track-check-box"
                        :text "Strict"
                        :selected strict?
                        :disable (nil? (core/get-game-track-strictness))
                        :on-selected-changed (async-event-handler cli/set-game-track-strictness!)}}]}))

(defn installed-addons-menu-bar
  "returns a description of the installed-addons tab-pane menu"
  [{:keys [fx/context]}]
  (let [selected-addon-dir (fx/sub-val context get-in [:app-state :cfg :selected-addon-dir])]
    {:fx/type :h-box
     :padding 10
     :spacing 10
     :children [{:fx/type :button
                 :text "Update all"
                 :id "update-all-button"
                 :on-action (async-handler cli/update-all)
                 :disable (nil? selected-addon-dir)}
                {:fx/type wow-dir-dropdown}
                {:fx/type game-track-dropdown}
                {:fx/type :button
                 :text (str "Update Available: " (core/latest-strongbox-release))
                 :on-action (handler #(utils/browse-to "https://github.com/ogri-la/strongbox/releases"))
                 :visible (not (core/latest-strongbox-version?))
                 :managed (not (core/latest-strongbox-version?))}]}))

(defn-spec table-column map?
  "returns a description of a table column that lives within a table"
  [column-data :gui/column-data]
  (let [column-class (if-let [column-id (some utils/nilable [(:id column-data) (:text column-data)])]
                       (lower-case (str column-id "-column"))
                       "column")
        column-name (:text column-data)
        default-cvf (fn [row] (get row (keyword column-name)))
        final-cvf {:cell-value-factory (get column-data :cell-value-factory default-cvf)}

        final-style {:style-class (into ["table-cell" column-class] (get column-data :style-class))}

        default {:fx/type :table-column
                 :min-width 80}]
    (merge default column-data final-cvf final-style)))

(defn-spec href-to-hyperlink map?
  "returns a hyperlink description or an empty text description"
  [row (s/nilable (s/keys :opt-un [::sp/url]))]
  (let [url (:url row)
        label (:source row)]
    (if (and url label)
      {:fx/type :hyperlink
       :on-action (handler #(utils/browse-to url))
       :text (str "↪ " label)}
      {:fx/type :text
       :text ""})))

(defn-spec addon-fs-link (s/or :hyperlink map?, :nothing nil?)
  "returns a hyperlink that opens a file browser to a path on the filesystem."
  [dirname (s/nilable ::sp/dirname)]
  (when dirname
    {:fx/type :hyperlink
     :on-action (handler #(utils/browse-to (format "%s/%s" (core/selected-addon-dir) dirname)))
     :text "↪ browse local files"}))

(defn-spec available-versions (s/or :ok string? :no-version-available nil?)
  "formats the 'available version' string depending on the state of the addon.
  pinned and ignored addons get a helpful prefix."
  [row map?]
  (cond
    (:ignore? row) "(ignored)"
    (:pinned-version row) (str "(pinned) " (:pinned-version row))
    :else
    (:version row)))

(defn-spec build-release-menu ::sp/list-of-maps
  "returns a list of `:menu-item` maps that will update the given `addon` with 
  the release data for a selected release in `release-list`."
  [addon :addon/expanded]
  (let [pin (fn [release _]
              (cli/set-version addon release))]
    (mapv (fn [release]
            (menu-item (or (:release-label release) (:version release))
                       (partial pin release)))
          (:release-list addon))))

(defn-spec singular-context-menu map?
  "context menu when a single addon is selected."
  [selected-addon :addon/toc]
  (let [pinned? (some? (:pinned-version selected-addon))
        release-list (:release-list selected-addon)
        releases-available? (and (not (empty? release-list))
                                 (not pinned?))
        ignored? (addon/ignored? selected-addon)]
    {:fx/type :context-menu
     :items [(menu-item "Update" (async-handler cli/update-selected)
                        {:disable (not (addon/updateable? selected-addon))})
             (menu-item "Re-install" (async-handler cli/re-install-or-update-selected)
                        {:disable (not (addon/re-installable? selected-addon))})
             separator
             (if pinned?
               (menu-item "Unpin release" (async-handler cli/unpin)
                          {:disable ignored?})
               (menu-item "Pin release" (async-handler cli/pin)
                          {:disable ignored?}))
             (if releases-available?
               (menu "Releases" (build-release-menu selected-addon))
               (menu "Releases" [] {:disable true})) ;; skips even attempting to build the menu
             separator
             (if ignored?
               (menu-item "Stop ignoring" (async-handler cli/clear-ignore-selected))
               (menu-item "Ignore" (async-handler cli/ignore-selected)))
             separator
             (menu-item "Delete" delete-selected-confirmation-handler
                        {:disable ignored?})]}))

(defn-spec multiple-context-menu map?
  "context menu when multiple addons are selected."
  [selected-addon-list :addon/toc-list]
  (let [num-selected (count selected-addon-list)
        some-pinned? (->> selected-addon-list (map :pinned-version) (some some?) boolean)
        some-ignored? (->> selected-addon-list (filter :ignore?) (some some?) boolean)]
    {:fx/type :context-menu
     :items [(menu-item (str num-selected " addons selected") donothing {:disable true})
             separator
             (menu-item "Update" (async-handler cli/update-selected))
             (menu-item "Re-install" (async-handler cli/re-install-or-update-selected))
             separator
             (if some-pinned?
               (menu-item "Unpin release" (async-handler cli/unpin))
               (menu-item "Pin release" (async-handler cli/pin)))
             (menu "Releases" [] {:disable true})
             separator
             (if some-ignored?
               (menu-item "Stop ignoring" (async-handler cli/clear-ignore-selected))
               (menu-item "Ignore" (async-handler cli/ignore-selected)))
             separator
             (menu-item "Delete" delete-selected-confirmation-handler)]}))

(defn uber-button
  "returns a widget describing the current state of the given addon"
  [row]
  (let [tick "\u2714" ;; '✔'
        unsteady "\u2941" ;; '⥁' CLOCKWISE CLOSED CIRCLE ARROW
        warnings "\u2501" ;; '━' heavy horizontal
        errors "\u2A2F" ;; '⨯'
        update "\u21A6" ;; '↦'

        [text, tooltip]
        (cond
          (:ignore? row) ["", "ignoring"]
          (core/unsteady? (:name row)) [unsteady "in flux"]
          (cli/addon-has-errors? row) [errors (format "%s error(s)" (cli/addon-num-errors row))]
          (cli/addon-has-warnings? row) [warnings (format "%s warning(s)" (cli/addon-num-warnings row))]
          ;; an addon may have updates AND errors/warnings ...
          ;;(:update? row) update
          :else [tick "no problems"])

        text (if (:update? row) (str text " " update) text)
        tooltip (if (:update? row) (str tooltip ", updates pending") tooltip)]

    {:fx/type fx.ext.node/with-tooltip-props
     :props {:tooltip {:fx/type :tooltip
                       :text tooltip
                       :show-delay 200}}
     :desc {:fx/type :button
            :text text
            :on-action (fn [_]
                         (cli/add-addon-tab row)
                         (switch-tab-latest))}}))

(defn installed-addons-table
  [{:keys [fx/context]}]
  ;; subscribe to re-render table when addons become unsteady
  (fx/sub-val context get-in [:app-state :unsteady-addon-list])
  ;; subscribe to re-render rows when addons emit warnings or errors
  (fx/sub-val context get-in [:app-state :log-lines])
  (let [row-list (fx/sub-val context get-in [:app-state :installed-addon-list])
        selected (fx/sub-val context get-in [:app-state :selected-addon-list])
        selected-addon-dir (fx/sub-val context get-in [:app-state :cfg :selected-addon-dir])

        iface-version (fn [row]
                        (some-> row :interface-version str utils/interface-version-to-game-version))

        column-list [{:text "source" :min-width 125 :pref-width 125 :max-width 125
                      :cell-factory {:fx/cell-type :table-cell
                                     :describe (fn [row]
                                                 {:graphic (href-to-hyperlink row)})}
                      :cell-value-factory identity
                      :resizable false}
                     {:text "name" :min-width 150 :pref-width 200 :max-width 500 :cell-value-factory (comp no-new-lines :label)}
                     {:text "description" :min-width 150 :pref-width 300 :cell-value-factory (comp no-new-lines :description)}
                     {:text "installed" :pref-width 150 :max-width 250 :cell-value-factory :installed-version}
                     {:text "available" :pref-width 150 :max-width 250 :cell-value-factory available-versions}
                     {:text "WoW" :min-width 70 :pref-width 70 :max-width 70 :cell-value-factory iface-version :resizable false}
                     {:text "" :style-class ["more-column"] :min-width 80 :max-width 80 :resizable false
                      :cell-factory {:fx/cell-type :table-cell
                                     :describe (fn [row]
                                                 (if row
                                                   {:graphic (uber-button row)}
                                                   {:text ""}))}
                      :cell-value-factory identity}]]

    {:fx/type fx.ext.table-view/with-selection-props
     :props {:selection-mode :multiple
             :on-selected-items-changed cli/select-addons*}
     :desc {:fx/type :table-view
            :id "installed-addons"
            :placeholder (if (nil? selected-addon-dir)
                           {:fx/type :v-box
                            :id "welcome-screen"
                            :children [{:fx/type :label
                                        :style-class ["big-welcome-text"]
                                        :text "STRONGBOX"}
                                       {:fx/type :label
                                        :style-class ["big-welcome-subtext"]
                                        :text "\"File\" \u2794 \"New addon directory\""}]}
                           {:fx/type :text
                            :style-class ["table-placeholder-text"]
                            :text "No addons found."})
            :column-resize-policy javafx.scene.control.TableView/CONSTRAINED_RESIZE_POLICY
            :pref-height 999.0
            :row-factory {:fx/cell-type :table-row
                          :describe (fn [row]
                                      {:on-mouse-clicked (fn [e]
                                                           ;; double click handler https://github.com/cljfx/cljfx/issues/118
                                                           (when (and (= javafx.scene.input.MouseButton/PRIMARY (.getButton e))
                                                                      (= 2 (.getClickCount e)))
                                                             (cli/add-addon-tab row)
                                                             (switch-tab-latest)))
                                       :style-class
                                       (remove nil?
                                               ["table-row-cell" ;; `:style-class` will actually *replace* the list of classes
                                                (when (:update? row) "updateable")
                                                (when (:ignore? row) "ignored")
                                                (when (and row (core/unsteady? (:name row))) "unsteady")
                                                (cond
                                                  (and row (cli/addon-has-errors? row)) "errors"
                                                  (and row (cli/addon-has-warnings? row)) "warnings")])})}

            :columns (mapv table-column column-list)
            :context-menu (if (= 1 (count selected))
                            (singular-context-menu (first selected))
                            (multiple-context-menu selected))
            :items (or row-list [])}}))

(defn notice-logger
  "a log widget that displays a list of log lines.
  used by itself as well as embedded into the addon detail page.
  pass it a `filter-fn` to remove entries in the `:log-lines` list."
  [{:keys [fx/context tab-idx filter-fn section-title]}]
  (let [filter-fn (or filter-fn identity)
        current-log-level (if tab-idx
                            (fx/sub-val context get-in [:app-state :tab-list tab-idx :log-level])
                            (fx/sub-val context get-in [:app-state :gui-log-level]))
        log-level-filter (fn [log-line]
                           (>= (-> log-line :level logging/level-map)
                               (logging/level-map current-log-level)))

        log-message-list (->> (fx/sub-val context get-in [:app-state :log-lines])
                              (filter filter-fn)
                              ;; nfi how to programmatically change column sort order
                              reverse)

        level-occurances (utils/count-occurances log-message-list :level)

        source-label (fn [row]
                       (or (some-> row :source :dirname)
                           (some-> row :source :name)
                           "app"))

        ;; hide 'source' column in notice-logger when embedded in addon-detail pane
        source-width (if section-title 0 150) ;; bit of a hack

        column-list [{:id "source" :text "source" :pref-width source-width :max-width source-width :min-width source-width :cell-value-factory source-label}
                     {:id "level" :text "level" :max-width 80 :cell-value-factory (comp name :level)}
                     {:id "time" :text "time" :max-width 100 :cell-value-factory :time}
                     {:id "message" :text "message" :pref-width 500 :cell-value-factory :message}]

        log-level-list [:debug :info :warn :error] ;; :report] ;; 'reports' won't be interesting, no need to filter by them right now.
        log-level-list (if-not (contains? level-occurances :debug)
                         (rest log-level-list)
                         log-level-list)
        selected-log-level (fx/sub-val context get-in (if tab-idx
                                                        [:app-state :tab-list tab-idx :log-level]
                                                        [:app-state :gui-log-level]))
        log-level-changed-handler (fn [log-level _]
                                    (cli/change-notice-logger-level (keyword log-level) tab-idx))

        label-coercer (fn [log-level]
                        (let [num-occurances (level-occurances log-level)]
                          (format "%s (%s)" (name log-level) (or num-occurances 0))))

        log-message-list (filter log-level-filter log-message-list)]
    {:fx/type :border-pane
     :top {:fx/type :h-box
           :style-class ["notice-logger-nav-box"]
           :children (utils/items
                      [(when section-title
                         {:fx/type :label
                          :style-class ["section-title"]
                          :text section-title})

                       {:fx/type radio-group
                        :options log-level-list
                        :value selected-log-level
                        :label-coercer label-coercer
                        :container-id "notice-logger-nav"
                        :on-action log-level-changed-handler}])}

     :center {:fx/type :table-view
              :id "notice-logger"
              :style-class ["table-view" "odd-rows"]
              :placeholder {:fx/type :text
                            :style-class ["table-placeholder-text"]
                            :text ""}
              :selection-mode :multiple
              :row-factory {:fx/cell-type :table-row
                            :describe (fn [row]
                                        ;; we get a nil row when going up the severity level and some rows get filtered out ... weird.
                                        (when row
                                          {:style-class ["table-row-cell" (name (:level row))]
                                           :on-mouse-clicked
                                           (fn [e]
                                             ;; double click handler https://github.com/cljfx/cljfx/issues/118
                                             (when (and (= javafx.scene.input.MouseButton/PRIMARY (.getButton e))
                                                        (= 2 (.getClickCount e)))
                                               (if (or (contains? (:source row) :dirname)
                                                       (contains? (:source row) :source-id))
                                                 (do (cli/add-addon-tab (:source row))
                                                     (switch-tab-latest))
                                                 (let [remaining-seconds (- 60 (-> (Calendar/getInstance) (.get Calendar/SECOND)))]
                                                   (if (> remaining-seconds 1)
                                                     (timbre/warn (format "self destruction in T-minus %s seconds" remaining-seconds))
                                                     (timbre/error "fah-wooosh ... BOOOOOO ... /oh the humanity/ ... OOOOOOHHHMMM"))))))}))}
              :column-resize-policy javafx.scene.control.TableView/CONSTRAINED_RESIZE_POLICY
              :columns (mapv table-column column-list)
              :items (or log-message-list [])}}))

(defn installed-addons-pane
  [_]
  {:fx/type :border-pane
   :top {:fx/type installed-addons-menu-bar}
   :center {:fx/type installed-addons-table}})

(defn search-addons-table
  [{:keys [fx/context]}]
  (let [idx-key #(select-keys % [:source :source-id])
        installed-addon-idx (mapv idx-key (fx/sub-val context get-in [:app-state :installed-addon-list]))
        installed? #(utils/in? (idx-key %) installed-addon-idx)

        search-state (fx/sub-val context get-in [:app-state :search])
        addon-list (cli/search-results search-state)

        ;; rare case when there are precisely $cap results, the next page is empty
        empty-next-page (and (= 0 (count addon-list))
                             (> (-> search-state :page) 0))

        number-format #(.format number-formatter %)

        column-list [{:text "source" :min-width 125 :pref-width 125 :max-width 125 :resizable false
                      :cell-factory {:fx/cell-type :table-cell
                                     :describe (fn [row]
                                                 {:graphic (href-to-hyperlink row)})}
                      :cell-value-factory identity}
                     {:text "name" :min-width 150 :pref-width 250 :cell-value-factory (comp no-new-lines :label)}
                     {:text "description" :min-width 200 :pref-width 400 :cell-value-factory (comp no-new-lines :description)}
                     {:text "tags" :min-width 200 :pref-width 250 :cell-value-factory (comp str :tag-list)}
                     {:text "updated" :min-width 85 :max-width 85 :pref-width 85 :resizable false :cell-value-factory (comp #(utils/safe-subs % 10) :updated-date)}
                     {:text "downloads" :min-width 120 :pref-width 120 :max-width 120 :resizable false :cell-value-factory (comp number-format :download-count)}
                     {:text "" :style-class ["install-button-column"] :min-width 120 :pref-width 120 :max-width 120 :resizable false
                      :cell-factory {:fx/cell-type :table-cell
                                     :describe (fn [addon]
                                                 {:graphic (button "install" (async-handler #(search-results-install-handler [addon]))
                                                                   {:disabled? (installed? addon)})})}
                      :cell-value-factory identity}]]

    {:fx/type fx.ext.table-view/with-selection-props
     :props {:selection-mode :multiple
             ;; unlike gui.clj, we have access to the original data here. no need to re-select addons.
             :on-selected-items-changed cli/select-addons-search*}
     :desc {:fx/type :table-view
            :id "search-addons"
            :placeholder {:fx/type :label
                          :style-class ["table-placeholder-text"]
                          :text (if empty-next-page
                                  "ᕙ(`▿´)ᕗ"
                                  "No search results.")}
            :row-factory {:fx/cell-type :table-row
                          :describe (fn [addon]
                                      {:on-mouse-clicked (fn [e]
                                                           ;; double click handler https://github.com/cljfx/cljfx/issues/118
                                                           (when (and (= javafx.scene.input.MouseButton/PRIMARY (.getButton e))
                                                                      (= 2 (.getClickCount e)))
                                                             (cli/add-addon-tab addon)
                                                             (switch-tab-latest)))
                                       :style-class ["table-row-cell"
                                                     (when (installed? addon)
                                                       "ignored")]})}
            :column-resize-policy javafx.scene.control.TableView/CONSTRAINED_RESIZE_POLICY
            :pref-height 999.0
            :columns (mapv table-column column-list)
            :items addon-list}}))

(defn search-addons-search-field
  [{:keys [fx/context]}]
  (let [search-state (fx/sub-val context get-in [:app-state :search])]
    {:fx/type :h-box
     :padding 10
     :spacing 10
     :children
     [{:fx/type :text-field
       :id "search-text-field"
       :prompt-text "search"
       :on-text-changed cli/search}

      {:fx/type :button
       :id "search-install-button"
       :text "install selected"
       :on-action (async-handler #(search-results-install-handler (core/get-state :search :selected-result-list)))}

      {:fx/type :button
       :id "search-random-button"
       :text "random"
       :on-action (handler cli/random-search)}

      {:fx/type :h-box
       :id "spacer"
       :h-box/hgrow :ALWAYS}

      {:fx/type :button
       :id "search-prev-button"
       :text "previous"
       :disable (not (cli/search-has-prev? search-state))
       :on-action (handler cli/search-results-prev-page)}

      {:fx/type :button
       :id "search-next-button"
       :text "next"
       :disable (not (cli/search-has-next? search-state))
       :on-action (handler cli/search-results-next-page)}]}))

(defn search-addons-pane
  [_]
  {:fx/type :border-pane
   :top {:fx/type search-addons-search-field}
   :center {:fx/type search-addons-table}})

(defn addon-detail-button-menu
  "a row of buttons attached to available actions for the given addon"
  [{:keys [addon]}]
  {:fx/type :h-box
   :children [(if (addon/installed? addon)
                (button "Re-install" (async-handler #(cli/re-install-or-update-selected [addon]))
                        {:disabled? (not (addon/re-installable? addon))
                         :tooltip (format "Re-install version %s" (:installed-version addon))})

                (button "Install" (async-handler #(cli/install-addon addon))
                        {:disabled? (addon/installed? addon)
                         :tooltip (format "Install %s version %s" (:name addon) (:version addon))}))

              (button "Update" (async-handler #(cli/update-selected [addon]))
                      {:disabled? (not (addon/updateable? addon))
                       :tooltip (format "Update to version %s" (:version addon))})

              (if (addon/pinned? addon)
                (button "Unpin" (async-handler #(cli/unpin [addon]))
                        {:disabled? (not (addon/unpinnable? addon))
                         :tooltip (format "Unpin from version %s" (:pinned-version addon))})

                (button "Pin" (async-handler #(cli/pin [addon]))
                        {:disabled? (not (addon/pinnable? addon))
                         :tooltip (format "Pin to version %s" (:installed-version addon))}))

              {:fx/type :separator
               :orientation :vertical}

              (if (addon/ignored? addon)
                (button "Stop ignoring" (async-handler #(cli/clear-ignore-selected [addon])))
                (button "Ignore" (async-handler #(cli/ignore-selected [addon]))
                        {:tooltip "Prevent all changes"
                         :disabled? (not (addon/ignorable? addon))}))

              {:fx/type :separator
               :orientation :vertical}

              (button "Delete" (async-handler #(confirm->action "Confirm" "Are you sure you want to delete this addon?" (partial cli/delete-selected [addon])))
                      {:disabled? (not (addon/deletable? addon))
                       :tooltip "Permanently delete"})]})

(defn addon-detail-key-vals
  "displays a two-column table of `key: val` fields for what we know about an addon."
  [{:keys [addon]}]
  (let [column-list [{:text "key" :min-width 150 :pref-width 150 :max-width 150 :resizable false :cell-value-factory (comp name :key)}
                     {:text "val" :cell-value-factory :val}]

        blacklist [:group-addons :release-list]
        sanitised (apply dissoc addon blacklist)

        row-list (apply utils/csv-map [:key :val] (vec sanitised))
        row-list (sort-by :key row-list)]
    {:fx/type :border-pane
     :top {:fx/type :label
           :style-class ["section-title"]
           :text "raw data"}
     :center {:fx/type :table-view
              :id "key-vals"
              :style-class ["table-view" "odd-rows"]
              :placeholder {:fx/type :text
                            :style-class ["table-placeholder-text"]
                            :text "(not installed)"}
              :column-resize-policy javafx.scene.control.TableView/CONSTRAINED_RESIZE_POLICY
              :columns (mapv table-column column-list)
              :items (or row-list [])}}))

(defn addon-detail-group-addons
  "displays a list of other addons that came grouped with this addon"
  [{:keys [addon]}]
  (let [opener #(component-instance (addon-fs-link (:dirname %)))
        column-list [{:text "" :style-class ["open-link-column"] :min-width 150 :pref-width 150 :max-width 150 :resizable false :cell-value-factory opener}
                     {:text "name" :cell-value-factory :dirname}]
        row-list (:group-addons addon)
        disabled? (empty? row-list)]
    {:fx/type :border-pane
     :top {:fx/type :label
           :style-class (if disabled? ["section-title", "disabled-text"] ["section-title"])
           :text "grouped addons"}
     :center {:fx/type :table-view
              :id "group-addons"
              :style-class ["table-view" "odd-rows"]
              :placeholder {:fx/type :text
                            :style-class ["table-placeholder-text"]
                            :text "(not grouped)"}
              :column-resize-policy javafx.scene.control.TableView/CONSTRAINED_RESIZE_POLICY
              :columns (mapv table-column column-list)
              :items (or row-list [])
              :disable disabled?}}))

(defn addon-detail-release-widget
  "displays a list of available releases for the given addon"
  [{:keys [addon]}]
  (let [install-button (fn [release]
                         (component-instance
                          (button "install" (async-handler #(cli/set-version addon release)))))
        column-list [{:text "" :style-class ["install-button-column"] :min-width 120 :pref-width 120 :max-width 120 :resizable false :cell-value-factory install-button}
                     {:text "name" :cell-value-factory #(or (:release-label %) (:version %))}]
        row-list (or (rest (:release-list addon)) [])
        disabled? (not (addon/releases-visible? addon))]
    {:fx/type :border-pane
     :top {:fx/type :label
           :style-class (if disabled? ["section-title", "disabled-text"] ["section-title"])
           :text "releases"}
     :center {:fx/type :table-view
              :id "release-list"
              :style-class ["table-view" "odd-rows"]
              :placeholder {:fx/type :text
                            :style-class ["table-placeholder-text"]
                            :text "(no releases)"}
              :column-resize-policy javafx.scene.control.TableView/CONSTRAINED_RESIZE_POLICY
              :columns (mapv table-column column-list)
              :items row-list
              :disable disabled?}}))

(defn addon-detail-pane
  "a place to elaborate on what we know about an addon as well somewhere we can put lots of buttons and widgets."
  [{:keys [fx/context addon-id tab-idx]}]
  (let [installed-addons (fx/sub-val context get-in [:app-state :installed-addon-list])
        catalogue (fx/sub-val context get-in [:app-state :db]) ;; worst case is actually not so bad ...
        ;;addon-id-keys (keys addon-id) ;; [dirname] [source source-id], [source source-id dirname]

        -id-dirname (:dirname addon-id)
        -id-dirname? (not (nil? -id-dirname))
        dirname-matcher (fn [addon]
                          (= -id-dirname (:dirname addon)))
        -id-source (select-keys addon-id [:source :source-id])
        -id-source? (not (empty? -id-source))
        source-matcher (fn [addon]
                         (= -id-source (select-keys addon [:source :source-id])))

        matcher (fn [addon]
                  (or (when -id-dirname?
                        (dirname-matcher addon))
                      (when -id-source?
                        (source-matcher addon))))

        ;; we may be given an installed addon, an ignored and unmatched addon, a catalogue entry so look in the installed
        ;; addon list first because it's smaller than the catalogue.
        addon (or (->> installed-addons (filter matcher) first)
                  (->> catalogue (filter matcher) first))

        ;; at this point addon may still be nil!
        ;; for example, an unmatched addon in the install dir is double clicked. we have a :dirname and that is all.
        ;; we can open the addon-detail pane but if we then delete the addon there is no longer any way to tie this addon detail pane
        ;; to addon data in the installed-addon-list (deleted) or the catalogue (no match).
        ;; we're forced to commit harikiri and close ourselves.
        ]
    (if (nil? addon)
      ;; this dodgy logic can be pushed back up the stack but we ultimately need to check for an addon and remove/exclude a tab if it exists.
      ;; deleting an addon doesn't affect the :tab-list, so we can't push this into #tabber, but perhaps we should re-check the open tabs when an addon is deleted?
      ;; todo: more thought required. for now it doesn't crash.
      (do (cli/remove-tab-at-idx tab-idx)
          {:fx/type :label :text "goodbye"})

      (let [notice-pane-filter (logging/log-line-filter-with-reports (core/selected-addon-dir) addon)]
        {:fx/type :border-pane
         :id "addon-detail-pane"
         :style-class ["addon-detail"]
         :top {:fx/type :v-box
               :children
               (utils/items
                [{:fx/type :label
                  :style-class ["title"]
                  :text (:label addon)}

                 {:fx/type :h-box
                  :style-class ["subtitle"]
                  :children (utils/items
                             [(when (:installed-version addon)
                                {:fx/type :label
                                 :style-class ["installed-version"]
                                 :text (:installed-version addon)})

                              (when (:update? addon)
                                {:fx/type :label
                                 :style-class ["version"]
                                 :text (format "%s available" (:version addon))})

                              ;; if installed, path to addon directory, clicking it opens file browser
                              (addon-fs-link (:dirname addon))

                              ;; order is important, a hyperlink may not exist, can't have nav jumping around.
                              (href-to-hyperlink addon)])}

                 (when-not (empty? (:description addon))
                   {:fx/type :label
                    :style-class ["description"]
                    :wrap-text true
                    :text (:description addon)})

                 {:fx/type addon-detail-button-menu
                  :addon addon}])}

         :center {:fx/type :grid-pane
                  :children [{:fx/type addon-detail-key-vals
                              :addon addon
                              :grid-pane/column 1
                              :grid-pane/hgrow :always
                              :grid-pane/vgrow :always}

                             {:fx/type addon-detail-release-widget
                              :addon addon
                              :grid-pane/column 2
                              :grid-pane/hgrow :always
                              :grid-pane/vgrow :always}

                             {:fx/type addon-detail-group-addons
                              :addon addon
                              :grid-pane/column 3
                              :grid-pane/hgrow :always
                              :grid-pane/vgrow :always}]}

         :bottom {:fx/type notice-logger
                  :tab-idx tab-idx
                  :section-title "notices"
                  :filter-fn notice-pane-filter}}))))

(defn addon-detail-tab
  [{:keys [tab tab-idx]}]
  {:fx/type :tab
   :id (:tab-id tab)
   :text (:label tab)
   :closable (:closable? tab)
   :on-closed (fn [_]
                (cli/remove-tab-at-idx tab-idx)
                (switch-tab INSTALLED-TAB))
   :content {:fx/type addon-detail-pane
             :tab-idx tab-idx
             :addon-id (:tab-data tab)}})

(defn tabber
  [{:keys [fx/context]}]
  (let [selected-addon-dir (fx/sub-val context get-in [:app-state :cfg :selected-addon-dir])
        dynamic-tab-list (fx/sub-val context get-in [:app-state :tab-list])
        static-tabs
        [{:fx/type :tab
          :text "installed"
          :id "installed-tab"
          :closable false
          :content {:fx/type installed-addons-pane}}
         {:fx/type :tab
          :text "search"
          :id "search-tab"
          :disable (nil? selected-addon-dir)
          :closable false
          ;; when the 'search' tab is selected, ensure the search field is focused so the user can just start typing
          :on-selection-changed (fn [ev]
                                  (when (-> ev .getTarget .isSelected)
                                    (let [text-field (-> ev .getTarget .getTabPane (.lookupAll "#search-text-field") first)]
                                      (Platform/runLater
                                       (fn []
                                         (-> text-field .requestFocus))))))
          :content {:fx/type search-addons-pane}}
         {:fx/type :tab
          :text "log"
          :id "log-tab"
          :closable false
          :content {:fx/type notice-logger}}]

        dynamic-tabs (map-indexed (fn [idx tab] {:fx/type addon-detail-tab :tab tab :tab-idx idx}) dynamic-tab-list)]
    {:fx/type :tab-pane
     :id "tabber"
     :tab-closing-policy javafx.scene.control.TabPane$TabClosingPolicy/ALL_TABS
     :tabs (into static-tabs dynamic-tabs)}))

(defn split-pane-button
  "the little button in the bottom right of the screen that toggles the split gui feature"
  [{:keys [fx/context]}]
  (let [log-lines (fx/sub-val context get-in [:app-state :log-lines])
        log-lines (cli/log-entries-since-last-refresh log-lines)

        ;; {:warn 1, :info 20}
        stats (utils/count-occurances log-lines :level)

        cmp (fn [kv1 kv2]
              (compare (get logging/level-map (first kv1))
                       (get logging/level-map (first kv2))))

        max-level (or (->> stats (sort cmp) last first)
                      logging/default-log-level)

        has-errors? (contains? stats :error)
        has-warnings? (contains? stats :warn)

        clf (partial clojure.pprint/cl-format nil)
        lbl (cond
              ;; '~:p' to pluralise using 's'
              ;; '~:*' to 'go back' a consumed argument
              ;; '~d' to format digit as a decimal (vs binary, hex, etc)
              has-errors? (clf "error~:p (~:*~d)" (:error stats)) ;; "error (1)", "errors (2)"
              has-warnings? (clf "warning~:p (~:*~d)" (:warn stats))
              :else "split")

        tooltip (when (or has-errors? has-warnings?) "since last refresh")]

    (button lbl (async-handler (fn []
                                 (cli/toggle-split-pane)
                                 (cli/change-notice-logger-level max-level)))
            {:style-class (cond
                            has-errors? "with-error"
                            has-warnings? "with-warning")
             :tooltip tooltip
             :tooltip-delay 400})))

(defn status-bar
  "this is the litle strip of text at the bottom of the application."
  [{:keys [fx/context]}]
  (let [num-matching-template "%s of %s installed addons found in catalogue."
        all-matching-template "all installed addons found in catalogue."
        catalogue-count-template "%s addons in catalogue."

        ia (fx/sub-val context get-in [:app-state :installed-addon-list])

        uia (filter :matched? ia)

        a-count (count (fx/sub-val context get-in [:app-state :db]))
        ia-count (count ia)
        uia-count (count uia)

        strings [(format catalogue-count-template a-count)
                 (if (= ia-count uia-count)
                   all-matching-template
                   (format num-matching-template uia-count ia-count))]]

    {:fx/type :h-box
     :id "status-bar"
     :children [{:fx/type :h-box
                 :id "status-bar-left"
                 :children [{:fx/type :text
                             :style-class ["text"]
                             :text (join " " strings)}]}
                {:fx/type :h-box
                 :id "status-bar-right"
                 :children [{:fx/type split-pane-button}]}]}))

;;


(defn app
  "returns a description of the javafx Stage, Scene and the 'root' node.
  the root node is the top-most node from which all others are descendents of."
  [{:keys [fx/context]}]
  (let [;; re-render gui whenever style state changes
        style (fx/sub-val context get :style)
        showing? (fx/sub-val context get-in [:app-state :gui-showing?])
        theme (fx/sub-val context get-in [:app-state :cfg :gui-theme])
        split-pane-on? (fx/sub-val context get-in [:app-state :gui-split-pane])]
    {:fx/type :stage
     :showing showing?
     :on-close-request exit-handler
     :title "strongbox"
     :width 1024
     :height 768
     :scene {:fx/type :scene
             :on-key-pressed (fn [e]
                               ;; ctrl-w
                               (when (and (.isControlDown e)
                                          (= (.getCode e) (KeyCode/W)))
                                 (let [;; when closing a tab, select the previous tab
                                       ;; UNLESS that previous tab is the last of the static tabs
                                       ;; then select the first of the static tabs
                                       prev-tab (dec (tab-index))
                                       prev-tab (if (= prev-tab (dec NUM-STATIC-TABS)) 0 prev-tab)]
                                   (cli/remove-tab-at-idx (tab-list-tab-index))
                                   (switch-tab prev-tab)))
                               nil)
             :stylesheets [(::css/url style)]
             :root {:fx/type :border-pane
                    :id (name theme)
                    :top {:fx/type menu-bar}
                    :center (if split-pane-on?
                              {:fx/type :split-pane
                               :orientation :vertical
                               :divider-positions [0.6]
                               :items [{:fx/type tabber}
                                       {:fx/type notice-logger}]}
                              {:fx/type tabber})
                    :bottom {:fx/type status-bar}}}}))

(defn start
  []
  (timbre/info "starting gui")
  (let [;; the gui uses a copy of the application state because the state atom needs to be wrapped
        state-template {:app-state nil,
                        :style (style)}
        gui-state (atom (fx/create-context state-template)) ;; cache/lru-cache-factory))
        update-gui-state (fn [new-state]
                           (swap! gui-state fx/swap-context assoc :app-state new-state))
        _ (core/state-bind [] update-gui-state)

        ;; css watcher for live coding
        _ (doseq [rf [#'style #'major-theme-map #'sub-theme-map #'themes]
                  :let [key (str rf)]]
            (add-watch rf key (fn [_ _ _ _]
                                (swap! gui-state fx/swap-context assoc :style (style))))
            (core/add-cleanup-fn #(remove-watch rf key)))

        ;; logging to app state for use in the UI
        _ (cli/init-ui-logger)

        ;; asynchronous searching. as the user types, update the state with search results asynchronously
        _ (cli/-init-search-listener)

        renderer (fx/create-renderer
                  :middleware (comp
                               fx/wrap-context-desc
                               (fx/wrap-map-desc (fn [_] {:fx/type app})))

                  ;; magic :(

                  :opts {:fx.opt/type->lifecycle #(or (fx/keyword->lifecycle %)
                                                      ;; For functions in `:fx/type` values, pass
                                                      ;; context from option map to these functions
                                                      (fx/fn->lifecycle-with-context %))})

        ;; don't do this, renderer has to be unmounted and the app closed before further state changes happen during cleanup
        ;;_ (core/add-cleanup-fn #(fx/unmount-renderer gui-state renderer))
        _ (swap! core/state assoc :disable-gui (fn []
                                                 (fx/unmount-renderer gui-state renderer)
                                                 ;; the slightest of delays allows any final rendering to happen before the exit-handler is called.
                                                 ;; only affects testing from the repl apparently and not `./run-tests.sh`
                                                 (Thread/sleep 25)))

        ;; on first load, because the catalogue hasn't been loaded
        ;; and because the search-field-input doesn't change,
        ;; and because the search component isn't re-rendered,
        ;; fake a change to get something to appear
        bump-search (fn []
                      (when-not (-> @core/state :search :term)
                        (cli/search "")))]

    (swap! core/state assoc :gui-showing? true)
    (fx/mount-renderer gui-state renderer)

    ;; `refresh` the app but kill the `refresh` if app is closed before it finishes.
    ;; happens during testing and causes a few weird windows to hang around.
    ;; see `(mapv (fn [_] (test :jfx)) (range 0 100))`
    (let [kick (future
                 (set-icon)
                 (core/refresh)
                 (bump-search))]
      (core/add-cleanup-fn #(future-cancel kick)))

    ;; calling the `renderer` will re-render the GUI.
    ;; useful apparently, but not being used.
    ;;renderer
    ))

(defn stop
  []
  (timbre/info "stopping gui")
  (when-let [unmount-renderer (:disable-gui @core/state)]
    ;; only affects tests running from repl apparently
    (unmount-renderer))
  (exit-handler)
  nil)
