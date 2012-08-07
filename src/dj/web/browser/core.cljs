(ns dj.web.browser.core
  (:require [goog.events :as events]
	    [goog.events.KeyHandler :as kh]
	    [goog.net.XhrIo :as xhr]
	    [goog.net.EventType :as et]
	    [goog.dom.forms :as forms]
	    [goog.dom :as dom]
	    [goog.Uri :as guri]
	    [enfocus.core :as ef]
	    [cljs.reader :as reader]
	    [clojure.string :as string])
  (:require-macros [enfocus.macros :as em]))

(defn log
  "uses js/alert to print value then returns value"
  [val]
  (js/alert val)
  val)

(defn remove-nth
  "removes nth value in vector"
  [v n]
  (into (subvec v 0 n)
	(subvec v (inc n) (count v))))

(defn async-request
  "calls func with text from server response from request with URI"
  [uri func]
  (let [req (new goog.net.XhrIo)]
    (events/listen req goog.net.EventType/COMPLETE
		   (fn [event]
		     (func (. req (getResponseText)))))
    (. req (send uri "GET"))))

(defn uri-cljs-data
  "embeded cljs data inside a URL with key"
  ([key protocol domain port path cljs-data]
     (goog.Uri/create protocol
		      nil
		      domain
		      port
		      path
		      (when cljs-data
			(doto (goog.Uri.QueryData.)
			  (.add key cljs-data)))))
  ([key path cljs-data]
     (goog.Uri/create nil
		      nil
		      nil
		      nil
		      path
		      (when cljs-data
			(doto (goog.Uri.QueryData.)
			  (.add key cljs-data)))))
  ([path cljs-data]
     (uri-cljs-data "cljs-data" path cljs-data)))

(defn bind-keyhandler
  "takes a atom of key-bindings to functions that accept an event
  object. key-bindings is a map of google key codes to functions"
  ;; if you want to prevent default keybindings, you must put
  ;; keyhandler on js/document, not on element

  ;; then on event call preventDefault()
  ([key-bindings opts]
     (let [{:keys [parent-element prevent-default]} opts]
       (events/listen (goog.events/KeyHandler. parent-element)
		      "key"
		      (if prevent-default
			(fn [event]
			  (when-let [f (@key-bindings event/keyCode)]
			    (.preventDefault event)
			    (f event)))
			(fn [event]
			  (when-let [f (@key-bindings event/keyCode)]
			    (f event)))))))
  ([key-bindings]
     (bind-keyhandler key-bindings {:parent-element js/document
				    :prevent-default true})))

(defn node-raw-text [id]
  (goog.dom/getRawTextContent (goog.dom/getElement id)))

(defn read-cljs-dom [id]
  (reader/read-string
   (node-raw-text id)))

(defn input-value [id]
  (goog.dom.forms/getValue (goog.dom/getElement id)))

(defn change-window-url! [url]
  (set! (.. js/window -location -href)
	url))

(defn now
  "returns the current time"
  []
  (.getTime (js/Date.)))

;; ----------------------------------------------------------------------
;; String editing functions

(defn insert-str
  "inserts character c in string s at position n"
  [s n c]
  (str (.substring s 0 n)
       c
       (.substring s n (count s))))

(defn split-str
  "split string at index"
  [s n]
  [(.substring s 0 n)
   (.substring s n (count s))])

(defn delete-char-str
  "deletes character of index n in string s"
  [s n]
  (str (.substring s 0 n)
       (.substring s (inc n) (count s))))