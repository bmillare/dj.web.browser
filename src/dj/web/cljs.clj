(ns dj.web.cljs
  (:require [cljs.closure]
	    [clojure.java.io :as io]
	    [dj.toolkit :as tk]
	    [clojure.core.memoize :as memoize]))

(defn files-timestamp [^java.io.File files]
  (reduce (fn [m f]
	    (assoc m f (.lastModified f)))
	  {}
	  files))

(def emit-cljs* (memoize/memo-fifo
		 (fn [cljs-dir timestamp-map file libs optimizations]
		   (let [options {:optimizations optimizations
				  :output-dir (io/file cljs-dir
						       "out")
				  :libs libs}]
		     (cljs.closure/build file
					 options)))
		 2))

(defn emit-cljs
  "accepts map :cljs-dir :dependencies :file and :libs"
  [m]
  (emit-cljs* (:cljs-dir m)
	      (files-timestamp (:dependencies m))
	      (:file m)
	      (:libs m)
	      (:optimizations m)))

(defmacro setp!
  "sets property of object"
  [obj prop v]
  `(set! (. ~obj ~(symbol (str "-" (name prop))))
	 ~v))

(defmacro json
  "converts map to json. sets up doto + set! on properties. keys must
  be plain symbols"
  [m]
  `(doto (new js/Object)
     ~@(map (fn [[k v]]
	      `(cljs/setp! ~k ~v))
	    m)))

(defmacro tag
  "convenience macro for creating elements. m is a :attrs map"
  [name m content]
  `(apply goog.dom/createDom ~name
	  (cljs/json ~m)
	  ~content))