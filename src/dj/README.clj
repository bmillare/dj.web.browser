(ns dj.README
  (:require [dj]))

;; I use my own dependency manager but I expanded the process so you
;; should be able to make your own project.clj file from this
;; information.

(def dependencies
     {:maven '[[org.clojure/core.memoize "0.5.0"]]
      :source ["git://github.com/cgrand/enlive.git"]
      :classpath ["usr/src/enfocus/project/cljs-src/"
		  "usr/src/dj.web.browser/src"]
      :cljs true
      :cljs-libs ["/home/bmillare/dj/usr/src/clojurescript/closure/library/third_party/closure"]})

(defn resolve-project-dependencies []
  (dj/add-dependencies! (:maven dependencies))
  (dj/add-dependencies! (:source dependencies))
  (when (:cljs dependencies)
    (dj/add-cljs-to-classpath!))
  (doseq [p (:classpath dependencies)]
    (dj/add-to-classpath! p)))

(resolve-project-dependencies)

(require '[dj.web.cljs :as cljs]
	 '[clojure.java.io :as io]
	 '[net.cgrand.enlive-html :as html])

;; ----------------------------------------------------------------------

;; This is some templating code I copy pasted from my own private
;; library. You should use whatever you are most comfortable with.

(defn html5 [content]
  (apply str
	 "<!DOCTYPE html>"
	 (html/emit* {:tag :html
		      :attrs {:lang "en"}
		      :content content})))

(defn head [content]
  {:tag :head
   :content content})

(defn base [m]
  (html5 [(head [{:tag :title
		  :content [(:title m)]}
		 {:tag :style
		  :content [(:style m)]}
		 {:tag :script
		  :content [(:script m)]}])
	  (:body m)]))

;; ----------------------------------------------------------------------

(def project-dir (.getParentFile (.getParentFile (io/file (.getFile (clojure.java.io/resource "dj/README.clj"))))))

(defn emit-example []
  (base {:title "Example Editor"
	 :style "
@-webkit-keyframes blink {
    0% {
        background-color: #cd0000;
        color: #404040;
    }
    50% {
        background-color: #cd0000;
        color: #404040;
    }
    51% {
        background-color: #404040;
        color: #d9d9d9;
    }
    100% {
        background-color: #404040;
        color: #d9d9d9;
    }
}
.cursor {
    -webkit-animation-name: blink;
    -webkit-animation-duration: 1s;
    -webkit-animation-iteration-count: infinite;
    -webkit-animation-timing-function: ease;
    -webkit-animation-direction: normal;
}
div.highlight-box {
    border: 1px solid #FF0000;
}
"
	 :script (cljs/emit-cljs (let [cljs-file (io/file project-dir "dj/web/browser/plaintexteditor.cljs")]
				   {:cljs-dir project-dir
				    :dependencies [cljs-file
						   (io/file project-dir "dj/web/browser/core.cljs")]
				    :file cljs-file
				    :libs (:cljs-libs dependencies)
				    :optimizations :advanced}))
	 :body {:tag :body
		:content [{:tag :div
			   :attrs {:id "editor"}}
			  {:tag :div
			   :attrs {:id "minibuffer"}}
			  {:tag :div
			   :attrs {:id "default-cljs-data"}
			   :content [(pr-str {:title "Editor"
					      :path "default/path"
					      :content "Default content"})]}
			  {:tag :script
			   :content ["dj.web.browser.plaintexteditor.main();"]}]}}))

(defn main []
  (spit (io/file project-dir "sample.html") (emit-example)))

(main)