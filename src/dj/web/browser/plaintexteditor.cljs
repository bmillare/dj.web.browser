(ns dj.web.browser.plaintexteditor
  (:require [goog.events :as events]
	    [goog.events.KeyHandler :as kh]
	    [goog.net.XhrIo :as xhr]
	    [goog.net.EventType :as et]
	    [goog.dom :as dom]
	    [goog.Uri :as guri]
	    [enfocus.core :as ef]
	    [cljs.reader :as reader]
	    [clojure.string :as string]
	    [dj.web.browser.core :as util])
  (:require-macros [enfocus.macros :as em]))

;; ----------------------------------------------------------------------
;; These functions are specific to an editor

;; Plain text editor

(defn state->txt [state]
  (apply str (interpose "\n" (:lines state))))

(defn txt->state [txt]
  {:cursor {:line 0 :column 0}
   :lines (into [] (string/split txt #"\n|\r"))})

(def editor-head (atom {:time (util/now)
			:state {:cursor {:line 0 :column 0}
				:lines [""]}}))

(defn initialize-head! [head-atom state]
  (reset! head-atom {:time (util/now)
		     :state state}))

(defn initialize-history! [history-atom head-atom]
  (reset! history-atom {@head-atom nil}))


;; forest stored in a map of state+time -> parent (just another
;; state+time) tradeoff simplicity and redundancy for efficiency, but
;; I assume structural sharing makes this ok. Modeled after git, where
;; we have commits of time and content/state, but here for simplicity,
;; hash = the whole data structure
(def editor-history (atom {}))

(def editor-log (atom ()))

(def editor-last-saved-content (atom nil))

(defn add-state! [history-atom head-atom f & args]
  (let [old-head @head-atom]
    (swap! head-atom (fn [oh]
		       {:time (util/now)
			:state (apply f (:state oh) args)}))
    (swap! history-atom
	   assoc
	   @head-atom
	   old-head)))

(defn become-parent! [history-atom head-atom]
  (swap! head-atom (fn [h]
		     (if-let [s (@history-atom h)]
		       s
		       h))))

;; state functions

(defn insert-character [state c]
  (let [{:keys [line column]} (:cursor state)]
    (-> state
	(update-in [:cursor :column] inc)
	(update-in [:lines line] util/insert-str column c))))

(defn insert-newline [state]
  (let [{:keys [line column]} (:cursor state)]
    (-> state
	(assoc-in [:cursor :column] 0)
	(update-in [:cursor :line] inc)
	(update-in [:lines] (fn [v]
			      (into (if (zero? line)
				      []
				      (subvec v 0 line))
				    (let [[x y] (util/split-str (v line)
								column)]
				      (into [x y]
					    (subvec v (inc line) (count v))))))))))

(defn kill-line [state]
  (let [{:keys [line]} (:cursor state)]
    (-> state
	(assoc-in [:cursor :column] 0)
	(assoc-in [:cursor :line] (if (zero? line)
				    0
				    (dec line)))
	(update-in [:lines] util/remove-nth line))))

(defn move-cursor [direction]
  (let [line-identity (fn [line v]
			line)
	line-fn ({:up (fn [line v]
			(if (zero? line)
			  0
			  (dec line)))
		  :down (fn [line v]
			  (if (= line (dec (count v)))
			    line
			    (inc line)))
		  :left line-identity
		  :right line-identity} direction)
	column-fn ({:up (fn [column line v]
			  (if (zero? line)
			    column
			    (let [prev-line-count (count (v (dec line)))]
			      (if (> column prev-line-count)
				prev-line-count
				column))))
		    :down (fn [column line v]
			    (if (= line (dec (count v)))
			      column
			      (let [next-line-count (count (v (inc line)))]
				(if (> column next-line-count)
				  next-line-count
				  column))))
		    :left (fn [column line v]
			    (if (zero? column)
			      0
			      (dec column)))
		    :right (fn [column line v]
			     (if (= column (count (v line)))
			       column
			       (inc column)))} direction)]
    (fn [state]
      (let [line (:line (:cursor state))]
	(-> state
	    (update-in [:cursor :line] line-fn (:lines state))
	    (update-in [:cursor :column] column-fn line (:lines state)))))))

(defn move-cursor-home [state]
  (assoc-in state [:cursor :column] 0))

(defn move-cursor-end [state]
  (let [{:keys [line]} (:cursor state)
	lines (:lines state)]
    (assoc-in state [:cursor :column] (count (lines line)))))

(let [move-left (move-cursor :left)]
  (defn delete-left-character [state]
    (let [{:keys [line column]} (:cursor state)
	  lines (:lines state)]
      (if (zero? column)
	(if (zero? line)
	  state
	  (-> state
	      (assoc-in [:lines (dec line)] (str (lines (dec line))
						 (lines line)))
	      (assoc-in [:cursor :column] (count (lines (dec line))))
	      (update-in [:cursor :line] dec)
	      (update-in [:lines] util/remove-nth line)))
	(-> state
	    (update-in [:lines line] util/delete-char-str (dec column))
	    move-left)))))

(defn render-state [state]
  (let [{:keys [line column]} (:cursor state)]
    (map-indexed (fn [idx line-str]
		   (doto (dom/createDom "div")
		     (em/at (em/do-> (em/add-class "line")
				     (if (= idx line)
				       (if (= column
					      (count line-str))
					 (let [cursor (dom/createDom "span")]
					   (em/at cursor
						  (em/do-> (em/add-class "cursor")
							   (em/content " ")))
					   (em/content (.substring line-str 0 column)
						       cursor))
					 (let [cursor (dom/createDom "span")]
					   (em/at cursor
						  (em/do-> (em/add-class "cursor")
							   (em/content (.substring line-str column (inc column)))))
					   (em/content (.substring line-str 0 column)
						       cursor
						       (.substring line-str (inc column) (count line-str)))))
				       (em/content (if (empty? line-str)
						     "\n"
						     line-str)))))))
		 (:lines state))))

(defn render-editor [state]
  (em/at js/document
	 ["div#editor"] (apply ef/en-content (render-state state))))

(defn render-minibuffer [content]
  (em/at js/document
	 ["div#minibuffer"] (em/content content)))

;; event functions

(def keymap (atom {}))
(def keymap-stack (atom (list {})))

(defn debug-info []
  (let [stack-count (count @keymap-stack)]
    (if (= 1 stack-count)
      (em/at js/document [:div#editor] (em/add-class "highlight-box"))
      (em/at js/document [:div#editor] (em/remove-class "highlight-box")))
    (str "title: " (:title @default-cljs-data) "\n"
	 "stack-count: " stack-count "\n"
	 "cursor-head: " (pr-str (:cursor (:state @editor-head))) "\n"
	 "modified: " (if (= @editor-last-saved-content
			     (:lines (:state @editor-head)))
			"--"
			"**") "\n"
			"editor-log: " (apply str "\n" (take 5 (interpose "\n" @editor-log))) "\n")))

(defn render-all []
  (render-editor (:state @editor-head))
  (render-minibuffer (debug-info)))

;;given a function that accepts state, does side effects
(def render-with (memoize (fn [f]
			    (fn [event]
			      (add-state! editor-history editor-head f)
			      (render-all)))))

(defn character [event]
  (let [c (String/fromCharCode (. event -charCode))]
    (if (= c "\0")
      nil
      (do (add-state! editor-history
		      editor-head
		      (fn [state]
			(insert-character state c)))
	  (render-all)))))

(defn push-bindings [new-bindings]
  (swap! keymap-stack (fn [kms]
			(let [top (first kms)]
			  (list* new-bindings
				 kms))))
  (reset! keymap (first @keymap-stack))
  (render-minibuffer (debug-info)))

(defn pop-bindings []
  (swap! keymap-stack (fn [kms]
			(if (= (count kms) 1)
			  kms
			  (rest kms))))
  (reset! keymap (first @keymap-stack))
  (render-minibuffer (debug-info)))

(def default-cljs-data (atom nil))

;; i want to easily refer to a event-wrapped version of a function
;; that is efficient in that there is only one function defined as so
;; (no duplicates), a memoized fn-generator
(def event-fn (memoize (fn
			 ([f w]
			    (fn [event]
			      (f w)))
			 ([f w x]
			    (fn [event]
			      (f w x)))
			 ([f w x y]
			    (fn [event]
			      (f w x y)))
			 ([f w x y z]
			    (fn [event]
			      (f w x y z)))
			 ([f w x y z & args]
			    (fn [event]
			      (apply f w x y z args))))))

(def insert-mode {goog.events.KeyCodes/SPACE character
		  goog.events.KeyCodes/ZERO character
		  goog.events.KeyCodes/ONE character
		  goog.events.KeyCodes/TWO character
		  goog.events.KeyCodes/THREE character
		  goog.events.KeyCodes/FOUR character
		  goog.events.KeyCodes/FIVE character
		  goog.events.KeyCodes/SIX character
		  goog.events.KeyCodes/SEVEN character
		  goog.events.KeyCodes/EIGHT character
		  goog.events.KeyCodes/NINE character
		  goog.events.KeyCodes/QUESTION_MARK character
		  goog.events.KeyCodes/A character
		  goog.events.KeyCodes/B character
		  goog.events.KeyCodes/C character
		  goog.events.KeyCodes/D character
		  goog.events.KeyCodes/E character
		  goog.events.KeyCodes/F character
		  goog.events.KeyCodes/G character
		  goog.events.KeyCodes/H character
		  goog.events.KeyCodes/I character
		  goog.events.KeyCodes/J character
		  goog.events.KeyCodes/K character
		  goog.events.KeyCodes/L character
		  goog.events.KeyCodes/M character
		  goog.events.KeyCodes/N character
		  goog.events.KeyCodes/O character
		  goog.events.KeyCodes/P character
		  goog.events.KeyCodes/Q character
		  goog.events.KeyCodes/R character
		  goog.events.KeyCodes/S character
		  goog.events.KeyCodes/T character
		  goog.events.KeyCodes/U character
		  goog.events.KeyCodes/V character
		  goog.events.KeyCodes/W character
		  goog.events.KeyCodes/X character
		  goog.events.KeyCodes/Y character
		  goog.events.KeyCodes/Z character
		  goog.events.KeyCodes/NUM_ZERO character
		  goog.events.KeyCodes/NUM_ONE character
		  goog.events.KeyCodes/NUM_TWO character
		  goog.events.KeyCodes/NUM_THREE character
		  goog.events.KeyCodes/NUM_FOUR character
		  goog.events.KeyCodes/NUM_FIVE character
		  goog.events.KeyCodes/NUM_SIX character
		  goog.events.KeyCodes/NUM_SEVEN character
		  goog.events.KeyCodes/NUM_EIGHT character
		  goog.events.KeyCodes/NUM_NINE character
		  goog.events.KeyCodes/NUM_MULTIPLY character
		  goog.events.KeyCodes/NUM_PLUS character
		  goog.events.KeyCodes/NUM_MINUS character
		  goog.events.KeyCodes/NUM_PERIOD character
		  goog.events.KeyCodes/NUM_DIVISION character
		  goog.events.KeyCodes/SEMICOLON character
		  goog.events.KeyCodes/DASH character
		  goog.events.KeyCodes/EQUALS character
		  goog.events.KeyCodes/COMMA character
		  goog.events.KeyCodes/PERIOD character
		  goog.events.KeyCodes/SLASH character
		  goog.events.KeyCodes/APOSTROPHE character
		  goog.events.KeyCodes/SINGLE_QUOTE character
		  goog.events.KeyCodes/OPEN_SQUARE_BRACKET character
		  goog.events.KeyCodes/BACKSLASH character
		  goog.events.KeyCodes/CLOSE_SQUARE_BRACKET character})

(def file-mode {goog.events.KeyCodes/W (fn [event]
					 (util/async-request (util/uri-cljs-data "/dj.web/write"
										 {:path (:path @default-cljs-data)
										  :content (state->txt (:state @editor-head))})
							     (fn [response]
							       (swap! editor-log conj (str "server response: " response))
							       (reset! editor-last-saved-content (:lines (:state @editor-head)))
							       (render-all)))
					 (pop-bindings))
		goog.events.KeyCodes/TAB (event-fn pop-bindings)
		goog.events.KeyCodes/ESC (event-fn pop-bindings)})

(def escape-mode {goog.events.KeyCodes/A (render-with move-cursor-home)
		  goog.events.KeyCodes/E (render-with move-cursor-end)
		  goog.events.KeyCodes/D (render-with kill-line)
		  goog.events.KeyCodes/H (render-with (move-cursor :left))
		  goog.events.KeyCodes/J (render-with (move-cursor :down))
		  goog.events.KeyCodes/K (render-with (move-cursor :up))
		  goog.events.KeyCodes/L (render-with (move-cursor :right))
		  goog.events.KeyCodes/ENTER (fn [event]
					       (add-state! editor-history editor-head insert-newline)
					       (pop-bindings)
					       (render-all))
		  goog.events.KeyCodes/Z (fn [event]
					   (become-parent! editor-history editor-head)
					   (render-all))
		  goog.events.KeyCodes/SEMICOLON (fn [event]
						   (let [c (String/fromCharCode (. event -charCode))]
						     (if (= c ";")
						       nil
						       (push-bindings  file-mode))))
		  goog.events.KeyCodes/TAB (event-fn pop-bindings)
		  goog.events.KeyCodes/ESC (event-fn pop-bindings)})

(def default-bindings {goog.events.KeyCodes/UP (render-with (move-cursor :up))
		       goog.events.KeyCodes/DOWN (render-with (move-cursor :down))
		       goog.events.KeyCodes/LEFT (render-with (move-cursor :left))
		       goog.events.KeyCodes/RIGHT (render-with (move-cursor :right))
		       goog.events.KeyCodes/MAC_ENTER nil
		       goog.events.KeyCodes/BACKSPACE (render-with delete-left-character)
		       goog.events.KeyCodes/TAB (event-fn push-bindings  escape-mode)
		       goog.events.KeyCodes/NUM_CENTER nil
		       goog.events.KeyCodes/ENTER (render-with insert-newline)
		       goog.events.KeyCodes/SHIFT nil
		       goog.events.KeyCodes/CTRL nil
		       goog.events.KeyCodes/ALT nil
		       goog.events.KeyCodes/PAUSE nil
		       goog.events.KeyCodes/CAPS_LOCK nil
		       goog.events.KeyCodes/ESC (event-fn push-bindings  escape-mode)
		       goog.events.KeyCodes/SPACE character
		       goog.events.KeyCodes/PAGE_UP nil
		       goog.events.KeyCodes/PAGE_DOWN nil
		       goog.events.KeyCodes/END cursor-end
		       goog.events.KeyCodes/HOME cursor-home
		       goog.events.KeyCodes/PRINT_SCREEN nil
		       goog.events.KeyCodes/INSERT nil
		       goog.events.KeyCodes/DELETE nil
		       goog.events.KeyCodes/ZERO character
		       goog.events.KeyCodes/ONE character
		       goog.events.KeyCodes/TWO character
		       goog.events.KeyCodes/THREE character
		       goog.events.KeyCodes/FOUR character
		       goog.events.KeyCodes/FIVE character
		       goog.events.KeyCodes/SIX character
		       goog.events.KeyCodes/SEVEN character
		       goog.events.KeyCodes/EIGHT character
		       goog.events.KeyCodes/NINE character
		       goog.events.KeyCodes/QUESTION_MARK character
		       goog.events.KeyCodes/A character
		       goog.events.KeyCodes/B character
		       goog.events.KeyCodes/C character
		       goog.events.KeyCodes/D character
		       goog.events.KeyCodes/E character
		       goog.events.KeyCodes/F character
		       goog.events.KeyCodes/G character
		       goog.events.KeyCodes/H character
		       goog.events.KeyCodes/I character
		       goog.events.KeyCodes/J character
		       goog.events.KeyCodes/K character
		       goog.events.KeyCodes/L character
		       goog.events.KeyCodes/M character
		       goog.events.KeyCodes/N character
		       goog.events.KeyCodes/O character
		       goog.events.KeyCodes/P character
		       goog.events.KeyCodes/Q character
		       goog.events.KeyCodes/R character
		       goog.events.KeyCodes/S character
		       goog.events.KeyCodes/T character
		       goog.events.KeyCodes/U character
		       goog.events.KeyCodes/V character
		       goog.events.KeyCodes/W character
		       goog.events.KeyCodes/X character
		       goog.events.KeyCodes/Y character
		       goog.events.KeyCodes/Z character
		       goog.events.KeyCodes/META nil
		       goog.events.KeyCodes/CONTEXT_MENU nil
		       goog.events.KeyCodes/NUM_ZERO character
		       goog.events.KeyCodes/NUM_ONE character
		       goog.events.KeyCodes/NUM_TWO character
		       goog.events.KeyCodes/NUM_THREE character
		       goog.events.KeyCodes/NUM_FOUR character
		       goog.events.KeyCodes/NUM_FIVE character
		       goog.events.KeyCodes/NUM_SIX character
		       goog.events.KeyCodes/NUM_SEVEN character
		       goog.events.KeyCodes/NUM_EIGHT character
		       goog.events.KeyCodes/NUM_NINE character
		       goog.events.KeyCodes/NUM_MULTIPLY character
		       goog.events.KeyCodes/NUM_PLUS character
		       goog.events.KeyCodes/NUM_MINUS character
		       goog.events.KeyCodes/NUM_PERIOD character
		       goog.events.KeyCodes/NUM_DIVISION character
		       goog.events.KeyCodes/F1 nil
		       goog.events.KeyCodes/F2 nil
		       goog.events.KeyCodes/F3 nil
		       goog.events.KeyCodes/F4 nil
		       goog.events.KeyCodes/F5 nil
		       goog.events.KeyCodes/F6 nil
		       goog.events.KeyCodes/F7 nil
		       goog.events.KeyCodes/F8 nil
		       goog.events.KeyCodes/F9 nil
		       goog.events.KeyCodes/F10 nil
		       goog.events.KeyCodes/F11 nil
		       goog.events.KeyCodes/F12 nil
		       goog.events.KeyCodes/NUMLOCK nil
		       goog.events.KeyCodes/SEMICOLON character
		       goog.events.KeyCodes/DASH character
		       goog.events.KeyCodes/EQUALS character
		       goog.events.KeyCodes/COMMA character
		       goog.events.KeyCodes/PERIOD character
		       goog.events.KeyCodes/SLASH character
		       goog.events.KeyCodes/APOSTROPHE character
		       goog.events.KeyCodes/SINGLE_QUOTE character
		       goog.events.KeyCodes/OPEN_SQUARE_BRACKET character
		       goog.events.KeyCodes/BACKSLASH character
		       goog.events.KeyCodes/CLOSE_SQUARE_BRACKET character
		       goog.events.KeyCodes/WIN_KEY nil
		       goog.events.KeyCodes/MAC_FF_META nil
		       goog.events.KeyCodes/WIN_IME nil})

(defn ^:export main []
  (reset! default-cljs-data (reader/read-string (util/node-raw-text "default-cljs-data")))
  (reset! keymap-stack (list default-bindings))
  (initialize-head! editor-head (txt->state (:content @default-cljs-data)))
  (reset! editor-last-saved-content (:lines (:state @editor-head)))
  (initialize-history! editor-history editor-head)
  (render-all)
  (reset! keymap (first @keymap-stack))
  (util/bind-keyhandler keymap))

