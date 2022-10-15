;; # Do What I Mean Viewer

;; This namespace shows how to use Clerk's viewer api to build a [Do
;; What I Mean (DWIM)](https://en.wikipedia.org/wiki/DWIM) viewer that
;; automatically tries to guess the viewer for a given data
;; structure.

;; This can be convienient but comes with the tradeoff not not always
;; being right. For example, it's not easy to choose between plotly
;; and vega.

;; We will show this here for two viewers, `vega` and `hiccup` but
;; this approach could of course be extended to more.
(ns nextjournal.clerk-dwim
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as viewer]))

;; For now the predicate in the `vega-viewer` is very simple: applying
;; to all maps with a `:data` key in them. We are calling
;; `viewer/get-safe` because `clojure.core/get` will throw when passed
;; a sorted-map with non-keyword keys in it.
(def vega-viewer
  {:pred (fn [x] (and (map? x)
                     (viewer/get-safe x :data)))
   :transform-fn viewer/vl})


;; We apply the hiccup viewer to vectors where the first element is a
;; keyword, the second element can be a map and its other elements are
;; again hiccup elements following these rules – or a string.
(def hiccup-viewer
  {:pred (fn hiccup-el? [x] (and (vector? x)
                                (keyword? (first x))
                                (every? (some-fn string? hiccup-el?)
                                        (cond-> (rest x)
                                          (map? (second x)) rest))))
   :transform-fn viewer/html})

(def all-dwim-viewers
  [hiccup-viewer vega-viewer])

;; We are adding these viewers to this `*ns*` here.

^:nextjournal.clerk/no-cache
(clerk/add-viewers! [hiccup-viewer vega-viewer])

;; If you with to modify Clerk's default viewers, you can call the
;; following function.

(defn install-to-clerk-defaults! []
  (viewer/reset-viewers! :default (viewer/add-viewers (viewer/get-default-viewers) all-dwim-viewers)))

;; Let's try this out!

;; These examples are wrapped in `clerk/example` so they show up when
;; viewed through Clerk. When evaluated as normal Clojure library code
;; `clerk/example` evaluates to `nil`, just like
;; `clojure.core/comment`.
(clerk/example
  {:data {:values [{"a" "A" "b" 128} {"a" "B" "b" 100} {"a" "C" "b" 43}
                   {"a" "D" "b" 91} {"a" "E" "b" 81} {"a" "F" "b" 53}
                   {"a" "G" "b" 19} {"a" "H" "b" 87} {"a" "I" "b" 52}]}
   :width 400 :height 140
   :mark "bar"
   :encoding {"x" {"field" "a" "type" "nominal" "axis" {"labelAngle" 0}}
              "y" {"field" "b" "type" "quantitative"}}}

  (sorted-map "hello" :world)

  [:strong "we " [:em "love"] " hiccup, right?"]

  (vec (range 100)))
