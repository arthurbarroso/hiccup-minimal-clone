(ns hiccup.core
  (:gen-class))

(def a1 [:p])
(def a2 [:p "something"])
(def a3 [:p {:prop "a"}])
(def a4 [:p {:prop "a"} "something"])
(def a5 [:img {:src "./smile.png"}])
(def b1 [:div [:p "a"]])
(def b2 [:div [:p "a"] [:p "b"]])
(def b3 [:div '([:p "a"] [:p "b"])])

(def single-tag #{:img :hr :input :meta :embed :link})

(def svg-a
  [:svg
   {:width 100
    :height 100
    :viewBox "-1 -1 100 100"
    :xmlns "http://www.w3.org/2000/svg"}
   [:g
    {:transform "scale(1)"}
    [:rect {:width 10 :height 20 :x -5.0 :y -10.0}]]])


(defn hiccup?
  [item]
  (and (vector? item)
       (keyword? (first item))))

(defn compile-style
  [m]
  (let [f (fn [[k v]] (str (name k) ":" (str v) ";"))]
    (->> m
         (map f)
         (apply str))))

(defn compile-property
  [[k v]]
  (let [v (if (map? v) (compile-style v) (str v))]
    (str (name k) "=\"" v "\"")))


(defn compile-properties [m]
  (->> m
       (map compile-property)
       (interpose " ")
       (#(conj % " "))
       (apply str)))

(def exm {:style {:width "100px"
                  :height "200px"}})



(declare html)

(defn compile-item
  [[k & content]]
  (let [props (first content)
        props-string (when (map? props) (compile-properties props))
        opening-tag (str "<" (name k) props-string (when (single-tag k) "/") ">")
        closing-tag (when (not (single-tag k)) (str "</" (name k) ">"))
        content (if (map? props) (rest content) content)]
    (cond
      (or (empty? content) (nil? content))
      (str opening-tag closing-tag)

      (and (= 1 (count content)) (string? (first content)))
      (str opening-tag (first content) closing-tag)

      :else (str opening-tag (apply html content) closing-tag)
      )))


(defn html
  [& content]
  (cond
    (and (= 1 (count content)) (string? (first content)))
    (first content)

    (and (= 1 (count content)) (hiccup? (first content)))
    (apply compile-item content)

    (and (= 1 (count content)) (seq? (first content)))
    (apply str (map compile-item (first content)))

    :else (apply str (map html content))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (hiccup? [:p "paragraph"])
  (hiccup? [])
  (name :key)
  (:a {:a "a"})
  (compile-properties exm)
  (html a1)
  (html a2)
  (html a3)
  (html a4)
  (html a5)
  (html b1)
  (html b2)
  (html b3)
  (html svg-a)
  (spit "drawing.svg" (html svg-a))
  )