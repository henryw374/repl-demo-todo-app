(ns cs-snap-app.app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [cljs.pprint :as pp] ;;for editing/debugging code
            [clojure.string :as str])) ;;!!!!need to get rid of this

;; --- APP STATE ---

;; sorted map, will sort by ids
; (def initial-todos-sorted (into (sorted-map) initial-todos))  
;; atom - mutable wrapper around an immutable data structure
;; this is a reagent atom, reagent atoms react to changes, keeps track of components and re renders when atom has changed
;;ratom
(defonce todos (r/atom (sorted-map)))

(defonce counter (r/atom 0))

;; --- Watch the State ---

(add-watch todos :todos
            (fn [key _atom _old-state new-state]
              (println "---" key "atom changed ---")
              (pp/pprint new-state)))

;; --- UTILITIES ---

(defn add-todo [text]
  (let [id (swap! counter inc)
    new-todo {:id id, :title text, :done false}]
    (swap! todos assoc id new-todo))) ;;~30min

(defn toggle-done [id]
  (swap! todos update-in [id :done] not))

(defn delete-todo [id]
  (swap! todos dissoc id))

(defn save-todo [id title]
  (swap! todos assoc-in [id :title] title))

(defn complete-all-toggle [b]
  (let [g #(assoc-in % [1 :done] b)]
    (swap! todos (fn [m]
                  (->> m
                      (map g)
                      (into (empty m)))))))

(defn clear-completed []
  (let [g #(get-in % [1 :done])]
    (swap! todos (fn [m]
                    (->> m
                        (remove g)
                        (into (empty m))))))) ;;refactor into an mmap? 43:40 2nd

;; --- Initialize App with sample data ---

(defonce init (do
                (add-todo "Wash the dishes")
                (add-todo "Dry and fold laundry")
                (add-todo "Feed cats")
                (add-todo "Water plants along windows")
                (add-todo "Drink water!")
                (add-todo "Breath")))

(def radius-lg 10)
(def radius-sm 5)
(def chart-width 210)
(def chart-height 100)
(def bar-spacing 2)

;; --- VIEWS ---

(defn ratio [x y] (/ x y))

(defn percentage [fn] (* (fn) 100))

(defn pie-chart [showing]
  (let [items (vals @todos)
        done-count (count (filter :done items))
        active-count (- (count items) done-count)
        total-count (+ (count items))
        circumf-circ (* 2 3.14 radius-sm)
        percent-val (percentage #(ratio done-count total-count)) 
        percent-circ (/ (* percent-val circumf-circ) 100)] ;;!!!figure out tech terms for stroke-dasharray
        ; props-for (fn [kw]
        ;     {:class (when (= kw @showing) "selected")
        ;       :on-click #(reset! showing kw)
        ;       :href "#"})]
  [:div.pie-chart
    [:h4 "Complete vs. incomplete tasks"]
    [:div.pie-flex
      [:svg.pie {:x 0 :y 0 :width chart-width :height chart-height :viewBox "0 0 20 20"}
        [:circle {:r radius-lg :cx 10 :cy 10 :fill "turquoise"}] 
        [:circle {:r radius-sm :cx 10 :cy 10 :fill "turquoise"
                  :stroke "tomato" 
                  :stroke-width 10
                  :stroke-dasharray [percent-circ circumf-circ]
                  :transform "rotate(-90) translate(-20)"}]]
      [:span.pie-count
        [:p "Complete: " done-count]
        [:p "Incomplete: " active-count]
        [:p "Total: " total-count]]]]))                  
;;showing active vs non values-47min

(defn- word-count []
  (let [items (vals @todos)]
  (reduce conj []
        (map (fn [items] (count (str/split (get items :title) #"\s+")))
             (filter (fn [items] (>= (get items :id) 1)) items)))))

;; which is better??? into vs conj
; (defn- random-point []
;   (let [items (vals @todos)]
;   (into []
;         (map (fn [items] (count (str/split (get items :title) #"\s+")))
;              (filter (fn [items] (>= (get items :id) 1)) items)))))

(defonce chart-data ;;this isnt updating with new to dos (defonce?)
  (let [items (vals @todos) points (word-count)]
        ; title-count (count (map second items))
      (println "points" points)
      (r/atom {:points points
               :chart-max (reduce max 1 points)})))

(defn bar-chart []
  [:div.bar-chart 
  [:h4 "Word count of tasks"]
    (let [{:keys [points chart-max]} @chart-data
        bar-width (- (/ chart-width (count points))
                        bar-spacing)]
        [:svg.bar {:x 0 
                    :y 0
                    :width chart-width 
                    :height chart-height}
          (for [[i point] (map-indexed vector points)
                :let [x (* i (+ bar-width bar-spacing))
                      pct (- 1 (/ point chart-max))
                      bar-height (- chart-height (* chart-height pct))
                      y (- chart-height bar-height)]]
            [:rect {:key i
                    :x x :y y
                    :width bar-width
                    :height bar-height}])])])

(defn todo-input [{:keys [title on-save on-stop]}]
  (let [input-text (r/atom title) ;;add something similar for updating word count?
        update-text #(reset! input-text %)
        stop #(do (reset! input-text "")
                  (when on-stop (on-stop)))
        save #(let [trimmed-text (-> @input-text str str/trim)]
                (if-not (empty? trimmed-text) (on-save trimmed-text))
                (stop))
        key-pressed #(case %
                        "Enter" (save)
                        "Esc" (stop)
                        "Escape" (stop)
                        nil)]

  (fn [{:keys [class placeholder]}]
      [:input {:class class
              :placeholder placeholder
              :auto-focus true
              :type "text"
              :value @input-text
              :on-blur save
              :on-change #(update-text (.. % -target -value))
              :on-key-down #(key-pressed (.. % -key))}])))

(defn todo-item [_props-map]
  (let [editing (r/atom false)]
   (fn [{:keys [id title done]}]
    [:li {:class (str (when done "completed ")
                      (when @editing "editing"))}
      [:div.view
        [:input {:type "checkbox" 
                :class "toggle" 
                :checked done 
                :on-change #(toggle-done id)}]
        [:label {:on-double-click #(reset! editing true)} title]
        [:button.destroy {:on-click #(delete-todo id)} [:p "X"]]]
        (when @editing
          [todo-input {:class "edit"
                       :title title
                       :on-save (fn [text] (save-todo id text))
                       :on-stop #(reset! editing false)}])])))

(defn todo-list [showing]
  (let [items (vals @todos)
        filter-fn (case @showing
                    :done :done
                    :active (complement :done)
                    :all identity)
        visible-items (filter filter-fn items)
        all-complete? (every? :done items)]
    [:section.main
      [:input {:id "toggle-all"
               :class "toggle-all" ;:mark all as done
               :type "checkbox"
               :checked all-complete?
               :on-change #(complete-all-toggle (not all-complete?))}]
      [:label {:for "toggle-all"} "Mark all as complete"]
      [:ul.todo-list
        (for [todo visible-items]
          ^{:key (:id todo)} [todo-item todo])]]))

(defn todo-entry []
  [:header.header
    [:h1 "Order of the day"]
    [todo-input {:class "new-todo"
                 :placeholder "I need to.."
                 :on-save add-todo}]])

(defn footer-controls []
  (let [items (vals @todos)
        done-count (count (filter :done items))]
    [:footer.footer
      (when (pos? done-count)
      [:button.clear-completed {:on-click clear-completed} "Clear completed"])]))

(defn app []
  (let [showing (r/atom :all)] ; showing can be all active or done
  (fn []
    [:div
    [:div.content
      [:section.banner
        [pie-chart showing] ;;prob dont need showing there
        [bar-chart]]
      [:section.todo-app
        [todo-entry]
        (when (seq @todos)
          [:div
            [todo-list showing]
            [footer-controls]])]]
        [:footer.info
          ; [:p "Double-click to edit a todo"] ;;change into tooltip??
          [:p "Snap eHealth Technical Challenge 2021"]]])))

;; --- RENDER ---

(defn render []
  (rdom/render [app] (.getElementById js/document "root")))

(defn ^:export main []
  (render))

(defn ^:dev/after-load reload! []
  (render))
