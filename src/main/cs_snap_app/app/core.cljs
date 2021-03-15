(ns cs-snap-app.app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [cljs.pprint :as pp]
            [clojure.string :as str]))

;; --- APP STATE ---

(defonce todos (r/atom (sorted-map)))

(defonce counter (r/atom 0))

;; --- Watch the State ---

(add-watch todos :todos
            (fn [key _atom _old-state new-state]
              (pp/pprint new-state)))

;; --- UTILITIES ---

(defn add-todo [text]
  (let [id (swap! counter inc)
    new-todo {:id id, :title text, :done false}]
    (swap! todos assoc id new-todo)))

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
                        (into (empty m)))))))

;; --- Initialize App with some sample data ---

(defonce init (do
              (add-todo "Wash the dishes")
              (add-todo "Dry and fold laundry")
              (add-todo "Feed the cats")
              (add-todo "Water the window plants")
              (add-todo "Drink water!")
              (add-todo "Breathe")))

(def radius-lg 10)
(def radius-sm 5)
(def chart-width 240)
(def chart-height 130)
(def pie-width 140)
(def pie-height 140)
(def bar-spacing 2)

;; --- VIEWS ---

(defn ratio [x y] (/ x y))

(defn percentage [fn] (* (fn) 100))

(defn pie-chart []
  (let [items (vals @todos)
        done-count (count (filter :done items))
        active-count (- (count items) done-count)
        total-count (+ (count items))
        circumf-circ (* 2 3.14 radius-sm)
        percent-val (percentage #(ratio done-count total-count)) 
        percent-circ (/ (* percent-val circumf-circ) 100)]
  [:div.pie-chart
    [:h4 "Complete vs. incomplete tasks"]
    [:div.pie-flex
      [:svg.pie {:x 0 :y 0 :width pie-width :height pie-height :viewBox "0 0 20 20"}
        [:circle {:r radius-lg :cx 10 :cy 10 :fill "rgb(36, 148, 171)"}] 
        [:circle.pie-circ {:r radius-sm :cx 10 :cy 10 :fill "rgb(36, 148, 171)"
                  :stroke "rgb(240, 123, 102)" 
                  :stroke-width 10
                  :stroke-dasharray [percent-circ circumf-circ]
                  :transform "rotate(-90) translate(-20)"}]]
      [:span.pie-count
        [:div#keyC] [:p.key "Complete: " done-count]
        [:div#keyX] [:p.key "Incomplete: " active-count]
        [:div#key] [:p.key "Total: " total-count]]]]))                  

(defn- word-count []
  (let [items (vals @todos)]
  (reduce conj []
    (map (fn [items] (count (str/split (get items :title) #"\s+")))
        (filter (fn [items] (>= (get items :id) 1)) items)))))

(defn bar-chart []
(let [items (vals @todos) 
      points (word-count) 
      chart-max (reduce max 1 points)]
  [:div.bar-chart 
  [:h4 "Word count of tasks"]
    (let [keys [points chart-max]
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
                    :height bar-height}])])]))

(defn todo-input [{:keys [title on-save on-stop]}]
  (let [input-text (r/atom title)
        update-text #(reset! input-text %)
        stop #(do (reset! input-text "")
                  (when on-stop (on-stop)))
        save #(let [trimmed-text (-> @input-text str str/trim)]
                (if-not (empty? trimmed-text) (on-save trimmed-text))
                (stop))
        key-pressed #(case %2
                        "Enter" (save)
                        "Esc" (stop)
                        "Escape" (do (stop)
                                     (.blur %1))
                        nil)]

  (fn [{:keys [class placeholder]}]
      [:div.input-container
      [:input {:class class
              :placeholder placeholder
              :auto-focus true
              :type "text"
              :value @input-text
              :on-blur save
              :on-change #(update-text (.. % -target -value))
              :on-key-down #(key-pressed (.. % -target) (.. % -key))}]])))

(defn todo-item []
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
      [:ul.todo-list
        (for [todo visible-items]
          ^{:key (:id todo)} [todo-item todo])]]))

(defn todo-entry []
  [:header.header
    [:h1 "Order of the day"]
    [todo-input {:class "new-todo"
                 :placeholder "After coffee.."
                 :on-save add-todo}]])

(defn controls []
  (let [items (vals @todos)
        done-count (count (filter :done items))
        all-complete? (every? :done items)]
    [:footer.info
        [:span.toggle
          [:input {:id "toggle-all"
               :class "toggle-all"
               :type "checkbox"
               :checked all-complete?
               :on-change #(complete-all-toggle (not all-complete?))}]
      [:span "Mark all"]]
      (when (pos? done-count)
      [:button.clear-completed {:on-click clear-completed} "Clear completed"])]))

(defn app []
  (let [showing (r/atom :all)]
  (fn []
    [:div
    [:div.content
      [:section.banner
        [pie-chart]
        [bar-chart]]
      [:section.todo-app
        [todo-entry]
        (when (seq @todos)
          [:div
            [todo-list showing]
            [controls]])]]
        [:footer.footer
          [:p "ClojureScript Fun 2021"]]])))

;; --- RENDER ---

(defn render []
  (rdom/render [app] (.getElementById js/document "root")))

(defn ^:export main []
  (render))

(defn ^:dev/after-load reload! []
  (render))
