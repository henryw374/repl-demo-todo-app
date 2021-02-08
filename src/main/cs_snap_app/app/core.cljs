(ns cs-snap-app.app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [cljs.pprint :as pp] ;;for editing and debugging code
            [clojure.string :as str])) 

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
                        (into (empty m))))))) ;;refactor into an mmap function 43:40 2nd vid

;; --- Initialize App with sample data ---
; (defn- random-point []
;   (js/Math.floor (* (js/Math.random) 10)))

(defn- random-point [total-count]
  total-count)

(defonce chart-data
  (let [points (map random-point (range 4))]              ;; <1>
    (r/atom {:points points
             :chart-max (reduce max 1 points)})))

(defonce init (do
                (add-todo "Wash dishes")
                (add-todo "Fold laundry")
                (add-todo "Feed cats")
                (add-todo "Water plants")))

; (def chart-data [{:title 'active' :value :active}
;                  {:title 'all' :value :all}
;                  {:title 'done' :value :done}])

(def chart-width 300)
(def chart-height 120)
(def bar-spacing 1)

(defn word-count []
  )

;; --- VIEWS ---

(defn calc [{:keys [active-count done-count total-count] :as data}]
  (let [h (/ total-count 100)]
    (assoc data :done-count (/ total-count (* h h)))))

(defn concentric-circles [showing]
  (let [items (vals @todos)
        done-count (count (filter :done items))
        active-count (- (count items) done-count)
        total-count (+ (count items))]
  [:div.pie-chart
    [:span.todo-count        
      [:h4 "Complete vs. incomplete tasks"]
      [:svg {:style {:width "150px"
                    :height "150px"}}
      ; [:circle {:r 50 :cx 75 :cy 75 :fill "white"}]
      [:circle {:r 50, :cx 75, :cy 75, :fill "green" :stroke "blue" :stroke-width 25 :stroke-dasharray 100 * active-count / total-count}]
      ; [:circle {:r 25, :cx 75, :cy 75, :fill "transparent" :stroke "tomato" :stroke-width 25 :stroke-dasharray "calc(100 * (done-count / total-count))"}]
            [:circle {:r 25, :cx 75, :cy 75, :fill "transparent" :stroke "tomato" :stroke-width 25 :stroke-dasharray 100 * done-count / total-count}]
      ; [:circle {:r done-count, :cx 75, :cy 75, :fill "white"}]
      [:transform "rotate(-90) translate(-20)"]
      ; [:path {:fill "none"
      ;         :d "M 30,40 C 100,40 50,110 120,110"}]
              ]]]))

(defn pie-chart [showing]
  (let [items (vals @todos)
        done-count (count (filter :done items))
        active-count (- (count items) done-count)
        total-count (+ (count items))
        props-for (fn [kw]
                    {:class (when (= kw @showing) "selected")
                      :on-click #(reset! showing kw)
                      :href "#"})]
                      
  [:div.pie-chart
    [:span.todo-count        
    [:h4 "Complete vs. incomplete tasks"]
      [:strong active-count] " " (case active-count 1 "item" "items") " left"]
      (js/console.log "active" :done)
      (js/console.log "done" 100 * done-count / total-count)
      (js/console.log "total" total-count)
        [:div.pie

          [:div.pie-segment {:style {:backgroundColor "#90EE90"}
                                     :data-start 0 
                                     :data-value [active-count] }
                                     [:p "active" active-count]]
          [:div.pie-segment {:style {:value done-count
                                     :backgroundColor "#777777"}
                                     :data-start active-count 
                                     :height #(active-count)
                                     :data-value done-count }
                                     [:p "done" done-count]]
          [:div.pie-segment {:style {:value total-count
                                     :backgroundColor "#qqq"}
                                     :data-start done-count 
                                     :data-value total-count }
                                     [:p "total" total-count]]]])) ;;showing all active and done options -47min


(defn bar-chart []
  [:div.bar-chart 
  [:h4 "Word count of tasks"]
  (let [{:keys [points chart-max]} @chart-data             ;; <2>
          bar-width (- (/ chart-width (count points))
                      bar-spacing)]
      [:svg.chart {:x 0 :y 0
                  :width chart-width :height chart-height}
        (for [[i point] (map-indexed vector points)          ;; <3>
              :let [x (* i (+ bar-width bar-spacing))        ;; <4>
                    pct (- 1 (/ point chart-max))
                    bar-height (- chart-height (* chart-height pct))
                    y (- chart-height bar-height)]]
          [:rect {:key i                                     ;; <5>
                  :x x :y y
                  :width bar-width
                  :height bar-height}])])])

(defn todo-input [{:keys [title on-save on-stop]}]
  (let [input-text (r/atom title)
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
    [:h1 "to-do items"]
    [todo-input {:class "new-todo"
                 :placeholder "Create new to do"
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
      [:section.banner
        [concentric-circles]
        [pie-chart showing]
        [bar-chart]]
      [:section.todo-app
        [todo-entry]
        (when (seq @todos)
          [:div
            [todo-list showing]
            [footer-controls]])]
        [:footer.info
          [:p "Double-click to edit a todo"]]])))

;; --- RENDER ---

(defn render []
  (rdom/render [app] (.getElementById js/document "root")))

(defn ^:export main []
  (render))

(defn ^:dev/after-load reload! []
  (render))
