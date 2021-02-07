(ns cs-snap-app.app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [cljs.pprint :as pp] ;;for editing and debugging code
            [clojure.string :as str])) 

;; --- APP STATE ---

; (def initial-todos {1 {:id 1, :title "Wash dishes", :done false}
;                     3 {:id 3, :title "Fold laundry", :done false}
;                     2 {:id 2, :title "Feed bunnies", :done false}})
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

;; --- Initialize App with sample data ---

(defonce init (do
                (add-todo "Wash dishes")
                (add-todo "Fold laundry")
                (add-todo "Feed cats")))

;; --- VIEWS ---

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

(defn todo-list []
  (let [items (vals @todos)]
    [:section.main
      [:ul.todo-list
        (for [todo items]
          ^{:key (:id todo)} [todo-item todo])]]))

(defn todo-entry []
  [:header.header
    [:h1 "todo items"]
    [todo-input {:class "new-todo"
                 :palceholder "what needs to be done?"
                 :on-save add-todo}]])

(defn footer-controls []
  [:footer.footer
    [:div "footer controls"]])

(defn app []
  [:div
    [:section.banner
      [:div.pie-chart
        [:h2 "Complete vs. incomplete tasks"]]
      [:div.bar-chart 
        [:h2 "Word count of tasks"]]]
    [:section.todo-app ;;class todoapp
      [todo-entry]
      (when (seq @todos)
        [:div
          [todo-list]
          [footer-controls]])]
      [:footer.info
        [:p "Double-click to edit a todo"]]]) 
      

;; --- RENDER ---

(defn render []
  (rdom/render [app] (.getElementById js/document "root")))

(defn ^:export main []
  (render))

(defn ^:dev/after-load reload! []
  (render))
