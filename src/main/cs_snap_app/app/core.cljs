(ns cs-snap-app.app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [cljs.pprint :as pp])) ;;for editing and debugging code

;; --- APP STATE ---

(def initial-todos {1 {:id 1, :title "Wash dishes", :done false}
                    3 {:id 3, :title "Fold laundry", :done false}
                    2 {:id 2, :title "Feed bunnies", :done false}})
;; sorted map, will sort by ids
(def initial-todos-sorted (into (sorted-map) initial-todos))  
;; atom - mutable wrapper around an immutable data structure
;; this is a reagent atom, reagent atoms react to changes, keeps track of components and re renders when atom has changed
;;ratom
(defonce todos (r/atom initial-todos-sorted))

(defonce counter (r/atom 3))

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

;; --- VIEWS ---

(defn todo-input []
  (let [input-text (r/atom "")
        update-text #(reset! input-text %)
        stop #(reset! input-text "")
        save #(do
                (add-todo @input-text)
                (stop))
        key-pressed #(case %
                        "Enter" (save)
                        "Esc" (stop)
                        "Escape" (stop)
                        nil)]
  (fn []
    [:input {:class "new-todo"
            :placeholder "Create a new item"
            :type "text"
            :value @input-text
            :on-blur save
            :on-change #(update-text (.. % -target -value))
            :on-key-down #(key-pressed (.. % -key))}])))

(defn todo-item [{:keys [title]}]
  [:li
    [:div.view
      [:input {:type "checkbox"}]
      [:label title]]])

(defn todo-list []
  (let [items (vals @todos)]
    [:section.main
      [:ul.todo-list
        (for [todo items]
          ^{:key (:id todo)} [todo-item todo])]]))

(defn todo-entry []
  [:header.header
    [:h1 "todo items"]
    [todo-input]])

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
    [:section.todoapp ;;class todoapp
      [todo-entry]
      [:div
        [todo-list]
        [footer-controls]]]
      [:footer.info
        [:p "Footer info"]]]) 
      

;; --- RENDER ---

(defn render []
  (rdom/render [app] (.getElementById js/document "root")))

(defn ^:export main []
  (render))

(defn ^:dev/after-load reload! []
  (render))
