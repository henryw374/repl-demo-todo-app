(ns cs-snap-app.app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]))

;; --- APP STATE ---

(def initial-todos {1 {:id 1, :title "Do laundry", :done false}
                    3 {:id 3, :title "But groceries", :done false}
                    2 {:id 2, :title "Wash dishes", :done false}})
;; sorted map, will sort by ids
(def initial-todos-sorted (into (sorted-map) initial-todos))  
;; atom - mutable wrapper around an immutable data structure
;; this is a reagent atom, reagent atoms react to changes, keeps track of components and re renders when atom has changed
;;ratom
(defonce todos (r/atom initial-todos-sorted))

;; --- VIEWS ---

(defn todo-input []
  [:input {:class "new-todo"
           :placeholder "Create a new item"
           :type "text"}])

(defn todo-list []
  (let [items (vals @todos)]
    [:section.main
      [:ul.todo-list
        (for [todo items]
          ^{:key (:id todo)} [:li (:title todo)])]]))


(defn todo-entry []
  [:header.header
    [:h1 "todos"]
    [todo-input]])

(defn footer-controls []
  [:footer.footer
    [:div "footer controls"]])

(defn app []
  [:div
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
