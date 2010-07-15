;;; recommendations.clj -- Collaborative filtering library for Clojure 
;;; built on the Incanter Library

;; by Michael Harrison, mh@michaelharrison.ws
;; July 15, 2010

;; Copyright (c) David Edgar Liebke, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.htincanter.at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; CHANGE LOG
;; Jul 15, 2010: Add documention
;; Jun 30, 2010: First version produces recommendations for a member of
;; a map of critics to item-rating maps, using Pearson correlation.

(ns incanter.intel.recommendations
  (:use [clojure.set :only [union intersection difference]])
  (:use [incanter.stats :only [correlation]]))

; Example data that can be used to drive the functions below, taken from
; Programming Collective Intelligence, O'Reilly
(def critics-ratings
     {"Lisa"    {"Lady" 2.5, "Snakes" 3.5, "Luck" 3.0, "Superman" 3.5, "Dupree" 2.5, "Listener" 3.0},
      "Gene"    {"Lady" 3.0, "Snakes" 3.5, "Luck" 1.5, "Superman" 5.0, "Dupree" 3.5, "Listener" 3.0},
      "Michael" {"Lady" 2.5, "Snakes" 3.0, "Superman" 3.5, "Listener" 4.0},
      "Claudia" {"Snakes" 3.5, "Luck" 3.0, "Listener" 4.5, "Superman" 4.0, "Dupree" 2.5},
      "Mick"    {"Lady" 3.0, "Snakes" 4.0, "Luck" 2.0, "Superman" 3.0, "Listener" 3.0, "Dupree" 2.0},
      "Jack"    {"Lady" 3.0, "Snakes" 4.0, "Listener" 3.0, "Superman" 5.0, "Dupree" 3.5},
      "Toby"    {"Snakes" 4.5, "Superman" 4.0, "Dupree" 1.0}})

(defn ratings-pearson
"
  Returns the Pearson correlation between two critics' numerical ratings
  of the same set of items. This is a simple way to determine the 
  similarity of the two critics' tastes.

  Examples:
    (ratings-pearson 
      {\"Lisa\"    {\"Lady\" 2.5, \"Snakes\" 3.5, \"Superman\" 4.0},
       \"Gene\"    {\"Lady\" 3.0, \"Snakes\" 4.0, \"Luck\" 4.5},
       \"Toby\"    {\"Snakes\" 4.5, \"Superman\" 4.0, \"Dupree\" 1.0}}
      \"Lisa\"
      \"Gene\")
    ; => 0.99998
"
([ratings-map critic1 critic2]
  (let [items (intersection (-> critic1 ratings-map keys set) (-> critic2 ratings-map keys set))]
    (if (zero? (count items)) 
      0
      (correlation (map (ratings-map critic1) items)
		   (map (ratings-map critic2) items))))))


  
(defn items-ratings 
"
  Produces a mapping of items to critic-rating maps from a mapping of 
  critics to item-rating maps and the collections and critics and items
  to take from the input mapping.

  Examples:
    (items-ratings
      {\"Lisa\"    {\"Lady\" 2.5, \"Snakes\" 3.5, \"Superman\" 4.0},
       \"Gene\"    {\"Lady\" 3.0, \"Snakes\" 4.0, \"Luck\" 4.5},
       \"Toby\"    {\"Snakes\" 4.5, \"Superman\" 4.0, \"Dupree\" 1.0}}
      '(\"Lisa\" \"Gene\")
      '(\"Lady\" \"Snakes\"))
    ; => {\"Snakes\" {\"Gene\" 4.0, \"Lisa\" 3.5}, \"Lady\" {\"Gene\" 3.0, \"Lisa\" 2.5}}
"
([ratings-map critics items]
	(reduce (fn [ratings item] 
		  (reduce (fn [ratings critic] 
			    (if ((ratings-map critic) item)
			      (assoc ratings item (assoc (ratings item) critic ((critics-ratings critic) item)))
			      ratings))
			  ratings
			  critics))
		(zipmap items (repeat {}))
		items)))

(defn recommendations 
"
  Given a map of critics to item-rating maps and a specific critic from 
  that map, produces the likely ratings the specific critic would assign
  to items the critic has not yet rated.

  Example:
  (recommendations
    {\"Lisa\"    {\"Lady\" 2.5, \"Snakes\" 3.5, \"Luck\" 3.0, \"Superman\" 4.0},
     \"Gene\"    {\"Lady\" 3.0, \"Snakes\" 4.0, \"Luck\" 4.5, \"Dupree\" 3.5},
     \"Mick\"    {\"Lady\" 2.5, \"Luck\" 3.0, \"Superman\" 3.5, \"Dupree\" 2.0},
     \"Toby\"    {\"Snakes\" 4.5, \"Superman\" 4.0, \"Dupree\" 1.0}}
    \"Toby\")
    ; => {\"Lady\" 3.5, \"Luck\" 0.5}
"
([ratings-map critic_id]
  (let [others       (filter #(not= critic_id %) (keys ratings-map))
	similarities (zipmap others (map #(ratings-pearson ratings-map critic_id %) others))
	all-items    (apply union (map #(-> % ratings-map keys set) others))
	items        (difference all-items (set (keys (ratings-map critic_id)))) 
	ratings-by-item (items-ratings ratings-map others items)]
    (zipmap (map first ratings-by-item)
	    (map (fn [[item reviews]] 
		   (/ (apply + (map (fn [[critic rating]] (* (similarities critic) rating)) reviews))
		      (apply + (map (fn [[critic rating]] (similarities critic)) reviews))))
		 ratings-by-item)))))