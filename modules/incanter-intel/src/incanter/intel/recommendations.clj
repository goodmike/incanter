(ns incanter.intel.recommendations
  (:use [clojure.set :only [union intersection difference]])
  (:use [incanter.stats :only [correlation]]))

(def critics-ratings
     {"Lisa"    {"Lady" 2.5, "Snakes" 3.5, "Luck" 3.0, "Superman" 3.5, "Dupree" 2.5, "Listener" 3.0},
      "Gene"    {"Lady" 3.0, "Snakes" 3.5, "Luck" 1.5, "Superman" 5.0, "Dupree" 3.5, "Listener" 3.0},
      "Michael" {"Lady" 2.5, "Snakes" 3.0, "Superman" 3.5, "Listener" 4.0},
      "Claudia" {"Snakes" 3.5, "Luck" 3.0, "Listener" 4.5, "Superman" 4.0, "Dupree" 2.5},
      "Mick"    {"Lady" 3.0, "Snakes" 4.0, "Luck" 2.0, "Superman" 3.0, "Listener" 3.0, "Dupree" 2.0},
      "Jack"    {"Lady" 3.0, "Snakes" 4.0, "Listener" 3.0, "Superman" 5.0, "Dupree" 3.5},
      "Toby"    {"Snakes" 4.5, "Superman" 4.0, "Dupree" 1.0}})

(defn ratings-pearson [ratings-map critic1 critic2]
  (let [items (intersection (-> critic1 ratings-map keys set) (-> critic2 ratings-map keys set))]
    (if (zero? (count items)) 
      0
      (correlation (map (ratings-map critic1) items)
		   (map (ratings-map critic2) items)))))
  
(defn items-ratings [ratings-map critics items]
	(reduce (fn [ratings item] 
		  (reduce (fn [ratings critic] 
			    (if ((ratings-map critic) item)
			      (assoc ratings item (assoc (ratings item) critic ((critics-ratings critic) item)))
			      ratings))
			  ratings
			  critics))
		(zipmap items (repeat {}))
		items))

(defn recommendations [ratings-map critic_id]
  (let [others       (filter #(not= critic_id %) (keys ratings-map))
	    similarities (zipmap others (map #(ratings-pearson ratings-map critic_id %) others))
	    all-items    (apply union (map #(-> % ratings-map keys set) others))
	    items        (difference all-items (set (keys (ratings-map critic_id)))) 
	    ratings-by-item (items-ratings ratings-map others items)]
    (zipmap (map first ratings-by-item)
	    (map (fn [[item reviews]] 
		   (/ (apply + (map (fn [[critic rating]] (* (similarities critic) rating)) reviews))
		      (apply + (map (fn [[critic rating]] (similarities critic)) reviews))))
		 ratings-by-item))))