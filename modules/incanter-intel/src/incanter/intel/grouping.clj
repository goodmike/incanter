;;; recommendations.clj -- Grouping algorithms for Clojure
;;; built on the Incanter Library

;; by Michael Harrison, mh@michaelharrison.ws
;; August 12, 2010

;; Copyright (c) David Edgar Liebke, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.htincanter.at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; CHANGE LOG
;; August 12, 2010: First version adds hierarchical clustering

(ns incanter.intel.grouping
  (:use [incanter.stats :only [correlation]])
  (:use [incanter.io :only [read-dataset]])
  (:use [clojure.set :only [union]])
  (:use [clojure.contrib.seq-utils :only [indexed]])
)

(comment
; Examples of delicious.com tags for popular blogs, taken from
; Toby Segaran's site
(def delicious-filepath "/Users/michael/Source/incanter/examples/intel/data/blogdata-smaller.txt")
(def blogs-dataset (read-dataset delicious-filepath :header true :delim \tab))
(def blogs-tree (hier-cluster blogs-dataset))
) ; end comment exp

;; These could go in an impl file, soon

(defn strip-map-val 
"
  Breaks a value off a map, returning value and new map.

  Given a map and a key belonging to the map, returns a 2-element vector:
  First element is the value from the map that corresponds to the key;
  Second element is the map without the key and its value.

  Returns nil for first element if key is not found in map.

  Examples:
    (strip-map-val {:one 1 :two 2 :three 3} :one)
    ; => [1 {:two 2, :three 3}]
    (strip-map-val {:one 1 :two 2 :three 3} :four)
    ; => {:one 1, :two 2, :three 3}]
"
([mp key]
   [(mp key)
    (dissoc mp key)]))

(defn get-features-map [dataset]
  (let [handle-key (first (:column-names dataset))]
    (into {} (map #(strip-map-val % handle-key) (:rows dataset)))))


(defrecord Node
    [^int index
     left
     right
     data
     ^double distance])

(defn positive-correlation [a b]
  (- 1 (correlation a b)))

(defn cache-distances [dim coll correl-fn]
     (into {} (for [x (range dim)
		    y (range dim)]
		(when (> x y)
		  [(hash-set x y) 
		   (future (correl-fn (coll x) (coll y)))]))))

(defn closer-distance [a b]
  (if (< @(val a) @(val b)) a b))

(defn merged-node [nodes-coll distances idx]
  (let [[indices distance-fut] (reduce closer-distance distances)
	[node0 node1] (filter (fn [node] (indices (:index node))) nodes-coll)]
    (Node. idx node0 node1 
	   (map #(/ (+ %1 %2) 2.0) (:data node0) (:data node1))
	   @distance-fut)))

(defn update-nodes [nodes-coll new-node]
	(conj (remove #(or (= (:left new-node) %) (= (:right new-node) %)) nodes-coll) new-node))

(defn update-distances [distances nodes-coll new-node]
	(let [idx0 (:index (:left new-node))
	      idx1 (:index (:right new-node))
	      new-idx (:index new-node)
	      indices (remove #(or (= idx0 %) (= idx1 %))
			      (reduce union (keys distances)))
	      expanded-distances (into distances 
				       (for [x indices]
					 (let [node (first (filter #(= (:index %) x) nodes-coll))]
					   [(hash-set x new-idx) 
					    (future (positive-correlation (:data node) (:data new-node)))])))]
	  
	  (into {} (remove (fn [[k v]] (or (k idx0) (k idx1))) expanded-distances))))



;; Showtime:

(defn hier-cluster [dataset]
  
  (let [feature-counts (vec (map vals (vals (get-features-map dataset))))
	leaves (map (fn [[idx counts]] (Node. idx nil nil counts 0.0)) (indexed feature-counts))
	cached-distances (cache-distances (count feature-counts)
					  feature-counts
					  positive-correlation)]
						 
    (loop [nodes      leaves
	   next-index (count nodes)
	   distances  cached-distances]
      (if (= 1 (count nodes))
	(first nodes)
	(let [new-node           (merged-node nodes distances next-index)
	      updated-nodes      (update-nodes nodes new-node)
	      updated-distances  (update-distances distances updated-nodes new-node)]
	  (recur updated-nodes
		 (inc next-index)
		 updated-distances))))))

