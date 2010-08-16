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
;; August 16, 2010: Added documentation strings

(ns incanter.intel.grouping
  (:use [incanter.stats :only [correlation]])
  (:use [incanter.io :only [read-dataset]])
  (:use [clojure.set :only [union]])
  (:use [clojure.contrib.seq-utils :only [indexed]])
)

(comment

  ; Examples of word counts for popular blogs, taken from
  ; Toby Segaran's site
  (def fullsize-file-path "/Users/michael/Source/incanter/examples/intel/data/blogdata.txt")
  (def small-file-path "/Users/michael/Source/incanter/examples/intel/data/blogdata-smaller.txt")
  (use '[incanter.io :only [read-dataset]])
  (def blogs-dataset (read-dataset small-file-path :header true :delim \tab))
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

(defn get-features-map 
"
  Transforms a standard Incanter dataset into a map of record names
  to records, using `strip-map-val`.

  Assumes first column label is to be used for records' names.

  Examples:
    my-dataset
    ; => [:Name :length :width]
         [\"Tom\" 80 20]
         [\"Jerry\" 8 2]
    (get-features-map my-dataset)
    ; => {\"Tom\" {:length 80, :width 20},
          \"Jerry\" {:length 8, :width 2}}
"
([dataset]
   (let [handle-key (first (:column-names dataset))]
     (into {} (map #(strip-map-val % handle-key) (:rows dataset))))))

; Represents the nodes of the tree that cluster items
(defrecord Node
    [^int index
     left
     right
     data
     ^double distance])

(defn positive-correlation 
"
  Uses Pearson correlation on vectors supplied and subtracts that from 1.
 
  Always returns a non-negative number. Lower numbers mean closer correlation.
"
([a b]
   (- 1 (correlation a b))))

(defn cache-distances 
"
  Given a collection of vectors, builds a distance-map between every pairable
  set of vectors. 

  Uses index numbers of vectors within collection to build a 2-item hash-set
  to use as a key in distance-map: hence, caching distance for set #{a b}
  means distance for #{b a} may also be looked up in the cache.
"
([dim coll correl-fn]
   (into {} (for [x (range dim)
		  y (range dim)]
	      (when (> x y)
		[(hash-set x y) 
		 (future (correl-fn (coll x) (coll y)))])))))

(defn closer-distance 
"
  Comparing the two records from the distance cache, returns the record with 
  the smaller distance for a value.

  Expects futures as values, hence the `deref` of the distance record's `val`.  
"
([a b]
   (if (< @(val a) @(val b)) a b)))


(defn merged-node 
"
  Creates a merged node from two Node objects. Put another way, it creates
  a branch node joining the two nodes.

  Assigns a `data` member by averaging the data vectors of the child nodes.
  Assigns a `distance` member by looking up the distance between the child 
  nodes in the provided `distances` map argument.
  Assigns an `index` member from the provided `idx` integer argument.
"
([nodes-coll distances idx]
   (let [[indices distance-fut] (reduce closer-distance distances)
	 [node0 node1] (filter (fn [node] (indices (:index node))) nodes-coll)]
     (Node. idx node0 node1 
	    (map #(/ (+ %1 %2) 2.0) (:data node0) (:data node1))
	    @distance-fut))))

(defn update-nodes 
"
  Given a collection of nodes and a new node, returns a new collection based on
  the collection provided: the new node's child nodes are removed and the new 
  node is conjoined.
"
([nodes-coll new-node]
   (conj (remove #(or (= (:left new-node) %) (= (:right new-node) %)) 
		 nodes-coll) 
	 new-node))

(defn update-distances 
"
  Given a map of distances, a collection of nodes, and a new node, returns a 
  copy of the map of distances that takes the new node into account.

  Distances between the new node and all other nodes, except the new node's 
  child nodes, are added to the distance map. Then all distances with either 
  of the child nodes in its set key are removed.
"
([distances nodes-coll new-node]
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

(defn hier-cluster 
"
  Given a dataset, produced a tree that clusters records in the dataset
  according to their data's similarity.
"
([dataset]
  
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
		  updated-distances)))))))

