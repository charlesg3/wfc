(ns wfc.core
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.java.io :as io]
    [clojure.set :as set]
    [mikera.image.core :as i]
    [mikera.image.colours :as c])
  (:gen-class))

(def input-image "flowers.png")
(def N 3)

(defn img->all-rgb-pixels
  [img]
  (->> img
       (i/get-pixels)
       (vec)
       (map c/components-rgb)))

(defn img->color-index
  [img]
  (->> img
       (img->all-rgb-pixels)
       (distinct)
       (map-indexed vector)
       (into {})
       (set/map-invert)))

(defn img->indexed
  [img color-index]
  (let [w (i/width img)
        h (i/height img)]
    (->> (for [x (range -1 (inc w))
               y (range -1 (inc h))]
           [[x y] (if (and (<= 0 x (dec w))
                           (<= 0 y (dec h)))
                    (get color-index (c/components-rgb (i/get-pixel img x y)))
                    -1)])
         (into {}))))

(defn get-indexed-patch
  [indexed x y]
  (->> (for [yo [-1 0 1]
             xo [-1 0 1]]
         (get indexed [(+ x xo) (+ y yo)]))
       (vec)))

(defn indexed->w
  [indexed]
  (apply max (map (comp first first) indexed)))

(defn indexed->h
  [indexed]
  (apply max (map (comp second first) indexed)))

(defn indexed->indexed-hist
  [indexed]
  (let [w (indexed->w indexed)
        h (indexed->h indexed)]
    (->> (for [x (range w)
               y (range h)]
         (get-indexed-patch indexed x y))
       (frequencies))))

(defn filter-indexed-hist
  [patch indexed-hist]
  (->> indexed-hist
       (filter (fn [[pxs wt]]
                 (every? (fn [[px patch-px]]
                           (or (= -2 patch-px)
                               (= px patch-px)))
                         (map (fn [x y] [x y]) pxs patch))))
       (into {})))

(defn fresh-indexed-dest
  [w h]
  (->> (for [x (range -1 (inc w))
             y (range -1 (inc h))]
         [[x y] (if (and (<= 0 x (dec w))
                         (<= 0 y (dec h)))
                  -2
                  -1)])
       (into {})))

(defn indexed-edge-patch?
  [patch]
  (some #{-1} patch))

(defn indexed-patch->entropy
  [patch indexed-hist]
  (apply + (map second (filter-indexed-hist patch indexed-hist))))

(defn produce-entropy-map
  [dest indexed-hist]
  (->> (for [x (range (indexed->w dest))
             y (range (indexed->h dest))]
         [[x y] (-> (get-indexed-patch dest x y)
                    (indexed-patch->entropy indexed-hist))])
       (into {})))

(defn indexed-patch->new-patch
  [patch indexed-hist]
  (let [hist (filter-indexed-hist patch indexed-hist)
        total (apply + (vals hist))
        n (rand-int total)]
    (loop [candidate (first hist)
           r (rest hist)
           so-far (second candidate)]
      (if (or (nil? so-far) (< n so-far))
        (first candidate)
        (recur (first r) (rest r) (+ so-far (second (first r))))))))

(defn observe-propigate
  [entropy-map dest indexed-hist dest-w dest-h]
  (let [[x y] (->> entropy-map
                   seq
                   shuffle
                   (sort-by second)
                   ffirst)
        least-entropy-patch (get-indexed-patch dest x y)
        new-patch (indexed-patch->new-patch least-entropy-patch indexed-hist)
        new-dest
        (reduce (fn [eax [xo yo]]
                  (let [idx (+ (inc xo) (* 3 (inc yo)))]
                    (assoc eax [(+ x xo) (+ y yo)] (get new-patch idx))))
                dest
                (for [xo [-1 0 1] yo [-1 0 1]] [xo yo]))
        new-entropy-map
        (reduce (fn [eax [xo yo]]
                  (cond
                    (= 0 xo yo) (dissoc eax [(+ x xo) (+ y yo)])
                    (and (<= 0 (+ x xo) (dec dest-w))
                         (<= 0 (+ y yo) (dec dest-h)))
                    (assoc eax [(+ x xo) (+ y yo)] (indexed-patch->entropy
                                                    (get-indexed-patch new-dest (+ x xo) (+ y yo))
                                                    indexed-hist))
                    :else eax))
                entropy-map
                (for [xo [-1 0 1] yo [-1 0 1]] [xo yo]))]
    [new-dest new-entropy-map]))

(defn wfc
  [src-path dest-w dest-h]
  (let [img (i/load-image-resource src-path)
        color-index (img->color-index img)
        reverse-color-index (set/map-invert color-index)
        indexed (img->indexed img color-index)
        indexed-hist (indexed->indexed-hist indexed)]
    (loop [dest (fresh-indexed-dest dest-w dest-h)
           entropy-map (produce-entropy-map dest indexed-hist)]
      (if (empty? entropy-map)
        dest
        (let [[new-dest new-entropy-map]
              (observe-propigate entropy-map dest indexed-hist dest-w dest-h)]
          (recur new-dest new-entropy-map))))))

(defn -main
  [& args]
  (wfc input-image 64 64))
