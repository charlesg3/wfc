(ns wfc.core
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.java.io :as io]
    [mikera.image.core :as i]
    [mikera.image.colours :as colour])
  (:gen-class))

(def input-image "flowers.png")
(def N 3)

(defn img->mean-color [img]
  (let [num-pixels (* (i/width img) (i/height img))]
  (->> img
       (i/get-pixels)
       (vec)
       (map colour/components-rgb)
       (reduce (fn [[x y z] [r g b]]
                 [(+ r x) (+ g y) (+ b z)]) [0 0 0])
       ((fn [[a b c]]
          (mapv int [(/ a num-pixels) (/ b num-pixels) (/ c num-pixels)]))))))

(defn img->hist
  [img]
  (->> (for [x (range (+ (- (i/width img) N) 1))
             y (range (+ (- (i/height img) N) 1))]
         (->> (i/sub-image img x y N N)
              (i/get-pixels)
              (map colour/components-rgb)
              (vec)))
       (frequencies)))

(defn patch->entropy [patch hist]
  (let [num-pixels (* N N)
        entropy (->> hist
                     (map (fn [[pxs wt]]
                            (reduce (fn [eax [a b]]
                                      (if (= a b)
                                        (inc eax)
                                        eax)) 0 (map (fn [x y] [x y]) pxs patch))))
                     (apply max))]
    (double (/ entropy num-pixels))
    #_(->> patch
         (reduce (fn [[x y z] [r g b]]
                   [(+ (Math/abs (- r mr)) x)
                    (+ (Math/abs (- g mg)) y)
                    (+ (Math/abs (- b mb)) z)]) [0 0 0])
         ((fn [[a b c]]
            (+ a b c))))))

(defn filter-hist
  [patch hist]
  (->> hist
       (filter (fn [[pxs wt]]
                 (every? (fn [[px patch-px]]
                           (or (= [158 211 172] patch-px)
                               (= px patch-px)))
                         (map (fn [x y] [x y]) pxs patch))))
       (into {})))

(defn patch->new-patch [patch hist]
    (let [hist (filter-hist patch hist)
          total (apply + (vals hist))
          n (rand-int total)]
      (loop [candidate (first hist)
             r (rest hist)
             so-far (second candidate)]
        (if (or (nil? so-far) (< n so-far))
          (first candidate)
          (recur (first r) (rest r) (+ so-far (second (first r))))))))

(defn -main
  [& args]
  (let [src (-> input-image
                (io/resource)
                (i/load-image))
        dest (i/new-image 64 64)
        hist (img->hist src)
        [mr mg mb] (img->mean-color src)
        entropy-map* (atom {})]
    ;; initial state
    (doseq [x (range (i/width dest))
            y (range (i/height dest))]
      (i/set-pixel dest x y (colour/rgb-from-components mr mg mb)))

    (reset! entropy-map*
            (->> (for [x (range (+ (- (i/width dest) N) 1))
                       y (range (+ (- (i/height dest) N) 1))]
                   (let [patch (->> (i/sub-image dest x y N N)
                                    (i/get-pixels)
                                    (map colour/components-rgb)
                                    (vec))]
                     [[x y] [(patch->entropy patch hist) patch]]))
                 (into {})))

    (loop [i 2000]
      ;; observe
      (let [[[xd yd] [chosen-entropy patch]] (->> @entropy-map*
                                     (vec)
                                     (shuffle)
                                     (apply max-key #(first (second %))))
            new-patch (patch->new-patch patch hist)]



        ;(if (zero? (mod i 10))
        ;  (println i))

        (println i (first patch) xd yd (count @entropy-map*) chosen-entropy)

        ;; propagate
        (doseq [x (range N)
                y (range N)]
          (let [[nr ng nb]  (get new-patch (+ x (* y N)) [mr mg mb])]
            (when (and (<= 0 (+ x xd) (- (i/width dest) N))
                     (<= 0 (+ y yd) (- (i/height dest) N)))
              (i/set-pixel dest (+ x xd) (+ y yd)
                           (colour/rgb-from-components nr ng nb)))))

        (doseq [xe (range (- xd (dec N)) (+ xd N))
                ye (range (- yd (dec N)) (+ yd N))]
            (if (and (<= 0 xe (- (i/width dest) N))
                     (<= 0 ye (- (i/height dest) N)))
              (let [patch (->> (i/sub-image dest xe ye N N)
                               (i/get-pixels)
                               (map colour/components-rgb)
                               (vec))
                    new-entropy (patch->entropy patch hist)]
                (if (= new-entropy 1.0)
                  (swap! entropy-map* dissoc [xe ye])
                  (if (get @entropy-map* [xe ye])
                    (swap! entropy-map* assoc [xe ye] [new-entropy patch]))))))

        (if (and (pos? i) (pos? (count @entropy-map*)))
          (recur (dec i)))))
    (i/save dest "out.png")
    (i/show dest)))
