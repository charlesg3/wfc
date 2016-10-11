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

(defn entropy [patch [mr mg mb]]
  (let [num-pixels (* N N)]
    (->> patch
         (reduce (fn [[x y z] [r g b]]
                   [(+ (Math/abs (- r mr)) x)
                    (+ (Math/abs (- g mg)) y)
                    (+ (Math/abs (- b mb)) z)]) [0 0 0])
         ((fn [[a b c]]
            (+ a b c))))))

(defn generate-patch [patch hist]
  (->> hist
       (into [])
       shuffle
       ffirst))

(defn -main
  [& args]
  (let [src (-> input-image
                (io/resource)
                (i/load-image))
        dest (i/new-image 256 128)
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
                     [[x y] [(entropy patch [mr mg mb]) patch]]))
                 (into {})))

    (loop [i 1000]
      ;; observe
      (let [[[xd yd] [_ patch]] (->> @entropy-map*
                                     (vec)
                                     (shuffle)
                                     (sort-by (comp first second) <)
                                     (first))
            new-patch (generate-patch patch hist)]

        ;(if (zero? (mod i 10))
        ;  (println i))

        (println i)

        ;; propagate
        (doseq [x (range N)
                y (range N)]
          (let [[nr ng nb]  (get new-patch (+ x (* y N)))]
            (i/set-pixel dest (+ x xd) (+ y yd)
                         (colour/rgb-from-components nr ng nb))))

        (swap! entropy-map* assoc [xd yd] [(entropy new-patch [mr mg mb]) new-patch])

        (if (pos? i)
          (recur (dec i)))))
    (i/save dest "out.png")
    (i/show dest)))
