;; use multimethod to create a get-features method that can operate on multiple data types?
(defn get-words [string]
  (filter
   #(and (> (.length %) 2)
         (< (.length %) 20))
   (map #(.toLowerCase %)
        (re-seq #"\w+" string))))

;; map of maps: feature => {category => count}
(def *feature-counts* (ref {}))

(def *classification-counts* (ref {}))

(def *default-weight* 1.0)
(def *default-assumed-probability* 0.5)

;; map of category => threshold
(def *thresholds* (ref {}))

(defn feature-count [feature category]
  (let [feature-map (get @*feature-counts* feature {})]
    (get feature-map category 0)))

(defn inc-feature [feature category]
  (let [fcount (get @*feature-counts* feature)]
    (assoc @*feature-counts*
           feature
           (if (nil? fcount)
             {category 1}
             (assoc fcount category (inc (feature-count feature category)))))))

(defn category-count [category]
  (get @*classification-counts* category 0))

(defn inc-category [category]
  (assoc @*classification-counts*
         category
         (inc (category-count category))))

(defn total-count []
  (apply + (vals @*classification-counts*)))

(defn categories []
  (keys @*classification-counts*))

(defn train [item category]
  (dosync
   (dorun
    (map #(ref-set *feature-counts* (inc-feature % category)) (get-words item)))
   (ref-set *classification-counts* (inc-category category))))

(defn feature-probability [feature category]
  (if (== (category-count category) 0)
    0
    (/ (feature-count feature category)
       (category-count category))))

(defn weighted-probability
  ([feature category] (weighted-probability feature category *default-weight* *default-assumed-probability*))
  ([feature category weight] (weighted-probability feature category weight *default-assumed-probability*))
  ([feature category weight assumed-probability]
   (let [basic-probability (feature-probability feature category)
         total-in-all-categories (apply + (for [x (categories)] (feature-count feature x)))]

     (do
       ;; (println "basic-probability:" basic-probability)
       ;; (println "total-in-all-categories:" total-in-all-categories)
       ;; (println "weight:" weight)
       ;; (println "assumed-probability" assumed-probability)
       (/ (+ (* weight assumed-probability)
             (* total-in-all-categories basic-probability))
          (+ weight total-in-all-categories))))))
    
(defn document-probability [item category]
  (let [features (get-words item)]
    (apply * (map #(weighted-probability % category) features))))

(defn category-probability [category]
  (/ (category-count category) (total-count)))

(defn probability [item category]
  (* (document-probability item category)
     (category-probability category)))

(defn threshold [category]
  (get @*thresholds* category 1))

(defn set-threshold [category threshold]
  (dosync
   (ref-set *thresholds*
            (assoc @*thresholds* category threshold))))

;; def classify(self, item, default=None):
;;   probs = {}
;;   # Find the category with the highest probability
;;   max = 0.0
;;   for cat in self.categories():
;;     probs[cat] = self.prob(items, cat)
;;     if probs[cat] > max:
;;       max = probs[cat]
;;       best = cat
;;  # Make sure the probability exceeds threshold * next best
;;  for cat in probs:
;;    if cat == best: continue
;;    if probs[cat] * self.getthreshold(best) > probs[best]: return default
;;  return best

(defn category-probabilities
  ([item] (category-probabilities item (categories) {}))
  ([item category-list prob-map]
   (if (empty? category-list)
     prob-map
     (let [category (first category-list)]
       (assoc (category-probabilities item (rest category-list) prob-map)
              category
              (probability item category))))))

(defn best-guess-category
  ([category-prob-map] (best-guess-category category-prob-map nil 0))
  ([category-prob-map guess prob]
   (if (empty? category-prob-map)
     guess
     (let [current-guess (first (keys category-prob-map))
           current-prob (get category-prob-map current-guess)
           next-map (dissoc category-prob-map current-guess)]
       
       (if (> current-prob prob)
         (best-guess-category next-map current-guess current-prob)
         (best-guess-category next-map guess prob))))))

;; Make sure the probability exceeds threshold * next best
(defn category-probability-exceeds-threshold?
  [category-prob-map best]
  (some #(> (* (get category-prob-map %) (threshold best))
            (get category-prob-map %))
        (keys (dissoc category-prob-map best))))

(defn classify [item default-category]
  (let [category-prob-map (category-probabilities item)
        best (best-guess-category category-prob-map)]

    (if (category-probability-exceeds-threshold? category-prob-map best)
      best
      default-category)))
  
(defn sample-train []
  (let [item-map
        {"nobody owns the water" "good",
        "the quick rabbit jumps fences" "good",
        "buy pharmaceuticals now" "bad",
        "make quick money at the online casino" "bad",
        "the quick brown fox jumps" "good"}]
    (dorun
     (map train (keys item-map) (vals item-map)))))
