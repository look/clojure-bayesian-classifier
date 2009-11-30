(use 'clojure.test)

(use-fixtures :each (defn fixture-feature-counts [test-fn]
                      (def *feature-counts* (ref {"python" {"bad" 1, "good" 3}, "viagra" {"bad" 10}}))
                      (test-fn))

              (defn fixture-classification-counts [test-fn]
                (def *classification-counts* (ref {"good" 1, "bad" 1}))
                (test-fn)))

(deftest get-words-test
  (is (= '("foo" "bar" "baz") (get-words "foo BAR BaZ")))
  (testing "short words are omitted"
           (is (= '("the" "foo") (get-words "to the foo"))))

  (testing "long words are omitted"
           (is (= '("huh") (get-words "twentycharactersisreallylongforawordindeed huh")))))

(deftest feature-count-test
  (is (= 1 (feature-count "python" "bad")))
  (is (= 3 (feature-count "python" "good")))
  (testing "non-existent classifications return 0"
           (is (= 0 (feature-count "viagra" "good"))))

  (testing "non-existent feature returns 0 for all categories"
           (is (= 0 (feature-count "doesnotexist" "bad")))
           (is (= 0 (feature-count "doesnotexist" "good")))))

(deftest category-count-test
  (testing "non-existent categories return 0"
           (is (= 0 (category-count "asdf"))))

  (is (= 1  (category-count "good")))
  (is (= 1 (category-count "bad"))))

(deftest train-test
  (train "clojure language python" "good")
  (is (= 2 (category-count "good")))
  (is (= 0 (feature-count "clojure" "bad")))
  (is (= 1 (feature-count "clojure" "good")))
  (is (= 4 (feature-count "python" "good"))))

(deftest feature-probability-test
  (testing "probability of non-existent category is 0"
           (is (= 0 (feature-probability "python" "asdf"))))

  (testing "probability of a feature that does not exist is 0"
           (is (= 0 (feature-probability "nonexistent" "good"))))

  (testing "probability of a feature not in the category is 0"
           (is (= 0 (feature-probability "viagra" "good"))))

  (testing "probability of a feature in a category"
           (is (= 10 (feature-probability "viagra" "bad")))
           (is (= 3 (feature-probability "python" "good")))))

(deftest total-count-test
  (is (= 2 (total-count))))

(deftest weighted-probability-test
  (testing "with default arguments"
           (is (= 2.5 (weighted-probability "python" "good")))
           (is (= 0.9 (weighted-probability "python" "bad"))))

  (testing "with specific arguments"
           (is (= 2.5  (weighted-probability "python" "good" 1.0 0.5)))
           (is (= 0.9 (weighted-probability "python" "bad" 1.0 0.5)))
           (is (= (/ 0.5 11) (weighted-probability "viagra" "good" 1.0 0.5)))
           (is (= (/ 100.5 11) (weighted-probability "viagra" "bad" 1.0 0.5)))))

(deftest document-probability-test
  (testing "features already known"
           (is (= 0.11363636363636365 (document-probability "python viagra" "good"))))

  (testing "features unknown should use assumed probability"
           (is (= 0.25 (document-probability "new stuff" "good")))
           (is (= 0.25 (document-probability "new stuff" "bad")))
           (is (= 0.125 (document-probability "new stuff rules" "good")))))

(deftest category-probability-test
  (testing "category that exists"
           (is (= 0.5 (category-probability "bad")))
           (is (= 0.5 (category-probability "good"))))

  (testing "category that doesn't exist"
           (is (= 0 (category-count "stuff")))))

(deftest probability-test
  (testing "item with features already identified"
        (is (= 4.111363636363636 (probability "python viagra" "bad"))))

  (testing "item with unidentified features should be product of assumed probabilities for each feature times probability of category"
        (is (= 0.0625 (probability "foo bar baz" "bad")))
        (is (= 0.0625 (probability "foo bar baz" "good"))))

  (testing "unknown category"
        (is (= 0 (probability "foo bar baz" "stuff")))))


(deftest threshold-test
  (def *thresholds* (ref {"good" 2.0, "bad" 6.0}))

  (is (= 2.0 (threshold "good")))
  (is (= 6.0 (threshold "bad")))
  (is (= 1 (threshold "nothingset"))))

(deftest set-threshold-test
  (is (= {"good" 1.5} (do
                        (set-threshold "good" 1.5)
                        (deref *thresholds*)))))

(deftest classify-test
    ;; empty out the data structures
  (def *feature-counts* (ref {}))
  (def *classification-counts* (ref {}))

  (sample-train)

  (is (= "good" (classify "quick rabbit" "unknown"))))


(deftest sample-train-test
  ;; empty out the data structures
  (def *feature-counts* (ref {}))
  (def *classification-counts* (ref {}))

  (sample-train)

  (is (= 3 (category-count "good")))
  (is (= 2 (category-count "bad")))
  (is (= 5 (total-count)))

  (is (= 0 (feature-count "casino" "good")))
  (is (= 1 (feature-count "casino" "bad")))
  (is (= 2 (feature-count "quick" "good")))
  (is (= 1 (feature-count "quick" "bad")))
  (is (= 3 (feature-count "the" "good")))
  (is (= 1 (feature-count "the" "bad")))

  (is (= 0 (feature-count "the" "foobar")))
  (is (= 0 (feature-count "notindatabase" "good"))))


