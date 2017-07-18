(ns norvig-spell-corrector.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def WORDS (frequencies (re-seq #"\w+" (str/lower-case (slurp "resources/big.txt")))))

(defn known [words]
  (let [known-list (into #{} (keys WORDS))] (not-empty (filter #(contains? known-list %) words))))

(defn P [word]
  (cond (get WORDS word) (float (/ (get WORDS word) (reduce + (vals WORDS))))
        :else 0))

(defn edits1 [word]
  (flatten (map-indexed (fn [i l]
    (list 
      (str (subs word 0 i) (subs word (inc i)))  ; deletion
      (cond (< i (- (count word) 1)) (str (subs word 0 i) (nth word (inc i)) l (subs word (+ i 2)))) ; transpose
      (map (fn [c] (str (subs word 0 i) (char c) (subs word (inc i)))) (range (int \a) (inc (int \z)))) ; replace
      (map (fn [c] (str (subs word 0 i) (char c) l (subs word (inc i)) ) ) (range (int \a) (inc (int \z))))))  ; insertion
   word)))

(defn edits2 [word]
  (map #(edits1 %) (edits1 word)))

(defn candidates [word]
  (or (known (list word)) (known (edits1 word)) (known (edits2 word)) (list word)))

(defn correction [word]
  (apply max-key P (flatten (candidates word))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
