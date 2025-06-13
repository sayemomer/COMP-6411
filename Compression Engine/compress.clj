(ns compress
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Load and prepare the frequency map from file
(defn load-frequency-map
  "Loads frequency.txt and returns a map of word -> index (frequency rank)."
  [freq-file]
  (try
    (let [content        (str/trim (slurp freq-file))
          words          (str/split content #"\s+")
          words-with-idx (map-indexed (fn [idx w] [(str/lower-case w) idx]) words)
          freq-map       (into {} words-with-idx)]
      (println "Loaded" (count freq-map) "entries into frequency map.")
      freq-map)
    (catch Exception e
      (println "Error loading frequency file:" (.getMessage e))
      {})))

;; Tokenize text into words, numbers, or punctuation symbols
(defn tokenize
  [text]
  (re-seq #"\d+|[A-Za-z]+|[^\sA-Za-z0-9]" text))

;; Compress a single token:
;; - Numbers wrapped as @n@
;; - Frequent words replaced by their index
;; - Others unchanged
(defn compress-token
  [freq-map token]
  (cond
    (re-matches #"\d+" token)
    (str "@" token "@")

    (contains? freq-map (str/lower-case token))
    (str (get freq-map (str/lower-case token)))

    :else token))

(defn compress-file
  "Reads input-file, prompts until it exists, compresses contents, and writes to input-file.ct"
  [input-file freq-map]
  (let [valid-file
        (loop [file input-file]
          (if (.exists (io/file file))
            file
            (do
              (println (str "File not found: " file))
              (println "Please enter a valid input file:")
              (flush)
              (recur (read-line)))))]
    (let [original    (slurp valid-file)
          tokens      (tokenize original)
          compressed  (map #(compress-token freq-map %) tokens)
          out-text    (str/join " " compressed)
          output-file (str valid-file ".ct")]  
      (spit output-file out-text)
      (println "Compressed file written to:" output-file))))

;; Invert the frequency map for decompression
(defn invert-map
  [m]
  (into {} (map (fn [[w idx]] [idx w]) m)))

;; Decompress a single token:
;; - @n@ → "n"
;; - digits → lookup frequent word
;; - others unchanged
(defn decompress-token
  [inv-map token]
  (cond
    (re-matches #"@(\d+)@" token)
    (second (re-find #"@(\d+)@" token))

    (re-matches #"\d+" token)
    (get inv-map (Integer/parseInt token) token)

    :else token))

;; Apply formatting rules to the plain text
(defn apply-formatting
  [text]
  (-> text
      ;; Ensure space after sentence-ending punctuation
      (str/replace #"([\.\?!])" "$1 ")
      ;; Space before opening punctuation
      (str/replace #"\s*([\(\[\{])" " $1")
      ;; Space after closing punctuation
      (str/replace #"([\)\]\}])\s*" "$1 ")
      ;; Surround dashes with spaces
      (str/replace #"-" " - ")
      ;; Remove space around @ markers
      (str/replace #"\s*@\s*" "@")
      str/trim))

(defn decompress-file
  "Reads compressed-file, prompts until it exists, decompresses contents, applies formatting, and prints."  
  [compressed-file freq-map]
  (let [valid-file
        (loop [file compressed-file]
          (if (.exists (io/file file))
            file
            (do
              (println (str "File not found: " file))
              (println "Please enter a valid compressed file:")
              (flush)
              (recur (read-line)))))]
    (let [inv-map    (invert-map freq-map)
          content    (slurp valid-file)
          tokens     (str/split content #"\s+")
          raw-tokens (map #(decompress-token inv-map %) tokens)
          raw-text   (str/join " " raw-tokens)
          formatted  (apply-formatting raw-text)
          sentences  (str/split formatted #"(?<=[\.\?!])\s+")
          cap-sents  (map str/capitalize sentences)
          result     (str/join " " cap-sents)]
      (println "Decompressed text:")
      (println result))))
