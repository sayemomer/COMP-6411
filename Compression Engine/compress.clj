;; Author: Omer Sayem

(ns compress
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn fileExists? [filename]
  (.exists (io/file filename)))

(defn promptForValidFile [initialFile promptMsg]
  (loop [file initialFile]
    (if (fileExists? file)
      file
      (do
        (println (str "File not found: " file))
        (println promptMsg)
        (flush)
        (recur (read-line))))))

(defn readFileContent [filename]
  (str/trim (slurp filename)))

(defn writeFileContent [filename content]
  (spit filename content))

(defn parseFrequencyWords [content]
  (str/split content #"\s+"))

(defn buildUniqueWordMap [words]
  (loop [wordList words
         idx 0
         seenWords #{}
         freqMap {}]
    (if (empty? wordList)
      freqMap
      (let [word (str/lower-case (first wordList))]
        (if (contains? seenWords word)
          (recur (rest wordList) (inc idx) seenWords freqMap)
          (recur (rest wordList) (inc idx) (conj seenWords word) (assoc freqMap word idx)))))))

(defn loadFrequencyMap [freqFile]
  (try
    (let [content (readFileContent freqFile)
          words (parseFrequencyWords content)
          freqMap (buildUniqueWordMap words)]
      freqMap)
    (catch Exception e
      (println "Error loading frequency file:" (.getMessage e))
      {})))

(defn invertFrequencyMap [freqMap]
  (into {} (map (fn [[word idx]] [idx word]) freqMap)))

(defn tokenizeText [text]
  (re-seq #"\d+|[A-Za-z]+|[^\sA-Za-z0-9]" text))

(defn isNumber? [token]
  (re-matches #"\d+" token))

(defn isWordInFrequency? [freqMap token]
  (contains? freqMap (str/lower-case token)))

(defn wrapNumber [number]
  (str "@" number "@"))

(defn getWordFrequencyIndex [freqMap word]
  (get freqMap (str/lower-case word)))

(defn compressSingleToken [freqMap token]
  (cond
    (isNumber? token) (wrapNumber token)
    (isWordInFrequency? freqMap token) (str (getWordFrequencyIndex freqMap token))
    :else token))

(defn compressTokens [freqMap tokens]
  (map #(compressSingleToken freqMap %) tokens))

(defn joinCompressedTokens [compressedTokens]
  (str/join " " compressedTokens))

(defn createOutputFilename [inputFile]
  (str inputFile ".ct"))

(defn compressFile [inputFile freqMap]
  (let [validFile (promptForValidFile inputFile "Please enter a valid input file:")
        content (readFileContent validFile)
        tokens (tokenizeText content)
        compressedTokens (compressTokens freqMap tokens)
        compressedText (joinCompressedTokens compressedTokens)
        outputFile (createOutputFilename validFile)]
    (writeFileContent outputFile compressedText)
    (println "Compressed file written to:" outputFile)))

(defn isWrappedNumber? [token]
  (re-matches #"@(\d+)@" token))

(defn unwrapNumber [token]
  (second (re-find #"@(\d+)@" token)))

(defn lookupFrequencyWord [invMap indexStr]
  (get invMap (Integer/parseInt indexStr) indexStr))

(defn decompressSingleToken [invMap token]
  (cond
    (isWrappedNumber? token) (unwrapNumber token)
    (isNumber? token) (lookupFrequencyWord invMap token)
    :else token))

(defn decompressTokens [invMap tokens]
  (map #(decompressSingleToken invMap %) tokens))

(defn joinDecompressedTokens [tokens]
  (str/join " " tokens))

(defn addSpaceAfterPunctuation [text]
  (str/replace text #"([\.\?!])" "$1 "))

(defn formatOpeningPunctuation [text]
  (str/replace text #"\s*([\(\[\{])" " $1"))

(defn formatClosingPunctuation [text]
  (str/replace text #"([\)\]\}])\s*" "$1 "))

(defn formatDashes [text]
  (str/replace text #"-" " - "))

(defn formatAtSymbols [text]
  (str/replace text #"\s*@\s*" "@"))

(defn applyBasicFormatting [text]
  (-> text
      addSpaceAfterPunctuation
      formatOpeningPunctuation
      formatClosingPunctuation
      formatDashes
      formatAtSymbols
      str/trim))

(defn splitIntoSentences [text]
  (str/split text #"(?<=[\.\?!])\s+"))

(defn capitalizeSentences [sentences]
  (map str/capitalize sentences))

(defn joinSentences [sentences]
  (str/join " " sentences))

(defn applySentenceCapitalization [text]
  (-> text
      splitIntoSentences
      capitalizeSentences
      joinSentences))

(defn formatDecompressedText [text]
  (-> text
      applyBasicFormatting
      applySentenceCapitalization))

(defn decompressFile [compressedFile freqMap]
  (let [validFile (promptForValidFile compressedFile "Please enter a valid compressed file:")
        invMap (invertFrequencyMap freqMap)
        content (readFileContent validFile)
        tokens (str/split content #"\s+")
        decompressedTokens (decompressTokens invMap tokens)
        rawText (joinDecompressedTokens decompressedTokens)
        formattedText (formatDecompressedText rawText)]
    (println "Decompressed text:")
    (println formattedText)))

