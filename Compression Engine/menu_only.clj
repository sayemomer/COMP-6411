(ns a2
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:require [compress :as compress]))
  ; this is where you would also include/require the compress module

(def freq-map (compress/load-frequency-map "frequency.txt"))

; Display the menu and ask the user for the option
(defn showMenu
  []
  (println "\n\n*** Compression Menu ***")
  (println "------------------\n")
  (println "*****")
  (println "1. Display list of files")
  (println "2. Display file contents")
  (println "3. Compress a file")
  (println "4. Uncompress a file")
  (println "5. Exit")
  (do 
    (print "\nEnter an option? ") 
    (flush) 
    (read-line)))


; Display all files in the current folder
(defn option1
  []
  ((defn list-files []
  (let [dir (io/file ".")]
    (doseq [f (file-seq dir)]
      (when (.isFile f)
        (println (.getName f))))))))


; Read and display the file contents (if the file exists). Java's File class can be used to 
; check for existence first. 
(defn option2
  []
  (print "\nPlease enter a file name => ") 
  (flush)
  (let [filename (read-line)
        file (io/file filename)]
    (if (.exists file)
      (println (slurp file))
      (println "Invalid file!"))))



; Compress the (valid) file provided by the user. You will replace the println expression with code 
; that calls your compression function
(defn option3
  []
  (print "\nPlease enter a file name => ")
  (flush)
  (let [file_name (read-line)]
    (compress/compress-file file_name freq-map)))


; Decompress the (valid) file provided by the user. You will replace the println expression with code 
; that calls your decompression function
(defn option4
  []
  (print "\nPlease enter a file name => ")
  (flush)
  (let [file_name (read-line)]
    (compress/decompress-file file_name freq-map)))


; If the menu selection is valid, call the relevant function to 
; process the selection
(defn processOption
  [option] ; other parm(s) can be provided here, if needed
  (if( = option "1")
     (option1)
     (if( = option "2")
        (option2)
        (if( = option "3")
           (option3)  ; other args(s) can be passed here, if needed
           (if( = option "4")
              (option4)   ; other args(s) can be passed here, if needed
              (println "Invalid Option, please try again"))))))


; Display the menu and get a menu item selection. Process the
; selection and then loop again to get the next menu selection
(defn menu
  [] ; parm(s) can be provided here, if needed
  (let [option (str/trim (showMenu))]
    (if (= option "5")
      (println "\nGood Bye\n")
      (do 
         (processOption option)
         (recur )))))   ; other args(s) can be passed here, if needed




; ------------------------------
; Run the program. You might want to prepare the data required for the mapping operations
; before you display the menu. You don't have to do this but it might make some things easier

(menu) ; other args(s) can be passed here, if needed
