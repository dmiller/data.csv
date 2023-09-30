;; Copyright (c) Jonas Enlund. All rights reserved.  The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns ^{:author "Jonas Enlund"
      :doc "Reading and writing comma separated values."}
  clojure.data.csv
  (:require (clojure [string :as str]))
  (:import #?(:clj (java.io PushbackReader Reader Writer StringReader EOFException)
              :cljr (System.IO TextReader TextWriter StringReader IOException))
		   #?(:cljr (clojure.lang PushbackTextReader))))

(set! *warn-on-reflection* true)

;; Reading

(def ^{:private true} lf  (int \newline))
(def ^{:private true} cr  (int \return))
(def ^{:private true} eof -1)

(defn- read-quoted-cell [^#?(:clj PushbackReader :cljr PushbackTextReader) reader ^StringBuilder sb sep quote]
  (loop [ch (#?(:clj .read :cljr .Read) reader)]
    (condp == ch
      quote (let [next-ch (#?(:clj .read :cljr .Read) reader)]
              (condp == next-ch
                quote (do (#?(:clj .append :cljr .Append) sb (char quote))
                          (recur (#?(:clj .read :cljr .Read) reader)))
                sep :sep
                lf  :eol
                cr  (let [next-next-ch (#?(:clj .read :cljr .Read) reader)]
                      (when (not= next-next-ch lf)
                        (#?(:clj .unread :cljr .Unread) reader next-next-ch))
                      :eol)
                eof :eof
                (throw (Exception. ^String (format "CSV error (unexpected character: %c)" next-ch)))))
      eof (throw (#?(:clj EOFException. :cljr IOException.) "CSV error (unexpected end of file)"))
      (do (#?(:clj .append :cljr .Append) sb (char ch))
          (recur (#?(:clj .read :cljr .Read) reader))))))

(defn- read-cell [^#?(:clj PushbackReader :cljr PushbackTextReader) reader ^StringBuilder sb sep quote]
  (let [first-ch (#?(:clj .read :cljr .Read) reader)]
    (if (== first-ch quote)
      (read-quoted-cell reader sb sep quote)
      (loop [ch first-ch]
        (condp == ch
          sep :sep
          lf  :eol
          cr (let [next-ch (#?(:clj .read :cljr .Read) reader)]
               (when (not= next-ch lf)
                 (#?(:clj .unread :cljr .Unread) reader next-ch))
               :eol)
          eof :eof
          (do (#?(:clj .append :cljr .Append) sb (char ch))
              (recur (#?(:clj .read :cljr .Read) reader))))))))

(defn- read-record [reader sep quote]
  (loop [record (transient [])]
    (let [cell (StringBuilder.)
          sentinel (read-cell reader cell sep quote)]
      (if (= sentinel :sep)
        (recur (conj! record (str cell)))
        [(persistent! (conj! record (str cell))) sentinel]))))

(defprotocol Read-CSV-From
  (read-csv-from [input sep quote]))

(extend-protocol Read-CSV-From
  String
  (read-csv-from [s sep quote]
    (read-csv-from (#?(:clj PushbackReader. :cljr PushbackTextReader.) (StringReader. s)) sep quote))
  
  #?(:clj Reader :cljr TextReader)
  (read-csv-from [reader sep quote]
    (read-csv-from (#?(:clj PushbackReader. :cljr PushbackTextReader.) reader) sep quote))
  
  #?(:clj PushbackReader :cljr PushbackTextReader)
  (read-csv-from [reader sep quote] 
    (lazy-seq
     (let [[record sentinel] (read-record reader sep quote)]
       (case sentinel
	 :eol (cons record (read-csv-from reader sep quote))
	 :eof (when-not (= record [""])
		(cons record nil)))))))

(defn read-csv
  "Reads CSV-data from input (String or java.io.Reader) into a lazy
  sequence of vectors.

   Valid options are
     :separator (default \\,)
     :quote (default \\\")"
  [input & options]
  (let [{:keys [separator quote] :or {separator \, quote \"}} options]
    (read-csv-from input (int separator) (int quote))))


;; Writing

(defn- write-cell [^#?(:clj Writer :cljr TextWriter) writer obj sep quote quote?]
  (let [string (str obj)
	must-quote (quote? string)]
    (when must-quote (#?(:clj .write :cljr .Write) writer (int quote)))
    (#?(:clj .write :cljr .Write)  writer (if must-quote
		     (str/escape string
				 {quote (str quote quote)})
		     string))
    (when must-quote (#?(:clj .write :cljr .Write)  writer (int quote)))))

(defn- write-record [^#?(:clj Writer :cljr TextWriter) writer record sep quote quote?]
  (loop [record record]
    (when-first [cell record]
      (write-cell writer cell sep quote quote?)
      (when-let [more (next record)]
	(#?(:clj .write :cljr .Write)  writer #?(:clj (int sep) :cljr sep))
	(recur more)))))

(defn- write-csv*
  [^#?(:clj Writer :cljr TextWriter) writer records sep quote quote? ^String newline]
  (loop [records records]
    (when-first [record records]
      (write-record writer record sep quote quote?)
      (#?(:clj .write :cljr .Write) writer newline)
      (recur (next records)))))

(defn write-csv
  "Writes data to writer in CSV-format.

   Valid options are
     :separator (Default \\,)
     :quote (Default \\\")
     :quote? (A predicate function which determines if a string should be quoted. Defaults to quoting only when necessary.)
     :newline (:lf (default) or :cr+lf)"
  [writer data & options]
  (let [opts (apply hash-map options)
        separator (or (:separator opts) \,)
        quote (or (:quote opts) \")
        quote? (or (:quote? opts) (let [should-quote #{separator quote \return \newline}] #(some should-quote %)))
        newline (or (:newline opts) :lf)]
    (write-csv* writer
		data
		separator
		quote
                quote?
		({:lf "\n" :cr+lf "\r\n"} newline))))
