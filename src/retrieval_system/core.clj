(ns retrieval-system.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [edu.stanford.nlp.pipeline StanfordCoreNLP Annotation XMLOutputter]
           [edu.stanford.nlp.ling CoreAnnotations
            CoreAnnotations$TokensAnnotation
            CoreAnnotations$SentencesAnnotation
            CoreAnnotations$LemmaAnnotation
            CoreAnnotations$TextAnnotation
            CoreAnnotations$PartOfSpeechAnnotation
            CoreAnnotations$NamedEntityTagAnnotation]
           [java.util Properties]))

(def ^:private props
  (doto (Properties.)
    (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse")))

(def ^:private pipeline
  (StanfordCoreNLP. props))

(defn annotate-document
  [document]
  (.process pipeline document))

(defn sentences
  [annotated-document]
  (.get annotated-document CoreAnnotations$SentencesAnnotation))

(defn tokens
  [annotated-document]
  (.get annotated-document CoreAnnotations$TokensAnnotation))

(defn word-info
  [annotated-document & annotations]
  (map (fn [word] (map (fn [annotation] (.get word annotation))
                      annotations))
       (tokens annotated-document)))

(def words CoreAnnotations$TextAnnotation)
(def lemmas CoreAnnotations$LemmaAnnotation)
(def pos-tags CoreAnnotations$PartOfSpeechAnnotation)
(def ner-tags CoreAnnotations$NamedEntityTagAnnotation)

(defn get-lemmas
  [annotated-document]
  (apply interleave (word-info annotated-document lemmas)))

(defn get-pos-tags
  [annotated-document]
  (apply interleave (word-info annotated-document pos-tags)))

(defn annotate
  "Given a document represented by a string, annotate the document and
  return as a list of lists with the provided information tags:
    (annotate document lemmas ner-tags)"
  [document & annotation-functions]
  (println annotation-functions)
  (let [annotated-sentences (sentences (annotate-document document))]
    (map (fn [sentence] (apply word-info sentence annotation-functions))
         annotated-sentences)))

(defn annotation-string
  [annotated-document]
  (string/join "\n" (map (fn [sentence]
                           (string/join " " (map (fn [token] (string/join "/" token))
                                            sentence)))
        annotated-document)))

(defn xml-print!
  "Print the annotated text to the given xml file."
  [annotation filename]
  (let [out (io/output-stream filename)]
    (XMLOutputter/xmlPrint annotation out pipeline)))

(defn parse-dir
  [directory]
  (for [file (file-seq (io/file directory)) :when (.endsWith (.getName file) ".txt")]
    (xml-print! (annotate-document (slurp file)) (str file ".xml"))))
