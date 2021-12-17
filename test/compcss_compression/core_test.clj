(ns compcss-compression.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [compcss-compression.core]
   [clj-ph-css.core]
   [matcho.core]))

(defmacro match-compression
  [before & after]
  `(matcho.core/match
    (->> (clj-ph-css.core/string->schema ~before)
         (compcss-compression.core/compression)
         (clj-ph-css.core/schema->string))
    (apply str ~@after)))

(deftest compression
  (testing "duplicate declarations"
    (match-compression
     "E{a:1;b:2;a:3}"
     "E{a:3;b:2}"))
  (testing "duplicate selectors"
    (match-compression
      "A,A,B{a:a}
       A A,A A{a:a}"
      "A,B{a:a}"
      "A A{a:a}"))
  (testing "duplicate style"
    (match-compression
      "E{a:1}
       E{a:2}"
      "E{a:2}"))
  (testing "duplicate media"
    (match-compression
      "@media all {A{}}
       @media all {B{}}
       A{}"
      "@media all{A{}B{}}"
      "A{}"))
  (testing "duplicate keyframes"
    (match-compression
      "@keyframes A {from {color: red}}
       @keyframes A {from {right: 100}}
       @keyframes A {to   {right: 100}}"
      "@keyframes A{from{color:red;right:100}to{right:100}}")))
