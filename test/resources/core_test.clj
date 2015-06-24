(ns resources.core-test
  (:require [clojure.test :refer :all]
            [resources.core :refer :all]))

(def hello-world
  (resource {:handle-ok "Hello, world"
             :available-media-types ["text/plain"]}))

(def error
  (resource {:known-method? false}))

(deftest a-test
  (testing "hello world"
    (let [response (run hello-world {})]
      (is (= (:status response) 200))
      (is (= (:body response) "hello world"))))
  (testing "error"
    (let [response (run error {})]
      (is (= (:status response) 501)))))
