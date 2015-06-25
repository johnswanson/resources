(ns resources.core-test
  (:require [clojure.test :refer :all]
            [resources.core :refer :all]
            [plumbing.core :refer [fnk]]))

(defn request [m]
  (merge {:remote-addr "127.0.0.1"
          :params {}
          :route-params {}
          :headers {"accept" "*/*" "content-type" "application/edn"}
          :host "127.0.0.1"
          :user-agent "curl/7.42.1"
          :server-port 8080
          :character-encoding "utf8"
          :uri "/api/some/path"
          :server-name "localhost"
          :query-string nil
          :body "{:this :is :a :test}"
          :scheme :http
          :request-method :get}
         m))

(def users {"bob" {:password "password"}
            "jim" {:password "lame"}})

(def auth-resource
  (resource {:params (fnk [request] (:params request))
             :email (fnk [params] (:email params))
             :password (fnk [params] (:password params))
             :user (fnk [email password]
                     (when-let [user (users email)]
                       (when (= password (:password user))
                         user)))
             :allowed? (fnk [user] user)
             :available-media-types (fnk [] ["text/plain" "application/edn"])
             :handle-ok (fnk [] {:hello :world})}))

(deftest authorization
  (testing "successful auth"
    (let [response (run auth-resource
                     (request {:params {:email "bob" :password "password"}
                               :headers {"accept" "application/edn"}}))]
      (is (= (:status response) 200))
      (is (= (:body response) (pr-str {:hello :world})))))
  (testing "unsuccessful auth"
    (let [response (run auth-resource
                     (request {:params {:email "jim" :password "password"}}))]
      (is (= (:status response) 403))))
  (testing "disallowed method"
    (let [response (run auth-resource
                     (request {:request-method :post}))]
      (is (= (:status response) 405)))))

(deftest asserted-errors
  (testing "no service"
    (let [response (run (resource {:service-available? (fnk [] false)})
                     {:request-method :get})]
      (is (= (:status response) 503))))
  (testing "not known method"
    (let [response (run (resource {})
                     {:request-method :weird})]
      (is (= (:status response) 501))))
  (testing "uri too long"
    (let [resource (resource {:uri-too-long?
                               (fnk [request]
                                 (> (count (:uri request)) 6))})
          long-response (run resource (request {:uri "/long-uri-goes-here"}))
          short-response (run resource (request {:uri "/shrt"}))]
      (is (= (:status long-response) 414))
      (is (= (:status short-response) 200)))))

