(ns resources.core
  (:require [plumbing.core :refer [fnk defnk]]
            [plumbing.graph :as graph]
            [slingshot.slingshot :refer [throw+ try+]]))

(def server-error             {:status 500})
(def not-implemented          {:status 501})
(def service-not-available    {:status 503})

(def malformed                {:status 400})
(def unauthorized             {:status 401})
(def forbidden                {:status 403})
(def not-found                {:status 404})
(def method-not-allowed       {:status 405})
(def not-acceptable           {:status 406})
(def conflict                 {:status 409})
(def gone                     {:status 410})
(def precondition-failed      {:status 412})
(def request-entity-too-large {:status 413})
(def uri-too-long             {:status 414})
(def unsupported-media-type   {:status 415})
(def unprocessable-entity     {:status 422})

(def multiple-representations {:status 300})
(def moved-permanently        {:status 301})
(def see-other                {:status 303})
(def not-modified             {:status 304})
(def moved-temporarily        {:status 307})

(def ok                       {:status 200})
(def created                  {:status 201})
(def accepted                 {:status 202})
(def no-content               {:status 204})

(defn throw-http-error [error] (throw+ error))

(defnk assert-no-errors! [service-available?
                          known-method?
                          uri-too-long?
                          malformed?
                          authorized?
                          allowed?
                          method-allowed?
                          acceptable?
                          valid-entity-length?
                          known-content-type?
                          processable?
                          valid-content-header?
                          exists?
                          request-method]
  (cond
    (not service-available?)    (throw+ service-not-available)
    (not known-method?)         (throw+ not-implemented)
    uri-too-long?               (throw+ uri-too-long)
    (not method-allowed?)       (throw+ method-not-allowed)
    malformed?                  (throw+ malformed)
    (not authorized?)           (throw+ unauthorized)
    (not allowed?)              (throw+ forbidden)
    (not valid-content-header?) (throw+ not-implemented)
    (not known-content-type?)   (throw+ unsupported-media-type)
    (not valid-entity-length?)  (throw+ request-entity-too-large)
    (not acceptable?)           (throw+ not-acceptable)
    (not processable?)          (throw+ unprocessable-entity)
    :else nil))

(def resource*
  {:acceptable? (fnk [encoding-available?
                      charset-available?
                      media-type-available?
                      language-available?]
                  (and media-type-available?
                       encoding-available?
                       charset-available?
                       language-available?))

   :headers
   (fnk [request]
     (:headers request))

   :service-available? (fnk [] true)

   :request-method
   (fnk [request]
     (:request-method request))

   :known-method?
   (fnk [request-method]
     (#{:get :post :put :delete :patch} request-method))

   :url-too-long? (fnk [] false)

   :methods [:get]
   :methods-set
   (fnk [methods] (set methods))

   :method-allowed?
   (fnk [methods-set request-method]
     (methods-set request-method))

   :malformed? (fnk [] false)
   :authorized? (fnk [] true)
   :allowed? (fnk [] true)

   :valid-content-header?
   (fnk [known-content-type?]
     known-content-type?)

   :content-types (fnk [] ["application/edn"])

   :known-content-type?
   (fnk [content-types headers request-method]
     (if (#{:put :post} request-method)
       (some #{(headers "content-type")}
             content-types)
       true))

   :valid-entity-length? (fnk [] true)

   :is-options? (fnk [request-method] (= request-method :options))

   :available-media-types (fnk [] ["text/plain"])
   :available-media-types-set
   (fnk [available-media-types]
     (set available-media-types))

   :accept (fnk [headers] (headers "accept"))
   :accept-exists? (fnk [accept] accept)
   :media-type-available?
   (fnk [available-media-types-set accept-exists? accept]
     (or (not accept-exists?)
         (= accept "*/*")
         (available-media-types-set accept)))

   :accept-language (fnk [headers] (headers "accept-language"))
   :accept-language-exists? (fnk [accept-language] accept-language)
   :language-available? (fnk [] true)

   :accept-charset (fnk [headers] (headers "accept-charset"))
   :accept-charset-exists? (fnk [accept-charset] accept-charset)
   :charset-available? (fnk [] true)

   :accept-encoding (fnk [headers] (headers "accept-encoding"))
   :accept-encoding-exists? (fnk [accept-encoding] accept-encoding)
   :encoding-available? (fnk [] true)

   :processable? (fnk [] true)

   :exists? (fnk [] true)
   :if-match-exists? (fnk [] false)
   :if-unmodified-since-exists? (fnk [] false)
   :if-none-match-exists? (fnk [] false)
   :if-modified-since-exists? (fnk [] false)
   :multiple-representations? (fnk [] false)

   :default-media-type-renderers (fnk [] {"application/edn" pr-str
                                          "text/plain"      str})

   :media-type
   (fnk [request available-media-types]
     (let [accept (get-in request [:headers "accept"])
           available-set (set available-media-types)]
       (if (or (= accept "*/*") (not (available-set accept)))
         (first available-media-types)
         (available-set accept))))

   :media-type-renderer
   (fnk [media-type default-media-type-renderers media-type-renderers]
     ((merge default-media-type-renderers media-type-renderers) media-type))

   :response (fnk [] {:status 200 :body "hey" :headers {}})
   })

(defn resource [m]
  (graph/compile (merge resource* m)))

(defn run [resource request]
  (let [r (resource {:request request})]
    (try+ (do (assert-no-errors! r)
              (:response r))
          (catch map? {:keys [status]}
            {:status status
             :body ""
             :headers {}}))))

