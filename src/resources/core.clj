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

(defn throw-http-error [error] (throw+ (merge {:type :http-error} error)))

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
                          valid-content-header?]
  ;; these things ALWAYS throw errors.
  ;; we want to be able to throw them at any point throughout
  ;; the process.
  (cond
    (not service-available?)    (throw-http-error service-not-available)
    (not known-method?)         (throw-http-error not-implemented)
    uri-too-long?               (throw-http-error uri-too-long)
    (not method-allowed?)       (throw-http-error method-not-allowed)
    malformed?                  (throw-http-error malformed)
    (not authorized?)           (throw-http-error unauthorized)
    (not allowed?)              (throw-http-error forbidden)
    (not valid-content-header?) (throw-http-error not-implemented)
    (not known-content-type?)   (throw-http-error unsupported-media-type)
    (not valid-entity-length?)  (throw-http-error request-entity-too-large)
    (not acceptable?)           (throw-http-error not-acceptable)
    (not processable?)          (throw-http-error unprocessable-entity)
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

   :uri-too-long? (fnk [] false)

   :methods (fnk [] [:get])
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
       (some #{(get headers "content-type")}
             content-types)
       true))

   :valid-entity-length? (fnk [] true)

   :is-options? (fnk [request-method] (= request-method :options))

   :available-media-types (fnk [] ["text/plain"])
   :available-media-types-set
   (fnk [available-media-types]
     (set available-media-types))

   :accept (fnk [headers] (get headers "accept"))
   :accept-exists? (fnk [accept] accept)
   :media-type
   (fnk [available-media-types-set available-media-types accept-exists? accept]
     (when-not (empty? available-media-types-set)
       (cond
         (not accept-exists?)               (first available-media-types)
         (= accept "*/*")                   (first available-media-types)
         (available-media-types-set accept) (available-media-types-set accept)
         :else nil)))
   :media-type-available?
   (fnk [media-type]
     media-type)

   :accept-language (fnk [headers] (get headers "accept-language"))
   :accept-language-exists? (fnk [accept-language] accept-language)
   :language-available? (fnk [] true)

   :accept-charset (fnk [headers] (get headers "accept-charset"))
   :accept-charset-exists? (fnk [accept-charset] accept-charset)
   :charset-available? (fnk [] true)

   :accept-encoding (fnk [headers] (get headers "accept-encoding"))
   :accept-encoding-exists? (fnk [accept-encoding] accept-encoding)
   :encoding-available? (fnk [] true)

   :processable? (fnk [] true)

   :exists? (fnk [] true)

   :if-match (fnk [headers] (get headers "if-match"))
   :if-match-exists? (fnk [if-match] if-match)

   :if-unmodified-since (fnk [headers] (get headers "if-unmodified-since"))
   :if-unmodified-since-exists? (fnk [if-unmodified-since] if-unmodified-since)

   :if-none-match (fnk [headers] (get headers "if-none-match"))
   :if-none-match-exists? (fnk [if-none-match] if-none-match)

   :if-modified-since (fnk [headers] (get headers "if-modified-since"))
   :if-modified-since-exists? (fnk [if-modified-since] if-modified-since)

   :multiple-representations? (fnk [] false)

   :media-type-renderers (fnk [] {})
   :default-media-type-renderers (fnk [] {"application/edn" pr-str
                                          "text/plain"      str})

   :all-media-type-renderers (fnk [media-type-renderers
                                   default-media-type-renderers]
                               (merge default-media-type-renderers
                                      media-type-renderers))

   :media-type-renderer (fnk [all-media-type-renderers media-type]
                            (get all-media-type-renderers media-type))

   ;; the body of an OK response
   :handle-ok (fnk [] "successful response")

   :render-ok-body (fnk [handle-ok
                         media-type-renderer
                         media-type-available?
                         default-media-type-renderers]
                     (if media-type-available?
                       (media-type-renderer handle-ok)
                       ((first (vals default-media-type-renderers)) handle-ok)))

   :response (fnk [render-ok-body] {:status 200 :body render-ok-body :headers {}})
   })

(defn resource [m]
  (graph/compile (merge resource* m)))

(defn run [resource request]
  (let [r (resource {:request request})]
    (try+ (do (assert-no-errors! r)
              (:response r))
          (catch map? {type :http-error :as thing}
            thing))))

