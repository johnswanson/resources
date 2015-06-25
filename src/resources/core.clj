(ns resources.core
  (:require [plumbing.core :refer [fnk defnk]]
            [plumbing.graph :as graph]
            [slingshot.slingshot :refer [throw+ try+]]))

(def server-error             500)
(def not-implemented          501)
(def service-not-available    503)

(def malformed                400)
(def unauthorized             401)
(def forbidden                403)
(def not-found                404)
(def method-not-allowed       405)
(def not-acceptable           406)
(def conflict                 409)
(def gone                     410)
(def precondition-failed      412)
(def request-entity-too-large 413)
(def uri-too-long             414)
(def unsupported-media-type   415)
(def unprocessable-entity     422)

(def multiple-representations 300)
(def moved-permanently        301)
(def see-other                303)
(def not-modified             304)
(def moved-temporarily        307)

(def ok                       200)
(def created                  201)
(def accepted                 202)
(def no-content               204)

(defn http-error! [error]
  (throw+ {:type :http-error
           :error error}))

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
    (not service-available?)    (http-error! service-not-available)
    (not known-method?)         (http-error! not-implemented)
    uri-too-long?               (http-error! uri-too-long)
    (not method-allowed?)       (http-error! method-not-allowed)
    malformed?                  (http-error! malformed)
    (not authorized?)           (http-error! unauthorized)
    (not allowed?)              (http-error! forbidden)
    (not valid-content-header?) (http-error! not-implemented)
    (not known-content-type?)   (http-error! unsupported-media-type)
    (not valid-entity-length?)  (http-error! request-entity-too-large)
    (not acceptable?)           (http-error! not-acceptable)
    (not processable?)          (http-error! unprocessable-entity)
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

   :media-type-renderers (fnk [] {"application/edn" pr-str
                                  "text/plain"      str})

   :default-media-type-renderer (fnk [] "text/plain")
   :default-media-type-renderer-fn
   (fnk [default-media-type-renderer
         media-type-renderers]
     (get media-type-renderers default-media-type-renderer))

   :media-type-renderer
   (fnk [media-type-renderers media-type default-media-type-renderer-fn]
     (get media-type-renderers
          media-type
          default-media-type-renderer-fn))

   :handle-500 (fnk [] "server-error")
   :handle-501 (fnk [] "not-implemented")
   :handle-503 (fnk [] "service-not-available")

   :handle-400 (fnk [] "malformed")
   :handle-401 (fnk [] "unauthorized")
   :handle-403 (fnk [] "forbidden")
   :handle-404 (fnk [] "not-found")
   :handle-405 (fnk [] "method-not-allowed")
   :handle-406 (fnk [] "not-acceptable")
   :handle-409 (fnk [] "conflict")
   :handle-410 (fnk [] "gone")
   :handle-412 (fnk [] "precondition-failed")
   :handle-413 (fnk [] "request-entity-too-large")
   :handle-414 (fnk [] "uri-too-long")
   :handle-415 (fnk [] "unsupported-media-type")
   :handle-422 (fnk [] "unprocessable-entity")

   :handle-300 (fnk [] "multiple-representations")
   :handle-301 (fnk [] "moved-permanently")
   :handle-303 (fnk [] "see-other")
   :handle-304 (fnk [] "not-modified")
   :handle-307 (fnk [] "moved-temporarily")

   :handle-200 (fnk [] "ok")
   :handle-201 (fnk [] "created")
   :handle-202 (fnk [] "accepted")
   :handle-204 (fnk [] "no-content")

   :status (fnk [request-method]
             (case request-method
               :get 200
               :post 201
               200))

   :body-rendered
   (fnk [body-raw media-type-renderer]
     (media-type-renderer body-raw))

   :body-raw (fnk [status
                   handle-500
                   handle-501
                   handle-503
                   handle-400
                   handle-401
                   handle-403
                   handle-404
                   handle-405
                   handle-406
                   handle-409
                   handle-410
                   handle-412
                   handle-413
                   handle-414
                   handle-415
                   handle-422
                   handle-300
                   handle-301
                   handle-303
                   handle-304
                   handle-307
                   handle-200
                   handle-201
                   handle-202
                   handle-204
                   :as resource]
               (let [kw (keyword (format "handle-%d" status))
                     body (resource kw)]
                 (when-not body (http-error! server-error))
                 body))

   :response (fnk [body-rendered status] {:status status
                                           :body body-rendered
                                           :headers {}})
   })

(defn resource [m]
  (let [graph (merge resource* m)
        compiled (graph/lazy-compile graph)]
    (fn [request]
      (let [response (compiled {:request request})]
        (try+ (do (assert-no-errors! response)
                  ((:request-method request) response)
                  (:response response))
              (catch [:type :http-error] {status :error}
                (let [new-graph (assoc graph :status (fnk [] status))
                      compiled (graph/lazy-compile new-graph)
                      response (compiled {:request request})]
                  (:response response)))
              (catch Object _
                (let [new-graph (assoc graph :status (fnk [] server-error))
                      compiled (graph/lazy-compile new-graph)
                      response (compiled {:request request})]
                  (:response response))))))))
