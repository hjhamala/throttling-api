(ns throttling-api.api
  (:require 
    [clj-time.core :as t]
    [clj-time.coerce :as c]
    [clj-http.client :as client]
    [clojure.data.json :as json]))

(def ^:private queries-limit 600)

(def ^:private queries-time-limit 600)

(def ^:private requests (ref []))

(defn current-time-ms
  []
  (c/to-long (t/now)))

(defn- write-error
  "Writes error to file, throws error so future will contain an error"
  [error]
  (with-open [w (clojure.java.io/writer  "request_log.txt" :append true)]
        (.write w (str error "\n")))
  (throw (Exception. error)))

(defmulti ^:private http-request 
  (fn [method endpoint token path params] method))

(defmethod ^:private http-request "GET" 
  [method endpoint token path params]
  (try (json/read-str (:body (client/get (str endpoint path) {:query-params (assoc-in params [:access_token] token) 
                                                              :accept :json
                                                              :throw-entire-message? true})) :key-fn keyword)
    (catch Exception e (write-error (.getMessage e)))))
      

(defmethod ^:private http-request "POST" 
  [method endpoint token path params]
  (try (json/read-str (:body (client/post (str endpoint path) {:content-type :transit+json
                                                          ;; FB API seems to want params for post in query not in form? Or maybe it is only token?
                                                          :query-params  (assoc-in params [:access_token] token) 
                                                          :accept :json
                                                          :throw-entire-message? true})) :key-fn keyword)
    (catch Exception e (write-error (.getMessage e)))))

(defmethod ^:private http-request "PUT" 
  [method endpoint token path params]
  (try (json/read-str (:body (client/put (str endpoint path) {:content-type :transit+json
                                                         :query-params  (assoc-in params [:access_token] token) 
                                                         :accept :json
                                                         :throw-entire-message? true})) :key-fn keyword)
    (catch Exception e (write-error (.getMessage e)))))

(defn- future-request
  [request-map]
  (let [[sleep request-type endpoint token path body] request-map]
    (when (> sleep 0)
      (java.lang.Thread/sleep sleep))
    (http-request request-type endpoint token path body)))
  
(defn- request
  [method endpoint token path params]
  (let [request-map (dosync
                      (if (> queries-limit (count @requests))
                        ;; ok, we can do this
                        (do 
                          (alter requests conj (current-time-ms))
                          [0 method endpoint token path params])
                        ;; not sure that we can
                        (let [oldest-request (first @requests)]
                          ;; remove oldest-request from vector
                          (alter requests subvec 1)
                          ;; 
                          (if (and (< oldest-request (current-time-ms))
                                   (< (* 1000 queries-time-limit) (- (current-time-ms) oldest-request)))
                            ;; Oldest one is out of time-limit so we may push request immediately
                            (do 
                              (alter requests conj (current-time-ms))
                              [0 method endpoint token path params])
                            ;; Oldest one is in time limit or in the future, we have to sleep exactly time-limit
                            (let [sleep-till (+ (* 1000 queries-time-limit) oldest-request)]
                               (alter requests conj sleep-till)
                               [(- sleep-till (current-time-ms)) method endpoint token path params])))))]
    (future (future-request request-map))))
                          

(defn get
  [endpoint token path]
  (request "GET" endpoint token path nil))

(defn post
  [endpoint token path params]
  (request "POST" endpoint token path params))

(defn put
  [endpoint token path params]
  (request "POST" endpoint token path params))

