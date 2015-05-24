(ns throttling-api.test
  (:require [throttling-api.api :as api] ))

(def token "key here")

(def p-get (partial api/get "https://graph.facebook.com" token))

(try @(p-get "/me?fields=id,name")
  (catch Exception e 
    (println "Here we handle error " (.getMessage e))))

