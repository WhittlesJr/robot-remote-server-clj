(ns robot-remote-server.core
  (:require [necessary-evil.core :as xml-rpc]
            [clojure.string :as str])
  (:import org.mortbay.jetty.Server)
  (:use [robot-remote-server keyword]
        ring.adapter.jetty))

(def *result* (atom {:status "PASS", :return "", :output "", :error "", :traceback ""}))
(def *server* (atom nil))

(defn find-kw-fn
  [this-ns fn-name]
  (ns-resolve this-ns (symbol fn-name)))

(defn clojurify-name
  "Make it nicer for Clojure developers to write keywords; replace underscores with dashes"
  [s]
  (str/replace s "_" "-"))

(defn wrap-rpc
  "Ring middleware to limit server's response to the particular path that RobotFramework petitions"
  [handler]
  (fn [req]
    (when (= "/RPC2" (:uri req))
      (handler req))))

;; WARNING: Less-than-functional code follows

(defn get-keyword-arguments*
  [a-ns kw-name]
  (let [clj-kw-name (clojurify-name kw-name)
        a-fn (find-kw-fn a-ns clj-kw-name)]
    (vec (map str (last (:arglists (meta a-fn)))))))

(defn get-keyword-documentation*
  [a-ns kw-name]
  (let [clj-kw-name (clojurify-name kw-name)
        a-fn (find-kw-fn a-ns clj-kw-name)]
    (:doc (meta a-fn))))

(defn get-keyword-names*
  [a-ns]
  (vec
   (map #(str/replace % "-" "_")
        (remove #(re-find #"(\*|!)" %)
                (map str
                     (map first (ns-publics a-ns)))))))
(defn run-keyword*
  [a-ns kw-name args]
  (let [clj-kw-name (clojurify-name kw-name)
        a-fn (find-kw-fn a-ns clj-kw-name)
        output (with-out-str (try
                                (apply a-fn args)
                                (catch Exception e
                                  (do
                                    (reset! *result* {:status "FAIL", :return "", :output "",
                                                      :error (with-out-str (prn e)), :traceback (with-out-str (.printStackTrace e))})
                                    @*result*))))]
    (swap! *result* assoc :output output :return output)
    @*result*))

(defmacro init-handler
  "Create handler for XML-RPC server. Justification: delayed evaluation of *ns*"
  []
  (let [this-ns *ns*]
    `(->
      (xml-rpc/end-point
       {:get_keyword_arguments       (fn
                                       [kw-name#]
                                       (get-keyword-arguments* ~this-ns kw-name#))
        :get_keyword_documentation   (fn
                                       [kw-name#]
                                       (get-keyword-documentation* ~this-ns kw-name#))
        :get_keyword_names           (fn []
                                       (get-keyword-names* ~this-ns))
        :run_keyword                 (fn
                                       [kw-name# args#]
                                       (run-keyword* ~this-ns kw-name# args#))        
        :stop_remote_server          (fn []
                                       (.stop @*server*))})
      wrap-rpc)))

(defn server-start!
  ([hndlr] (server-start! hndlr {:port 8270, :join? false}))
  ([hndlr opts] (reset! *server* (run-jetty hndlr opts))))

(defn server-stop!
  []
  (.stop @*server*))