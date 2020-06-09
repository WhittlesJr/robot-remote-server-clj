;; ## RobotFramework XML-RPC Remote Server in Clojure
;;
;; This XML-RPC server is designed to be used with RobotFramework (RF), to
;; allow developers to write RF keywords in Clojure.
;;
;; If you use Leiningen, just run `lein run` to use the example keyword
;; library included in the `robot-remote-server.keyword` namespace.
;;
;; Otherwise, `(:use)` the `robot-remote-server.core` namespace in your own
;; namespace containing RF keywords and add `(server-start! (init-handler))`
;; to start the remote server.
;;
;; You can pass a map of options to `(server-start!)` like you would to
;; `(run-jetty)`. To stop the server, use `(server-stop!)` or send
;; `:stop_remote_server` via RPC.
;;
;; Because RF sends requests to the /RPC2 path, that has been enforced for this
;; server using the `wrap-rpc` middleware defined in this namespace.
;;
(ns robot-remote-server.core
  (:require [clojure.stacktrace :as st]
            [clojure.string :as str]
            [lambdaisland.ansi :as ansi]
            [necessary-evil.core :as xml-rpc]
            [necessary-evil.value :as value]
            [ring.adapter.jetty :refer :all]
            [taoensso.encore :as encore]))

(defonce rrs-instance (atom nil))
(defonce enable-remote-stop (atom nil))

(defn find-kw-fn
  "Given a namespace and a fn-name as string, return the function in that namespace by that name"
  [a-ns fn-name]
  (ns-resolve a-ns (symbol fn-name)))

(defn clojurify-name
  "Make it nicer for Clojure developers to write keywords; replace underscores with dashes"
  [s]
  (str/replace s "_" "-"))

(defn kw-name->fn
  [a-ns kw-name]
  (->> (clojurify-name kw-name)
       (find-kw-fn a-ns)))

(defn wrap-rpc
  "Ring middleware to limit server's response to the particular path that RobotFramework petitions"
  [handler]
  (fn [req]
    (binding [value/*allow-nils* true]
      (when (= "/RPC2" (:uri req))
        (handler req)))))

(defn get-keyword-arguments*
  "Get arguments for a given RF keyword function identified by the string `kw-name` and located in the `a-ns` namespace"
  [a-ns kw-name]
  (let [a-fn         (kw-name->fn a-ns kw-name)
        args-as-strs (map str (last (:arglists (meta a-fn))))]
    (if (and (> (count args-as-strs) 1)
             (= "&" (nth args-as-strs (- (count args-as-strs) 2))))
      (let [last-arg     (last args-as-strs)
            trimmed-args (drop-last 2 args-as-strs)]
        (conj (vec trimmed-args) (str "*" last-arg)))
      args-as-strs)))

(defn get-keyword-documentation*
  "Get documentation string for a given RF keyword function identified by the string `kw-name` and located in the `a-ns` namespace"
  [a-ns kw-name]
  (-> (kw-name->fn a-ns kw-name)
      :doc
      (meta)))

(defn get-keyword-names*
  "Get a vector of RF keyword functions located in the `a-ns` namespace"
  [a-ns]
  (conj
   (vec
    (map #(str/replace % "-" "_")       ; RF expects underscores
         (remove #(or (re-find #"(\*|!)" %) (re-find #"^-" %)) ; non-keyword functions
                 (map str
                      (map first (ns-publics a-ns))))))
   "stop_remote_server"))

(declare stop-remote-server*)

(def return-val-defaults
  "RF expects this map"
  {:status    "PASS",
   :return    "",
   :output    "",
   :error     "",
   :traceback ""})

(defn sanitize-str [s]
  (->> (ansi/text->hiccup s)
       (map last)
       (str/join)
       (#(str/split % #"\\n"))
       (str/join "\n")))

(defn run-keyword*
  "Given a RF-formatted string representation of a Clojure function `kw-name` in
  the `a-ns` namespace called with `args` as a vector, evaluate the function"
  [a-ns kw-name args]
  (if (= kw-name "stop_remote_server")
    (stop-remote-server*)
    (let [a-fn (kw-name->fn a-ns kw-name)
          s    (new java.io.StringWriter)]
      (binding [*out* s]
        (let [result (try
                       {:return (apply a-fn args)}
                       (catch Throwable e
                         {:status    "FAIL"
                          :error     (with-out-str (prn e))
                          :traceback (with-out-str (st/print-stack-trace e))}))]
          (->> result
               (merge return-val-defaults
                      {:output (str s)})
               (encore/map-vals sanitize-str)))))))

;; WARNING: Less-than-functional code follows
;;
;; Use of `rrs-instance` inside the `init-handler` macro and in the two
;; functions that follow. This has been done so that the XML-RPC server can offer the
;; `stop_remote_server` command if desired.

(defn stop-remote-server*
  "Either stop the remote server (if enabled), or send a message explaining that
  it's disabled and what to do if you want to enable it"
  []
  (if (true? @enable-remote-stop)
    (do
      (future
        (Thread/sleep 3000)
        (.stop @rrs-instance))
      {:status "PASS", :return "The remote server has been stopped successfully.", :output "The remote server has been stopped successfully.", :error "", :traceback ""})
    (let [warning-msg (str "Stopping the server remotely has been disabled. "
                           "You can enable this functionality by passing 'true' "
                           "to the 'server-start!' function when starting the remote server. "
                           "See the project documentation at "
                           "http://github.com/semperos/robot-remote-server-clj "
                           "for more details.")]
      {:status "PASS", :return warning-msg, :output warning-msg, :error "", :traceback ""})))

(defmacro init-handler
  "Create handler for XML-RPC server. Set `expose-stop` to `false` to prevent exposing the `stop_remote_server` RPC command. Justification for using macro: delayed evaluation of `*ns*`"
  []
  (let [this-ns *ns*]
    `(->
      (xml-rpc/end-point
       {:get_keyword_arguments       (fn [kw-name#]
                                       (get-keyword-arguments* ~this-ns kw-name#))
        :get_keyword_documentation   (fn [kw-name#]
                                       (get-keyword-documentation* ~this-ns kw-name#))
        :get_keyword_names           (fn []
                                       (get-keyword-names* ~this-ns))
        :run_keyword                 (fn [kw-name# args#]
                                       (run-keyword* ~this-ns kw-name# args#))
        :stop_remote_server          (fn []
                                       (stop-remote-server*))})
      wrap-rpc)))

(defn server-start!
  "Given a Ring handler `hndlr`, an optional boolean to allow remote server stopping `enable-stop`, and an optional map of options to pass to Jetty, start a Jetty server"
  ([hndlr] (server-start! hndlr true {:port 8270, :join? false})) ; default to `true` to remain faithful to the RF spec
  ([hndlr enable-stop] (server-start! hndlr enable-stop {:port 8270, :join? false}))
  ([hndlr enable-stop opts]
   (when (and (not (nil? @rrs-instance)) (.isRunning @rrs-instance))
     (.stop @rrs-instance))
   (if (true? enable-stop)
     (reset! enable-remote-stop true)
     (reset! enable-remote-stop false))
   (reset! rrs-instance (run-jetty hndlr opts))))

(defn server-stop!
  "Stop the global Jetty server instance"
  []
  (.stop @rrs-instance))
