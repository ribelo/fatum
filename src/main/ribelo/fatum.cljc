(ns ribelo.fatum
  (:refer-clojure :exclude [-> ->> isa?])
  #?(:cljs
     (:require-macros [ribelo.fatum :refer [catching catch-errors attempt -> ->> when-ok if-ok]]))
  (:require
   #?(:clj  [clojure.core :as core]
      :cljs [cljs.core :as core]))
  #?(:clj (:import
         (ribelo.fatum Fail))))

#?(:clj
   (defmacro -if-clj  [then & [else]] (if (:ns &env) else then)))

#?(:cljs
   (defn ^{:jsdoc ["@constructor"]} Fail
     [msg data]
     (this-as this
       (set! (.-message ^js this) msg)
       (set! (.-data ^js this) data)
       (set! (.-get ^js this)
             (fn
               ([k] (-lookup data k))
               ([k not-found] (-lookup data k not-found))))
       this)))

#?(:cljs (set! (.. ^js Fail -prototype -__proto__) (.-prototype ^js ExceptionInfo)))

#?(:cljs
   (extend-type Fail
     ILookup
     (-lookup [this k] (.get this k))
     IAssociative
     (-contains-key? [this k] (-contains-key? (.-data this) k))
     (-assoc [this k v] (Fail. (.-message this) (-assoc (.-data this) k v)))
     IEquiv
     (-equiv [this o] (and (instance? js/Error o)
                           (= (.-message this)
                              (.-message o))
                           (= (.-data this)
                              (.-data o))))
     IEmptyableCollection
     (-empty [this] (Fail. (.-message this) {}))))

(defn fail
  "returns [[Fail]]"
  ([] (Fail. nil {}))
  ([msg] (Fail. msg {}))
  ([msg data]
   (Fail. msg (cond (map? data) data (nil? data) {} (sequential? data) (apply array-map data))))
  ([msg k v & kvs]
   (Fail. msg (apply array-map k v kvs))))

(defn fail!
  "throw [[Fail]]"
  ([] (throw (fail)))
  ([msg] (throw (fail msg)))
  ([msg data] (throw (fail msg data)))
  ([msg k v & kvs] (throw (apply fail msg k v kvs))))

(defn fail?
  "check if `x` is instance of Exception or js/Error in cljs"
  [x]
  #?(:clj
     (instance? java.lang.Exception x)
     :cljs
     (instance? js/Error x)))

(defn ensure-fail
  "ensure that `Exception` `err` is [[Fail]]"
  [x]
  (if (and (fail? x) #?(:clj (instance? java.lang.Exception x) :cljs (instance? js/Error x)))
    (fail (ex-message x) (ex-data x))
    x))

(defn ok?
  "check if `x` is not [[Fail]]"
  [x]
  (not (fail? x)))

(defn exception-info?
  "check if `x` is `ExceptionInfo`"
  [x]
  #?(:clj  (instance? clojure.lang.IExceptionInfo x)
     :cljs (instance? ExceptionInfo x)))

(defn -match-map?
  "chech if `x` has every `kv` from `m`"
  [x m]
  (when (or (map? x) (exception-info? x))
    (reduce-kv
     (fn [_ k v]
       (if (= v (get x k))
         true
         (reduced false)))
     true
     m)))

(defn isa?
  "check if `x` meets `pred`, or whether `Exception` has in `ex-data` under the
  key `k` the value `v`"
  [x pred]
   #?(:clj
      (cond
        (class? pred)
        (instance? pred x)
        (keyword? pred)
        (true? (get x pred))
        (fn? pred)
        (pred x)
        (map? pred)
        (-match-map? x pred))
      :cljs
      (cond
        (keyword? pred)
        (true? (get x pred))
        (fn? pred)
        (or (instance? pred x) (pred x))
        (map? pred)
        (-match-map? x pred))))

(defmacro catching
  "`try` to execute `expr`, if `catch` an error returns it itself"
  ([expr                     ] `(catching ~expr ~'_ nil))
  ([expr err catch]
   `(-if-clj
     (try ~expr (catch Exception ~err ~catch))
     (try ~expr (catch :default  ~err ~catch))))
  ([expr err catch finally]
   `(-if-clj
     (try ~expr (catch Exception ~err ~catch) (finally ~finally))
     (try ~expr (catch :default  ~err ~catch) (finally ~finally)))))

#?(:clj
   (defmacro catch-errors
     "like [[catching]], but returns a vector where the first element is the result of
  executing the `body` and the second is an `Exception`"
     [& body]
     `(-catching [(do ~@body) nil] e# [nil e#])))

#?(:clj
   (defmacro attempt
     "like [[catching]], but takes `body` as argument"
     [& body]
     `(catching (do ~@body) e# (ensure-fail e#))))

#?(:clj
   (defmacro when-ok
     "Like `clojure.core/when` however if first arg is binding vector behave like
  `clojure.core/when-let`, but can bind multiple values. check if all tests/bindings
  are [[ok?]], else return  `fail`"
     {:style/indent 1}
     ([test-or-bindings & body]
      (if (vector? test-or-bindings)
        (let [s (seq test-or-bindings)]
          (if s                         ; (when-let [] true) => true
            (let [[b1 b2 & bnext] s]
              `(let [b2# (attempt ~b2)
                     ~b1 b2#]
                 (if (ok? b2#)
                   (when-ok ~(vec bnext) ~@body)
                   (fail (ex-message b2#) {:binding '~b1 :expr '~b2}))))
            `(do ~@body)))
        `(when-ok [x# ~test-or-bindings] ~@body)))))

#?(:clj
   (defmacro if-ok
     "Like `core/if-let` but can bind multiple values for `then` iff all tests
  are `ok?`"
     {:style/indent 1}
     ([test-or-bindings then     ] `(when-ok ~test-or-bindings ~then))
     ([test-or-bindings then else]
      (if (vector? test-or-bindings)
        (let [s (seq test-or-bindings)]
          (if s                         ; (if-let [] true false) => true
            (let [[b1 b2 & bnext] s]
              `(let [b2# (attempt ~b2)
                     ~b1 b2#]
                 (if (ok? b2#)
                   (if-ok ~(vec bnext) ~then ~else)
                   (let [~b1 (assoc ~b1 :binding '~b1 :expr '~b2)] ~else))))
            then))
        `(if (ok? ~test-or-bindings) ~then ~else)))))

(defn call
  "[[attempt]] to call function `f` on value `x`"
  [x f]
  (attempt (f x)))

(defn then
  "[[attempt]] to call function `f` on value `x` if `x` is [[ok?]] and not
  `reduced`"
  [x f]
  (if (and (not (reduced? x)) (ok? x)) (attempt (f x)) x))

(defn then-if
  "[[attempt]] to call function `f` on value `x` if `x` is [[ok?]], not `reduced`
  and meets [[isa?]] condition"
  [x pred f]
  (if (and (not (reduced? x)) (ok? x) (isa? x pred)) (attempt (f x)) x))

(defn catch
  "[[attempt]] to call function `f` on value `x` if `x` is [[fail?]]"
  [x f]
  (if (fail? x) (attempt (f x)) x))

(defn catch-if
  "[[attempt]] to call function `f` on value `x` if `x` is [[fail?]], not
  `reduced` and meets [[isa?]] condition"
  [x pred f]
  (if (and (fail? x) (isa? x pred)) (attempt (f x)) x))

(defn fail-if
  "return [[fail]] with optional `msg` and `data` if `x` is [[ok?]] and
  meets [[isa?]] condition"
  ([x pred]
   (if (and (ok? x) (isa? (unreduced x) pred)) (fail) x))
  ([x pred msg]
   (if (and (ok? x) (isa? (unreduced x) pred)) (fail msg) x))
  ([x pred msg data-or-fn]
   (if (and (ok? x) (isa? (unreduced x) pred))
     (if (fn? data-or-fn)
       (fail msg (data-or-fn x))
       (fail msg data-or-fn))
     x)))

(defn throw-if
  "throw [[fail!]] with optional `msg` and `data` if `x` is [[ok?]] and
  meets [[isa?]] condition"
  ([x pred]
   (if-ok [result (fail-if x pred)] result (throw result)))
  ([x pred msg-or-fn]
   (if-ok [result (fail-if x pred msg-or-fn)] result (throw result)))
  ([x pred msg data]
   (if-ok [result (fail-if x pred msg data)] result (throw result))))

(defn finally
  "[[attempt]] to call function `f` on `unreduced` value of `x`. return `x`
  unchanged and `unreduced`."
  [x f]
  (attempt (f (unreduced x))) (unreduced x))

(defn maybe-throw
  "`throw` `x` if `x` meets [[fail?]]"
  [x]
  (when (fail? x) (throw x)))

(defn thru
  "[[attempt]] to call function `f` on `unreduced` value of `x`. return `x`
  unchanged. used for side effects"
  [x f]
  (attempt (f (unreduced x))) x)

(defn thru-if
  "[[attempt]] to call function `f` on `unreduced` value of `x` if `unreduced` `x`
  meets [[isa?]] condition. return `x` unchanged. used for side effects"
  ([x pred f]
   (if (isa? (unreduced x) pred) (do (f (unreduced x)) x) x)))

#?(:clj
   (defmacro ->
     "like `clojure.core/->` but `expr` is wrapped in [[attempt]], and the following functions in [[then]]"
     [expr & more]
     (let [ok (gensym "ok_")]
       `(core/-> (attempt ~expr)
                 ~@(map (fn [elem]
                          `(then (fn [~ok]
                                   ~(if (list? elem)
                                      `(when-ok [x# (~(first elem) ~ok ~@(rest elem))] x#)
                                      `(when-ok [x# (~elem ~ok)] x#)))))
                        more)))))

#?(:clj
   (defmacro ->>
     "like `clojure.core/->>` but `expr` is wrapped in [[attempt]], and the following
  functions in [[then]]"
     [expr & more]
     (let [ok (gensym "ok_")]
       `(core/->> (attempt ~expr)
                  ~@(map (fn [elem]
                           `(then (fn [~ok]
                                    ~(if (list? elem)
                                       `(when-ok [x# (~(first elem) ~ok ~@(rest elem))] x#)
                                       `(when-ok [x# (~elem ~ok)] x#)))))
                         more)))))

#?(:cljs
   (def transit-write-handlers
     {Fail
      (reify Object
        (tag [_ _] "f/fail")
        (rep [_ ^js err] [(ex-message err) (ex-data err)])
        (stringRep [_ _] nil)
        (verboseHandler [_] nil))}))

#?(:cljs
   (def transit-read-handlers {"f/fail" (fn [[msg data]] (fail msg data))}))
