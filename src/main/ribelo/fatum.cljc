(ns ribelo.fatum
  (:refer-clojure :exclude [-> ->> isa? and or every? some])
  #?(:cljs
     (:require-macros [ribelo.fatum :refer [catching catch-errors attempt -> ->> when-ok if-ok and or]]))
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
     (-equiv [this o] (core/and (instance? js/Error o)
                           (= (.-message this)
                              (.-message o))
                           (= (.-data this)
                              (.-data o))))
     IEmptyableCollection
     (-empty [this] (Fail. (.-message this) {}))))

(defn fail
  "returns [[Fail]]

  for flexibility, the `data` can be either a map, an explicit kv collection or
  a variadic kv collection "
  ([] (Fail. nil {}))
  ([msg] (Fail. msg {}))
  ([msg data]
   (Fail. msg (cond (map? data) data (nil? data) {} (sequential? data) (apply array-map data))))
  ([msg k v & kvs]
   (Fail. msg (apply array-map k v kvs))))

(defn fail!
  "throw [[Fail]]

  see [[fail]]"
  ([] (throw (fail)))
  ([msg] (throw (fail msg)))
  ([msg data] (throw (fail msg data)))
  ([msg k v & kvs] (throw (apply fail msg k v kvs))))

(defn fail?
  "check if `x` is instance of `Exception` in clj or `js/Error` in cljs"
  ([x]
   #?(:clj
      (instance? java.lang.Exception x)
      :cljs
      (instance? js/Error x)))
  ([x & more]
   (core/or
     (fail? x)
     (reduce
       (fn [_ y] (if (fail? y) (reduced true) false))
       false
       more))))

(defn fail?!
  "`throw` `x` if `x` meets [[fail?]] else returns x"
  ([x]
   (if (fail? x) (throw x) x))
  ([x & more]
   (fail?! x)
   (reduce
     (fn [_ y] (fail?! y))
     false
     more)))

(defn ensure-fail
  "ensure that `Exception` `err` is [[Fail]]"
  [x]
  (if #?(:clj (instance? java.lang.Exception x) :cljs (instance? js/Error x))
    (if (instance? Fail x) x (fail (ex-message x) (ex-data x)))
    (fail! "x is not a exception" {:x x :type (type x)})))

(defn ok?
  "check if `x` is not [[Fail]]"
  ([x]
   (not (fail? x)))
  ([x & more]
   (core/and
     (ok? x)
     (reduce
       (fn [_ y] (if-not (ok? y) (reduced false) true))
       true
       more))))

(defn ok?!
  "`throw` `x` if `x` meets [[fail?]] else returns x"
  ([x]
   (if (ok? x) x (throw x)))
  ([x & more]
   (ok?! x)
   (reduce
     (fn [_ y] (ok?! y))
     true
     more)))

(defn exception-info?
  "check if `x` is `ExceptionInfo`"
  [x]
  #?(:clj  (instance? clojure.lang.IExceptionInfo x)
     :cljs (instance? ExceptionInfo x)))

(defn -match-map?
  "chech if `x` has every `kv` from `m`"
  [x m]
  (when (core/or (map? x) (exception-info? x))
    (reduce-kv
     (fn [_ k v]
       (if (= v (get x k))
         true
         (reduced false)))
     true
     m)))

(defn isa?
  "check if `x` meets `pred`, or whether `Exception` has in `ex-data` under the
  key `k` the value `v`

  ```clojure
  (isa? 1 number?) => true
  ```
  ```clojure
  (isa? java.lang.ArithmeticException (catching (/ 1 0))) => true
  ```
  ```clojure
  (isa? {:a 1 :b 2} {:a 1}) => true
  ```
  ```clojure
  (isa? {:a 1 :b 2} {:c 3}) => false
  ```"
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
       (core/or (instance? pred x) (pred x))
       (map? pred)
       (-match-map? x pred))))

(defmacro catching
  "`try` to execute `expr`, if `catch` an error returns it itself

  ```clojure
  (catching (/ 1 0) => nil
  ```
  ```clojure
  (catching (/ 1 0) e e) => java.lang.ArithmeticException
  ```"
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
  executing the `body` and the second is an `Exception`

  ```clojure
  (catch-errors (/ 1 1)) => [1 nil]
  ```
  ```clojure
  (catch-errors (/ 1 0)) => [nil java.lang.ArithmeticException]
  ```"
     [& body]
     `(catching [(do ~@body) nil] e# [nil (ensure-fail e#)])))

#?(:clj
   (defmacro attempt
     "like [[catching]], but takes `body` as argument

  ```clojure
  (attempt (/ 1 1))
  => 1
  ```

  ```clojure
  (attempt (/ 1 0))
  =>
  #error {
  :cause \"Divide by zero\"
  :data {}
  :via
  [{:type ribelo.fatum.Fail
   :message \"Divide by zero\"
   :data {}}]
  :trace
  []}
  ```"
     [& body]
     `(catching (do ~@body) e# (ensure-fail e#))))

#?(:clj
   (defmacro when-ok
     "Like `clojure.core/when` however if first arg is binding vector behave like
  `clojure.core/when-let`, but can bind multiple values. check if all
  tests/bindings are [[ok?]], else return [[Fail]] with attached var & failing
  expresions

  ```clojure
  (when-ok (/ 1 1) :ok)
  => :ok
  ```

  ```clojure(when-ok nil :ok)
  => :ok
  ```

  ```clojure
  (when-ok (/ 1 0) :ok
  =>
  #error {
  :cause \"Divide by zero\"
  :data {:binding test, :expr (/ 1 0)}
  :via
  [{:type ribelo.fatum.Fail
   :message \"Divide by zero\"
   :data {:binding test, :expr (/ 1 0)}}]
  :trace
  []}
  ```

  ```clojure
  (when-ok [x (/ 1 0)] :ok)
  =>
  #error {
  :cause \"Divide by zero\"
  :data {:binding x, :expr (/ 1 0)}
  :via
  [{:type ribelo.fatum.Fail
   :message \"Divide by zero\"
   :data {:binding x, :expr (/ 1 0)}}]
  :trace
  []}
  ```
  "
     ([test-or-bindings & body]
      (if (vector? test-or-bindings)
        (let [s (seq test-or-bindings)]
          (if s                         ; (when-let [] true) => true
            (let [[b1 b2 & bnext] s]
              `(let [b2# (attempt ~b2)
                     ~b1 b2#]
                 (if (ok? b2#)
                   (when-ok ~(vec bnext) ~@body)
                   (fail (ex-message b2#) (merge (ex-data b2#) {::binding '~b1 ::expr '~b2})))))
            `(do ~@body)))
        `(when-ok [~'test ~test-or-bindings] ~@body)))))

#?(:clj
   (defmacro when-ok!
     "Like `clojure.core/when` however if first arg is binding vector behave like
  `clojure.core/when-let`, but can bind multiple values. check if all
  tests/bindings are [[ok?]], else throw [[Fail]] with attached var & failing
  expresions

  ```clojure
  (when-ok (/ 1 1) :ok)
  => :ok
  ```

  ```clojure(when-ok nil :ok)
  => :ok
  ```

  ```clojure
  (when-ok (/ 1 0) :ok
  =>
  #error {
  :cause \"Divide by zero\"
  :data {:binding test, :expr (/ 1 0)}
  :via
  [{:type ribelo.fatum.Fail
   :message \"Divide by zero\"
   :data {:binding test, :expr (/ 1 0)}}]
  :trace
  []}
  ```

  ```clojure
  (when-ok [x (/ 1 0)] :ok)
  =>
  #error {
  :cause \"Divide by zero\"
  :data {:binding x, :expr (/ 1 0)}
  :via
  [{:type ribelo.fatum.Fail
   :message \"Divide by zero\"
   :data {:binding x, :expr (/ 1 0)}}]
  :trace
  []}
  ```
  "
     ([test-or-bindings & body]
      (if (vector? test-or-bindings)
        (let [s (seq test-or-bindings)]
          (if s                         ; (when-let [] true) => true
            (let [[b1 b2 & bnext] s]
              `(let [b2# (attempt ~b2)
                     ~b1 b2#]
                 (if (ok? b2#)
                   (when-ok ~(vec bnext) ~@body)
                   (fail! (ex-message b2#) (merge (ex-data b2#) {::binding '~b1 ::expr '~b2})))))
            `(do ~@body)))
        `(when-ok [~'test ~test-or-bindings] ~@body)))))

#?(:clj
   (defmacro if-ok
     "Like `core/if-let` but can bind multiple values. execute `then` if all tests
  are `ok?`

  ```clojure
  (if-ok (/ 1 1) :ok :err)
  => :ok
  ```

  ```clojure
  (if-ok (/ 1 1) :ok :err)
  => :err
  ```

  ```clojure
  (if-ok nil :ok :err)
  => :ok
  ```"
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
  "[[attempt]] to call function `f` on value `x`

  ```clojure
  (call 1 inc)
  => 2
  ```

  ```clojure
  (call \"1\" inc)
  =>
  #error {
  :cause \"class java.lang.String cannot be cast to class java.lang.Number ...\"
  :data {}
  :via
  [{:type ribelo.fatum.Fail
   :message \"class java.lang.String cannot be cast to class java.lang.Number ... \"
   :data {}}]
  :trace
  []}
  ````
  "
  [x f]
  (attempt (f x)))

(defn then
  "[[attempt]] to call function `f` on value `x` if `x` is [[ok?]] and is not
  `reduced`

  ```clojure
  (-> {:name \"Ivan\" :age 17}
      (then #(update % :age inc)))
  => {:name \"Ivan\", :age 18}
  ```"
  [x f]
  (if (core/and (not (reduced? x)) (ok? x)) (attempt (f x)) x))

(defn then-if
  "[[attempt]] to call function `f` on value `x` if `x` is [[ok?]], not `reduced`
  and meets [[isa?]] condition

  ```clojure
  (-> {:name \"Ivan\" :age 17}
      (then-if (comp (partial <= 18) :age) #(assoc % :adult true))
      (then-if (comp (partial > 18) :age) #(assoc % :adult false)))
   => {:name \"Ivan\", :age 17, :adult false}
  ```"
  [x pred f]
  (if (core/and (not (reduced? x)) (ok? x) (isa? x pred)) (attempt (f x)) x))

(defn catch
  "[[attempt]] to call function `f` on value `x` if `x` is [[fail?]]

  ```clojure
  (-> {:name \"Ivan\" :age 17}
      (then-if (comp (partial <= 18) :age) #(assoc % :adult true))
      (then-if (comp (partial > 18) :age) #(assoc % :adult false))
      (fail-if (complement (comp :adult)) \"user is underage\" #(find % :age))
      (catch-if (constantly :err)))
  => :err
  ```"
  [x f]
  (if (fail? x) (attempt (f x)) x))

(defn catch-if
  "[[attempt]] to call function `f` on value `x` if `x` is [[fail?]], not
  `reduced` and meets [[isa?]] condition

  ```clojure
  (-> {:name \"Ivan\" :age 17}
      (then-if (comp (partial <= 18) :age) #(assoc % :adult true))
      (then-if (comp (partial > 18) :age) #(assoc % :adult false))
      (fail-if (complement (comp :adult)) \"user is underage\" #(find % :age))
      (catch-if (comp (partial > 18) :age) (constantly :err)))
  => :err
  ```"
  [x pred f]
  (if (core/and (fail? x) (isa? x pred)) (attempt (f x)) x))


(defn fail-if
  "return [[fail]] with optional `msg` and `data` if `x` is [[ok?]] and
  meets [[isa?]] condition

  ```clojure
  (-> {:name \"Ivan\" :age 17}
      (then-if (comp (partial <= 18) :age) #(assoc % :adult true))
      (then-if (comp (partial > 18) :age) #(assoc % :adult false))
      (fail-if (complement (comp :adult)) \"user is underage\" (juxt (constantly :user) identity)))
  =>
  #error {
  :cause \"user is underage\"
  :data {:user {:name \"Ivan\", :age 17, :adult false}}
  :via
  [{:type ribelo.fatum.Fail
   :message \"user is underage\"
   :data {:user {:name \"Ivan\", :age 17, :adult false}}}]
  :trace
  []}
  ```"
  ([x pred]
   (if (core/and (ok? x) (isa? (unreduced x) pred)) (fail) x))
  ([x pred msg]
   (if (core/and (ok? x) (isa? (unreduced x) pred)) (fail msg) x))
  ([x pred msg data-or-fn]
   (if (core/and (ok? x) (isa? (unreduced x) pred))
     (if (fn? data-or-fn)
       (fail msg (data-or-fn x))
       (fail msg data-or-fn))
     x)))

(defn throw-if
  "throw [[fail!]] with optional `msg` and `data` if `x` is [[ok?]] and
  meets [[isa?]] condition

  see [[fail-if]]"
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

(defn every?
  [pred xs]
  (core/or
   (reduce (fn [_ x] (if (pred x) true (reduced false))) true xs)
   (fail "not every?" {:pred pred :xs xs})))

(defn some
  [pred xs]
  (core/or
   (reduce (fn [_ x] (if (pred x) (reduced true) false)) false xs)
   (fail "not some" {:pred pred :xs xs})))

#?(:clj
   (defmacro and
     ([] true)
     ([x] (when-ok x x))
     ([x & next]
      `(let [and# ~x]
         (if (ok? and#) (and ~@next) and#)))))

#?(:clj
   (defmacro or
     ([] true)
     ([x] (when-ok x x))
     ([x & next]
      `(let [or# ~x]
         (if (ok? or#) or# (or ~@next))))))

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
   (def ^:no-doc transit-write-handlers
     {Fail
      (reify Object
        (tag [_ _] "f/fail")
        (rep [_ ^js err] [(ex-message err) (ex-data err)])
        (stringRep [_ _] nil)
        (verboseHandler [_] nil))}))

#?(:cljs
   (def ^:no-doc transit-read-handlers {"f/fail" (fn [[msg data]] (fail msg data))}))
