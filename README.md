<h1 align="center">Φαταλισμός</h1>

railway oriented programming for `clojure` & `clojurescript`, a functional approach to error handling.


### a word of introduction

`fatum` starting as a friendly copy of [fmnoise/flow](https://github.com/fmnoise/flow), but it went in a slightly different direction to suit my personal needs.


# rop

handling exceptions in a functional way, is a non-trivial problem. one way to solve it might be the [railway oriented programming (rop)](https://www.slideshare.net/ScottWlaschin/railway-oriented-programming) principle, widely used in other functional languages, but not very often (too rarely!) found in clojure.


## motivation

> A program is a spell cast over a computer, turning input into error messages.

```clojure
(defn create-user
  "completely meaningless but well nested code"
  [req]
  (cond
    (= 200 (:status req))
    (let [user (make-user (:body req))]
      (if (valid-user? user)
        (let [db-repsonse (update-db :add-user user)]
          (if (:success? db-reponse)
            db-response
            (throw (ex-info "failed updating db" {:user user :message (:message db-response)}))))
        (throw (ex-info "invalid user" {:user user}))))
    (= 500 (:status req))
    (throw (ex-info "something went wrong"))
    (= 404 (:status req))
    (throw (ex-info "something went even worse"))))
```

sufficiently complex, repeatedly revised and refactored code tends towards an infinite number of `if-else` conditions.


### performance

it is worth noting that exception handling in java is slow, very slow. using a custom type for exceptions allows the `stacktrace` to be omitted. throwing also involves an additional cost, the passing of value is much cheaper and faster. creating a fail is ~50x faster than creating an exception.


## fatum


### either

the main idea behind `fatum`, as behind [fmnoise/flow](https://github.com/fmnoise/flow/), is to separate values from errors. exceptions are first class citizens in `java/javascript` and there is no need for additional abstractions. each value can be either `ok?`, either `fail?`.


### Fail

`Fail` is a `class` inheriting from `ExceptionInfo`, which also behaves like a `hashmap`, allowing you to instantly check the contents of `ex-data` as well as add values to it. `get`, `assoc`, `keys`, `values`, `seq`, all tricks allowed.

in addition, to improve performance, the `stacktrace`, which is completely unnecessary in this case, is not included. creating a `Fail` as opposed to throwing an `Exception` is immediate and costless.


<a id="org3157b64"></a>

### attempt

this is similar to the solution found in [failjure](https://github.com/adambard/failjure#attempt). executes a body in `try/catch` context and, if an exception is thrown, exception is turned into `Fail` and returned as a value.

```clojure
(f/attempt (/ 1 0))
;; => #error {
;; :cause "Divide by zero"
;; :data {}
;; :via
;; [{:type ribelo.fatum.Fail
;;   :message "Divide by zero"
;;   :data {}}]
;; :trace
;; []}
```


### happy path

the most natural and idiomatic way to define a path is to use `->`, a similar solution can be found in `js`, where promises are handled with an endless sequence of `then`.

`then` [attempt](#org3157b64) to call function `f` when value is not meet `fail?` pred, otherwise bypass.

```clojure
(-> 1
    (f/then inc)
    (f/then inc)
    (f/then inc)
    (f/then inc))
;; => 5
```


### straying from the happy path&#x2026;

trying to use `->` together with `try/catch` will inevitably lead to a mess. again, `js` comes to the rescue, which implements a `catch` for promises.

`catch` [attempt](#org3157b64) to call function `f` when value meets `fail?` pred, otherwise bypass.

```clojure
(-> 1
    (f/then (fn [x] (/ x 0)))
    (f/catch (fn [err] (assoc err :code 418))))
;; => #error {
;; :cause "Divide by zero"
;; :data {:code 418}
;; :via
;; [{:type ribelo.fatum.Fail
;; :message "Divide by zero"
;; :data {:code 418}}]
;; :trace
;; []}

```


### side effects

the world is not `pure` and sometimes you just have to.

`thru` [attempt](#org3157b64) to call function `f`, bypassing value unchanged

```clojure
(-> 1 (f/then inc) (f/thru println))
;; => prints 2
;; => return 2

(-> 1 (f/then (fn [x] (/ x 0))) (f/thru println))
;; prints & return
;; => #error {
;; :cause "Divide by zero"
;; :data {}
;; :via
;; [{:type ribelo.fatum.Fail
;;   :message "Divide by zero"
;;   :data {}}]
;; :trace
;; []}
```


### another try

```clojure
(defn create-user
  "completely meaningless but well nested code"
  [req]
  (-> req
      (f/fail-if {:staus 500} "something went wrong")
      (f/fail-if {:staus 404} "something went even worse")
      (f/then-if {:status 200} (comp make-user :body))
      (f/fail-if (complement valid-user?) "invalid user" (partial array-map :user))
      (f/then (partial update-db :add-user))
      (f/fail-if (complement :success?) #(find % :message))
      (f/maybe-throw)))
```
