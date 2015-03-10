;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test.check
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :as ct]
            [clojure.test.check.rose-tree :as rose]))

(declare shrink-loop failure)

(defn- make-rng
  [seed]
  (if seed
    [seed (gen/random seed)]
    (let [non-nil-seed (System/currentTimeMillis)]
      [non-nil-seed (gen/random non-nil-seed)])))

(defn- complete
  [property num-trials seed]
  (ct/report-trial property num-trials num-trials)
  {:result true :num-tests num-trials :seed seed})

(defn- not-falsey-or-exception?
  "True if the value is not falsy or an exception"
  [value]
  (and value (not (instance? Throwable value))))

(defn numeric-stopper [num-tests]
  (fn
    ([] num-tests)
    ([so-far]
     (if (== so-far num-tests)
       :complete
       (inc so-far)))))

(defn- ^long ms-to-nanos [ms]
  (long (/ ms 1000000)))

(defn time-limiting-stopper [ms]
  (let [nanos (ms-to-nanos ms)]
    (fn
      ([] (System/nanoTime))
      ([so-far]
       (let [curr (System/nanoTime)]
         (if (>= curr nanos)
           :complete
           curr))))))

(defn- make-stopper [stop-options]
  (if (number? stop-options)
    (numeric-stopper stop-options)
    (if (and (map? stop-options)
             (number? (:ms stop-options)))
      (time-limiting-stopper (:ms stop-options))
      (assert false (str "couldn't determine what stop options to use, from " (pr-str stop-options) " pass a number to stop after a certain number of test runs, or a map {:ms 100} with a number of milliseconds to stop after that number of millisecond.")))))

(defn quick-check
  "Tests `property` until `stop-options` determine testing should stop.
  Takes optional keys `:seed` and `:max-size`. The seed parameter
  can be used to re-run previous tests, as the seed used is returned
  after a test is run. The max-size can be used to control the 'size'
  of generated values. The size will start at 0, and grow up to
  max-size, as the number of tests increases. Generators will use
  the size parameter to bound their growth. This prevents, for example,
  generating a five-thousand element vector on the very first test.

  Examples:

      (def p (for-all [a gen/pos-int] (> (* a a) a)))
      (quick-check 100 p)
  "
  [stop-options property & {:keys [seed max-size] :or {max-size 200}}]
  (let [[created-seed rng] (make-rng seed)
        size-seq (gen/make-size-range-seq max-size)
        stopper (make-stopper stop-options)]
    (loop [so-far (stopper)
           tests-run 0
           size-seq size-seq]
      (if (and (>= num-tests 1)
               (= so-far :complete))
        (complete property tests-run created-seed)
        (let [[size & rest-size-seq] size-seq
              result-map-rose (gen/call-gen property rng size)
              result-map (rose/root result-map-rose)
              result (:result result-map)
              args (:args result-map)]
          (if (not-falsey-or-exception? result)
            (do
              (ct/report-trial property so-far stop-options)
              (recur (stopper so-far) (inc tests-run) rest-size-seq))
            (failure property result-map-rose tests-run size created-seed)))))))

(defn- smallest-shrink
  [total-nodes-visited depth smallest]
  {:total-nodes-visited total-nodes-visited
   :depth depth
   :result (:result smallest)
   :smallest (:args smallest)})

(defn- shrink-loop
  "Shrinking a value produces a sequence of smaller values of the same type.
  Each of these values can then be shrunk. Think of this as a tree. We do a
  modified depth-first search of the tree:

  Do a non-exhaustive search for a deeper (than the root) failing example.
  Additional rules added to depth-first search:
  * If a node passes the property, you may continue searching at this depth,
  but not backtrack
  * If a node fails the property, search its children
  The value returned is the left-most failing example at the depth where a
  passing example was found."
  [rose-tree]
  (let [shrinks-this-depth (rose/children rose-tree)]
    (loop [nodes shrinks-this-depth
           current-smallest (rose/root rose-tree)
           total-nodes-visited 0
           depth 0]
      (if (empty? nodes)
        (smallest-shrink total-nodes-visited depth current-smallest)
        (let [[head & tail] nodes
              result (:result (rose/root head))]
          (if (not-falsey-or-exception? result)
            ;; this node passed the test, so now try testing its right-siblings
            (recur tail current-smallest (inc total-nodes-visited) depth)
            ;; this node failed the test, so check if it has children,
            ;; if so, traverse down them. If not, save this as the best example
            ;; seen now and then look at the right-siblings
            ;; children
            (let [children (rose/children head)]
              (if (empty? children)
                (recur tail (rose/root head) (inc total-nodes-visited) depth)
                (recur children (rose/root head) (inc total-nodes-visited) (inc depth))))))))))

(defn- failure
  [property failing-rose-tree trial-number size seed]
  (let [root (rose/root failing-rose-tree)
        result (:result root)
        failing-args (:args root)]

    (ct/report-failure property result trial-number failing-args)

    {:result result
     :seed seed
     :failing-size size
     :num-tests (inc trial-number)
     :fail (vec failing-args)
     :shrunk (shrink-loop failing-rose-tree)}))
