;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns pjstadig.scopes
  (:refer-clojure :exclude [closeable? scoped! scoped-thunk!
                            with-resource-scope])
  (:require [clojure.core.protocols :as p]
            [clojure.reflect :as reflect]
            [clojure.java.io :as io]))

(def ^:dynamic *resources*)

(defprotocol ScopedCloseable
  (close [this] "Close resource when leaving scope."))

(defmacro ^:private if-compile
  ([expr then] `(if-compile ~expr ~then nil))
  ([expr then else]
     (if (try (eval expr) (catch Throwable _ false))
       then
       else)))

(if-compile java.lang.AutoCloseable
  (extend-type java.lang.AutoCloseable
    ScopedCloseable (close [x] (.close x))))

(extend-type java.io.Closeable
  ScopedCloseable (close [x] (.close x)))

(def support-suppressed?
  (->> Throwable reflect/type-reflect :members
       (some #(= 'addSuppressed (:name %)))))

(defmacro ^:private add-suppressed*
  [t t'] (if support-suppressed? `(. ~t addSuppressed ~t')))

(defn ^:private add-suppressed
  [^Throwable t ^Throwable t'] (add-suppressed* t t'))

(defn close-resources
  "Close resources registered in *resources* suppressing any exceptions that
  occur."
  ([]
     (loop []
       (when (seq *resources*)
         (let [resource (first *resources*)]
           (set! *resources* (rest *resources*))
           (try
             (close resource)
             (catch Throwable t
               (close-resources t)))
           (recur)))))
  ([^Throwable t]
     (loop []
       (when (seq *resources*)
         (let [resource (first *resources*)]
           (set! *resources* (rest *resources*))
           (try
             (close resource)
             (catch Throwable t2
               (add-suppressed t t2)))
           (recur))))
     (throw t)))

(defmacro with-resource-scope
  "Establish a resource scope for body.  When with-resource-scope exits, all
  scoped objects that were registered with scoped! or scoped-thunk! during body
  will be closed.

  Scopes can be nested and only the resources registered within the nested
  scope will be cleaned up when the nested scope exist.

  If any exceptions occur when cleaning up resources they will be suppressed.
  See java.lang.Throwable#getSuppressed."
  [& body]
  `(binding [*resources* ()]
     (try
       ~@body
       (catch Throwable t#
         (close-resources t#))
       (finally
         (close-resources)))))

(defn closeable?
  "Return true if x satisfies ScopedCloseable."
  [x] (satisfies? ScopedCloseable x))

(defn scoped!
  "Returns x after registering it with the closest dynamic with-resource-scope.
  When its registeree scope exits, x will have its close method called.

  If a function f is given, then when its registeree scope exits, f will be
  called and given x as an argument, instead of attempting to call a close
  method on x."
  ([x]
     {:pre [(closeable? x)]}
     (set! *resources* (conj *resources* x))
     x)
  ([x f]
     (scoped! (reify
                ScopedCloseable
                (close [this]
                  (f x))))
     x))

(defn scoped-thunk!
  "Returns the thunk f after registering it with the closest dynamic
  with-resource-scope.  When its registeree scope exits, f will be called
  without any arguments."
  [f]
  (scoped! nil (fn [_] (f)))
  f)
