;;;; Copyright © 2013, 2014 Paul Stadig and contributors. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns pjstadig.test.scopes
  (:refer-clojure :exclude [closeable? scoped! scoped-thunk!
                            with-resource-scope])
  (:require [clojure.test :refer :all]
            [pjstadig.scopes :refer :all]))

(deftest test-scoped
  (let [closed? (atom false)
        closer (fn [_] (reset! closed? true))]
    (with-resource-scope
      (scoped! nil closer))
    (is @closed?)
    (reset! closed? false)
    (try
      (with-resource-scope
        (scoped! nil closer)
        (throw (Exception.)))
      (catch Exception e))
    (is @closed?)))

(deftest test-suppression
  (try
    (with-resource-scope
      (scoped-thunk! #(throw (Exception. "bar")))
      (throw (Exception. "foo")))
    (catch Exception e
      (is (= "foo" (.getMessage e)))
      (when support-suppressed?
        (let [suppressed (.getSuppressed e)]
          (is (= 1 (count suppressed)))
          (is (= "bar" (.getMessage (aget suppressed 0))))))))
  (try
    (with-resource-scope
      (scoped-thunk! #(throw (Exception. "foo")))
      (scoped-thunk! #(throw (Exception. "bar"))))
    (catch Exception e
      (is (= "bar" (.getMessage e)))
      (when support-suppressed?
        (let [suppressed (.getSuppressed e)]
          (is (= 1 (count suppressed)))
          (is (= "foo" (.getMessage (aget suppressed 0)))))))))

(deftest test-return-value
  (is (= 1 (with-resource-scope 1))))
