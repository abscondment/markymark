;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.clj-peg.string-wrapper
    (:import (com.lithinos.clj_peg IWrapper PegError)))

(defn- until-first-newline [s]
    (let [result (apply
                    str
                    (seq
                        (take-while
                            (fn [c]
                                (not= c \newline))
                            s)))]
        (str result
            (if (not= (count s) (count result))
                "..."
                ""))))

(defn- number-of-newlines [s]
    (count (filter #(= % \newline) s)))

(defn wrap-string [#^String i]
    (let [location  (ref 0)
          line      (ref 0)]
        (proxy [IWrapper] []

            (consume [terminal]
                (let [input-remainder   (subs i @location)
                      consumed          (if (instance? java.util.regex.Pattern terminal)
                                            (re-find terminal input-remainder)
                                            terminal)]
                    (dosync (alter location + (count consumed)))
                    (dosync (alter line + (number-of-newlines consumed)))
                    (if (> @location (count i))
                        (throw (PegError. "Consumed more than available")))
                    consumed))

            (context []
                (let [input-remainder   (subs i (max 0 @location))
                      remainder         (until-first-newline input-remainder)]
                    (subs   remainder
                            0
                            (min (count remainder) 40))))

            (fail [throwable]   (println "ERROR:")
                                (println "  Context: " (.context this))
                                (println "           " \u25B2)
                                (println "  Line:" @line)
                                (println "  Character:" @location)
                                (print "  Parse path:")
                                (println (.getMessage throwable))
                                false)

            (getMark [] @location)

            (isEmpty [] (>= @location (count i)))

            (returnToMark [mark] (dosync (ref-set location mark)) true)

            (test [terminal]
                (let [r (subs i @location)]
                    (if (instance? java.util.regex.Pattern terminal)
                        (not= (re-find terminal r) nil)
                        (=  (.indexOf r terminal) 0)))))))

