;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.clj-peg.core
    (:import (com.lithinos.clj_peg PegError CyclicalError)))

(gen-interface
 :name com.lithinos.clj_peg.IWrapper
 :methods [
    [consume        [Object]    Object]
    [context        []          Object]
    [fail           [Throwable] Boolean]
    [getMark        []          Object]
    [isEmpty        []          Boolean]
    [returnToMark   [Object]    Boolean]
    [test           [Object]    Boolean]
    ])

(use '(com.lithinos.clj-peg string-wrapper))    

(def #^{:private true} *debug-indent* "  ")

(defn- flat-debug [w & strs]
    (if @(:debug w)
        (apply println strs)))

(defn- debug [w & strs]
    (if (:debug w)
        (let [location  (first (last @(:call-stack w)))
              level     (count @(:call-stack w))
              tabs      (apply str (take level (iterate str *debug-indent*)))]
            (apply println (map (fn [s] (str tabs "[" location "] " s)) strs))
            (flush))))

(defn- debug-dir [w dir]
    (if (:debug w)
        (let [location  (first (last @(:call-stack w)))
              level     (count @(:call-stack w))
              tabs      (apply str (take level (iterate str *debug-indent*)))]
            (cond
                (= dir :Enter)  (println (str tabs "(" location " at " (.context (:i w))))
                (= dir :Exit)   (println (str tabs location ")")) ; Maybe this should backup and put the closing paren on the end of the prev line
                true            (throw (PegError. "The 'dir' parameter of debug-dir must be either :Enter or :Exit")))
            (flush))))

(defn- gen-leaf-rule-body [non-terminal rule]
    (let [nts   (.getName non-terminal)
          k     (keyword nts)
          w     (gensym "w")]
        `(intern
            ~*ns*
            (with-meta (symbol ~nts) {:private true})
            (fn [~w]
                (dosync (alter (:call-stack ~w) conj [~k (.getMark (:i ~w))]))
                (let [result# (.test (:i ~w) ~rule)]
                    (dosync (alter (:call-stack ~w) pop))
                    (if result#
                        {~k (.consume (:i ~w) ~rule)}
                        (throw (PegError. (str "\n\t" ~nts ": Unable to complete rule '" (print-str ~rule) "'")))))))))

(defn- gen-node-rule-body [non-terminal rule]
    (let [nts   (.getName non-terminal)
          k     (keyword nts)
          w     (gensym "w")]
        `(intern
            ~*ns*
            (with-meta (symbol ~nts) {:private true})
            (fn [~w]
                (@#'com.lithinos.clj-peg.core/debug-dir ~w :Enter)
                (try
                    (let [cs#               @(:call-stack ~w)
                          previous-calls#   (filter (fn [a#] (= (first a#) ~k)) cs#)]
                        (if (and    (> (count previous-calls#) 0)
                                    (=  (second (last previous-calls#))
                                        (.getMark (:i ~w))))
                            (throw (CyclicalError. (str "Infinite cycle error calling " ~nts)))))
                    (dosync (alter (:call-stack ~w) conj [~k (.getMark (:i ~w))]))
                    (let [result# (~rule ~w)]
                        (@#'com.lithinos.clj-peg.core/debug-dir ~w :Exit)
                        (dosync (alter (:call-stack ~w) pop))
                        {~k result#})
                    (catch PegError e# (let [stack-at-failure# @(:call-stack ~w)]
                                        (dosync (alter (:call-stack ~w) pop))
                                        (throw (PegError. (str "\n\t" ~nts (.getMessage e#)))))))))))

(defn- gen-rule-body [non-terminal rule]
    (if (or (list? rule)
            (symbol? rule)
            (instance? clojure.lang.Cons rule))
        (gen-node-rule-body non-terminal rule)
        (gen-leaf-rule-body non-terminal rule)))

(defn- expand-productions [non-terminals rules]
    (loop [non-terminals    non-terminals
           rules            rules
           result           ()]
        (if (> (count non-terminals) 0)
            (recur
                (rest non-terminals)
                (rest rules)
                (conj
                    result
                    (gen-rule-body
                        (first non-terminals)
                        (first rules))))
            (reverse result))))

(declare process)

(defn- process-symbol [s]
    (cond
        (= s (symbol "$"))  '(@#'com.lithinos.clj-peg.core/=e)
        (= s (symbol "+"))  '@#'com.lithinos.clj-peg.core/=+
        (= s (symbol "|"))  '@#'com.lithinos.clj-peg.core/=o
        (= s (symbol "!"))  '@#'com.lithinos.clj-peg.core/=!
        (= s (symbol "?"))  '@#'com.lithinos.clj-peg.core/=?
        (= s (symbol "&"))  '@#'com.lithinos.clj-peg.core/=&
        (= s (symbol "*"))  '@#'com.lithinos.clj-peg.core/=*
        (= s (symbol "="))  '@#'com.lithinos.clj-peg.core/=specified
        (= s (symbol "<+"))  '@#'com.lithinos.clj-peg.core/=+gather
        true                s))

(defn- process-map [rule-map]
    (if (not= 1 (count (keys rule-map)))
        (throw (PegError. "Mappings are only allowed to have a single key/value pair")))
    (let [k (first (keys rule-map))
          v (first (vals rule-map))]
        (cond
            (symbol? k) `(@#'com.lithinos.clj-peg.core/=ast ~k ~(process v))
            true        `(@#'com.lithinos.clj-peg.core/=track ~k ~(process v)))))

(defn- process-vector [rule-vector]
    `(@#'com.lithinos.clj-peg.core/=s ~(process (first rule-vector)) ~@(rest (process (rest rule-vector)))))

(defn- process-list [rule-list]
    ;(println "Process list" (first (rest rule-list)))
    (if (= (symbol "=") (first rule-list))
        `(~(process-symbol (first rule-list)) '~(first (rest rule-list))    ~@(map process (rest    (rest rule-list))))
        `(~(process-symbol (first rule-list))                               ~@(map process          (rest rule-list)))))

(defn- process [rule]
    (let [c (class rule)]
        (cond   ; This needs to be cleaned up...
            (= c clojure.lang.PersistentHashMap)        (process-map        rule)
            (= c clojure.lang.PersistentArrayMap)       (process-map        rule)
            (= c clojure.lang.PersistentVector)         (process-vector     rule)
            (= c clojure.lang.APersistentVector$Seq)    (process-vector     rule)
            (= c clojure.lang.LazilyPersistentVector)   (process-vector     rule)
            (= c clojure.lang.PersistentList)           (process-list       rule)
            (= c clojure.lang.PersistentList$EmptyList) (process-list       rule)
            (= c clojure.lang.Symbol)                   (process-symbol     rule)
            ; These need to be run by the related wrapper... but that's not possible at this point in the process
            ; Maybe treat them all as rules? Then add logic into the call rule that handles the leafs?
            (= c java.util.regex.Pattern)               rule
            (= c String)                                rule
            (= c nil)                                   nil
            true                                        (println "No defn for processing" rule "as" c))))

(defn- pre-process [rules]
    (map process rules))

(defn- dump-header [fn-name fn-doc productions]
    (println "=============================")
    (printf "Creating: %s\n" fn-name)
    (printf "\t%10s  %s\n" "doc:"       fn-doc)
    (printf "\t%10s\n" "Rules:")
    (doall (map (fn [[a b c]] (println (format "   %20s" a) b (if (string? c) (str "\"" c "\"") c))) productions))
    (println "============================="))

(defn- extract-and-wrap-rule [production]
    (let [non-terminal  (first production)
          rule          (nth production 2)]
        (if (and
                (not= (class rule) String)
                (not= (class rule) java.util.regex.Pattern)
                (contains? (ns-interns *ns*) (symbol (str non-terminal "-ast"))))
            `{~(symbol (str non-terminal "-ast")) ~rule}
            rule)))

(declare is-valid-peg?)

(defn- validate-rules [wrapped-rules]
    (try
        (@#'com.lithinos.clj-peg.core/is-valid-peg? wrapped-rules)
        true
        (catch Throwable e false)))

(defmacro make-parser [a]
    (let [debug             (if (contains? a :debug)        (a :debug)          false)
          show-header       (if (contains? a :show-header)  (a :show-header)    false)
          fn-name           (a :main)
          fn-doc            (if (contains? a :doc)        (a :doc)              "No doc string supplied")
          has-root-ast      (contains? (ns-interns *ns*) (symbol (str fn-name "-ast")))
          root-ast          (ns-resolve *ns* (symbol (str fn-name "-ast")))
          rules-as-string   (pr-str (a :rules))
          wrapped-rules     (wrap-string rules-as-string)
          rules-are-valid   (if (and    (contains? a :skip-validation)
                                        (a :skip-validation))
                                true
                                (validate-rules wrapped-rules))
          productions       (partition 3 (a :rules))
          non-terminals     (map first productions)
          rules             (map extract-and-wrap-rule productions)
          starting-exp      (first non-terminals)
          result            (gensym "result")]
        (when (not rules-are-valid)
            (@#'com.lithinos.clj-peg.core/is-valid-peg? wrapped-rules)
            (throw (Error. "Invalid rules")))
        (when show-header (dump-header fn-name fn-doc productions))
        `(list
            (declare ~fn-name ~@(map #(symbol (.getName %)) non-terminals))
            ~@(expand-productions non-terminals (pre-process rules))
            (defn ~fn-name
                ~fn-doc
                [#^IWrapper given-wrapper#]
                (let [w# {  :debug      ~debug
                            :call-stack (ref [])
                            :scope      (ref {})
                            :i          given-wrapper#}]
                    (try
                        (@#'com.lithinos.clj-peg.core/debug w# "Processing input" (format "'%s'" (w# :i)))
                        (dosync (alter (w# :call-stack) conj [:The-very-beginning -1]))
                        (@#'com.lithinos.clj-peg.core/debug w# "a")
                        (let [~result (~starting-exp w#)]
                            (@#'com.lithinos.clj-peg.core/debug-dir w# :Exit)
                            (dosync (alter (w# :call-stack) pop))
                            (dosync (alter (w# :call-stack) conj [:The-result -1]))
                            (@#'com.lithinos.clj-peg.core/debug w# ~result)
                            (if (contains? (ns-interns *ns*) (symbol ~(str fn-name "-ast")))
                                ((ns-resolve *ns* (symbol ~(str fn-name "-ast"))) ~result)
                                ~(if has-root-ast
                                    `(~root-ast ~result)
                                    `~result)))
                        (catch PegError e#
                            (@#'com.lithinos.clj-peg.core/debug
                                w#
                                ; This should really be passed over to the wrapper to decide what to dump on an error...
                                (format "\nERROR\n\tContext: '%s'\n\tMark: '%s'\n\tisEmpty: '%s'\n\t%s"
                                    (.context (w# :i))
                                    (.getMark (w# :i))
                                    (.isEmpty (w# :i))
                                    (.getMessage e#)))
                            (.fail (w# :i) e#))))))))

(def #^{:private true :doc "This doesn't really serve a purpose since its position is ignored when the macro expands"}
    <- :produces)

(defn- =o "Ordered options" [& sub-peg]
    (if (first sub-peg)
        (fn [w]
            (let [m (.getMark (:i w))]
                (try
                    ((first sub-peg) w)
                    (catch PegError e 
                        (do
                            (.returnToMark (:i w) m)
                            ((apply @#'com.lithinos.clj-peg.core/=o (rest sub-peg)) w)))
                    (catch CyclicalError e 
                        (if (first (rest sub-peg))
                           (do
                                (.returnToMark (:i w) m)
                                ((apply @#'com.lithinos.clj-peg.core/=o (rest sub-peg)) w))))
                                )))
        (throw (PegError. "Ran out of options."))))

(defn- =s "Sequenced" [& sub-peg]
    (fn [w]
        (loop [parts sub-peg
               result []]
            (if (> (count parts) 0)
                (recur
                    (rest parts)
                    (conj result ((first parts) w)))
                result))))

(defn- =* "Zero or more" [sub-peg]
    (fn [w]
        (if (.isEmpty (:i w))
            {:Zero-or-more :Zero}
            (loop [result   ()
                   m        (.getMark (:i w))]
                    (try
                        ; This needs to be tried at least once...
                        ; ... but if it was run once and found nothing, then it should not continue running...
                        (if (.isEmpty (:i w))
                            (if (> (count result) 0)
                                (reverse result)
                                {:Zero-or-more :Zero})
                            (recur
                                (conj result (sub-peg w))
                                (.getMark (:i w))))
                        (catch PegError e
                            (.returnToMark (:i w) m)
                            (if (> (count result) 0)
                                (reverse result)
                                {:Zero-or-more :Zero})))))))

(defn- =+ "One or more" [sub-peg]
    (fn [w]
        (loop [result (conj () (sub-peg w))]
            (try
                (recur (conj result (sub-peg w)))
                (catch PegError e (reverse result))))))

(defn- =? "One or none" [sub-peg]
    (fn [w]
        (try
            (sub-peg w)
        (catch PegError e ()))))

(defn- get-specific-count [w i]
    (if (seq? i)
        (let [method            (first i)
              scope             @(w :scope)
              get-tracked-value (fn [a]
                                    (if (contains? scope a)
                                        (scope a)
                                        a))
              args              (map get-tracked-value (rest i))]
            (apply (resolve method) args))
        i))

(defn- =specified "A specific count" [i sub-peg]
    (fn [w]
        (loop [times-left   (get-specific-count w i)
               result       ()]
            (if (= times-left 0)
                (reverse result)
                (recur  (dec times-left)
                        (conj result (sub-peg w)))))))

(defn- lookahead "General lookahead" [case-1 case-2 & sub-peg]
    (fn [w]
        (let [m             (.getMark (:i w))
              throw-error   (try
                                ((apply @#'com.lithinos.clj-peg.core/=o sub-peg) w)
                                case-1
                                (catch PegError e case-2))]
            (.returnToMark (:i w) m)
            (when throw-error (throw (PegError. "Lookahead failed"))))
        true))

(defn- =& "Positive lookahead" [& sub-peg]
    (apply @#'com.lithinos.clj-peg.core/lookahead false true sub-peg))

(defn- =! "Negative lookahead" [& sub-peg]
    (apply @#'com.lithinos.clj-peg.core/lookahead true false sub-peg))
            
(defn- =e "Empty" []
    (fn [w]
        (if (.isEmpty (:i w))
            {:Empty true}
            (throw (PegError. "Non-empty")))))

(defn- =+gather "Facilitate gathering one-or-more" [& sub-peg]
    (fn [w]
        (map    (fn [i] (second (first i)))
                ((apply @#'com.lithinos.clj-peg.core/=+ sub-peg) w))))

(defn- =track [k sub-peg]
    (fn [w]
        (let [result (sub-peg w)]
            (dosync (alter (w :scope) assoc k result))
            result)))

(defn- =ast [handler sub-peg]
	(fn [w]
		(handler (sub-peg w))))

(make-parser {
;    :debug              true
    :skip-validation    true
    :main               is-valid-peg?
    :rules (
        Expr                <- [OpenParen (* Whitespace) (+ Triplet) (* Whitespace) CloseParen $]
        Triplet             <- [Symbol Whitespace Junk Whitespace Body Whitespace]
        Symbol              <- #"^[A-Z:][A-Za-z0-9-]*"
        Junk                <- (+ [(! Whitespace) Any])
        Any                 <- #"^."
        Body                <- (| Grouping Symbol Terminal Ending)
        Grouping            <- (| Option Sequence ZeroOrMore OneOrMore ZeroOrOne SpecificCount PositiveLookahead NegativeLookahead Gather Track)
        GroupingBody        <- [Body (* [Whitespace Body])]
        Sequence            <- [OpenBracket (* Whitespace) GroupingBody                                         (* Whitespace) CloseBracket]
        Track               <- [OpenCurly   (* Whitespace) Symbol Whitespace Body                               (* Whitespace) CloseCurly]
        Option              <- [OpenParen   OptionSymbol                                Whitespace GroupingBody (* Whitespace) CloseParen]
        ZeroOrMore          <- [OpenParen   ZeroOrMoreSymbol                            Whitespace GroupingBody (* Whitespace) CloseParen]
        OneOrMore           <- [OpenParen   OneOrMoreSymbol                             Whitespace GroupingBody (* Whitespace) CloseParen]
        ZeroOrOne           <- [OpenParen   ZeroOrOneSymbol                             Whitespace GroupingBody (* Whitespace) CloseParen]
        PositiveLookahead   <- [OpenParen   PositiveLookaheadSymbol                     Whitespace GroupingBody (* Whitespace) CloseParen]
        NegativeLookahead   <- [OpenParen   NegativeLookaheadSymbol                     Whitespace GroupingBody (* Whitespace) CloseParen]
        Gather              <- [OpenParen   GatherSymbol                                Whitespace GroupingBody (* Whitespace) CloseParen]
        SpecificCount       <- [OpenParen   SpecificCountSymbol Whitespace CountOrCall  Whitespace GroupingBody (* Whitespace) CloseParen]
            CountOrCall             <- (| Count Call)
            Call                    <- [OpenParen FunctionSymbol Whitespace Symbol CloseParen]
            FunctionSymbol          <- #"^[A-Za-z][A-Za-z0-9-]*"
            OptionSymbol            <- "|"
            ZeroOrMoreSymbol        <- "*"
            OneOrMoreSymbol         <- "+"
            ZeroOrOneSymbol         <- "?"
            PositiveLookaheadSymbol <- "&"
            NegativeLookaheadSymbol <- "!"
            GatherSymbol            <- "<+"
            SpecificCountSymbol     <- "="
            Count                   <- #"^\d+"
            Ending                  <- "$"
            OpenBracket             <- "["
            CloseBracket            <- "]"
            OpenCurly               <- "{"
            CloseCurly              <- "}"
            OpenParen               <- "("
            CloseParen              <- ")"
            Whitespace              <- #"(?s)^[\s\n\r\t]+"
        Terminal            <- (| RegularExpression PegString)
            RegularExpression   <- [Hash PegString]
            Hash                <- "#"
            PegString                   <- (| DoubleQuotedString SingleQuotedString)
            DoubleQuotedString          <- [DoubleQuote (+ DoubleQuotedStringContent) DoubleQuote]
            SingleQuotedString          <- [SingleQuote (+ SingleQuotedStringContent) SingleQuote]
            DoubleQuotedStringContent   <- (| EscapedSlash EscapedDoubleQuote AnyNotDoubleQuote)
            SingleQuotedStringContent   <- (| EscapedSlash EscapedSingleQuote AnyNotSingleQuote)
                EscapedSlash        <- #"^[\\][\\]"
                EscapedDoubleQuote  <- #"^[\\]\""
                EscapedSingleQuote  <- #"^[\\]'"
                AnyNotDoubleQuote   <- #"^[^\"]"
                AnyNotSingleQuote   <- #"^[^']"
                DoubleQuote         <- "\""
                SingleQuote         <- "'")})

