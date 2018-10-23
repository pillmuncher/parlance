(ns parlance)


(defn return [v]
  "Make value v the result.  Don't consume any input."
  (fn [s]
    [[v] s]))


(defn bind [p f]
  "Determine the parser to be invoked next by calling function f on the result
  of parser p (i.e. f must return a parser)."
  (fn [s]
    (let [[r1 s1] (p s)
          p1 (apply f r1)]
      (p1 s1))))


(defn epsilon [s]
  "Recognize an empty string."
  [[] s])


(defn eoi [s]
  "Recognize the end of the input string."
  (if (empty? s)
    [[] ""]
    (throw (ex-info "trailing characters!"
                    {:type :parsing-error
                     :cause :trailing-characters}))))


(defn fmap* [f p]
  "Transform the result of parser p by applying function f."
  (fn [s]
    (let [[r s1] (p s)]
      [[(apply f r)] s1])))


(defn ignore [p]
  "Invoke parser p but ignore its result."
  (fn [s]
    (let [[r s1] (p s)]
      [[] s1])))


(defn and-then [p1 p2]
  "Invoke parsers p1 and p2 in sequence.  p2 starts off where p1 stopped."
  (fn [s]
    (let [[r1 s1] (p1 s)
          [r2 s2] (p2 s1)]
      [(into r1 r2) s2])))


(defn or-else [p1 p2]
  "Invoke parsers p1 and p2 alternatively.  If p1 fails, invoke p2 instead at
  the same point in input."
  (fn [s]
    (try
      (p1 s)
      (catch clojure.lang.ExceptionInfo e
        (case (-> e ex-data :type)
          :parsing-error (p2 s)
          (throw e))))))


(defn chain [px py & ps]
  "Invoke parsers p... in sequence.  Each one starts off where the previous
  one stopped."
  (reduce and-then (and-then px py) ps))


(defn choice [px py & ps]
  "Invoke parsers p... alternatively at the same point in input, until one
  returns a result.  This becomes the choice parsers result."
  (reduce or-else (or-else px py) ps))


(defn zero-or-more [p]
  "Invoke parser p repeatedly until failure.  Collect and return all results.
  If p never succeeds, return an empty result."
  (fn [s]
    (loop [acc [] s s]
      (let [[acc1 s1] (try
                        (p s)
                        (catch clojure.lang.ExceptionInfo e
                          (case (-> e ex-data :type)
                            :parsing-error [nil nil]
                            (throw e))))]
        (if (nil? acc1)
          [acc s]
          (recur (into acc acc1) s1))))))


(defn one-or-more [p]
  "Invoke parser p repeatedly until failure.  Collect and return all results.
  If p never succeeds, fail."
  (and-then p (zero-or-more p)))


(defn zero-or-one [p]
  "Invoke parser p once, but ignore failure."
  (or-else p epsilon))


(def opt zero-or-one)


(defn char [c]
  "Parse the character c."
  (fn [s]
    (if (= c (first s))
      [[(str c)] (rest s)]
      (throw (ex-info (format "expected %s, found %s!" c (first s))
                      {:type :parsing-error
                       :cause :excpected-character-not-found})))))


(defn word [cs]
  "Parse a consecutive word consisting of any characters in cs."
  (->> cs
       (map char)
       (reduce or-else)
       (one-or-more)
       (fmap* str)))


(def lower-word (word "abcdefghijklmnopqrstuvwxyz"))


(def positive-digit (->> "123456789" (map char) (apply choice)))
(def digit (or-else positive-digit (char \0)))
(def digits (one-or-more digit))
(def positive-integer (fmap* str (and-then positive-digit (opt digits))))
(def opt-sign (opt (or-else (char \-) (char \+))))
(def non-negative-integer (or-else (char \0) positive-integer))
(def integer (fmap* str (and-then opt-sign non-negative-integer))
(def decimal (fmap* str (chain opt-sign integer (char \.) digits)))


(defn pop-chars [n]
  "Read the next n characters and return them as result, joined into a single
  string."
  (fn [s]
    [[(apply str (take n s))] (drop n s)]))


(def n-block (bind (fmap* #(Integer/parseInt %) positive-integer) pop-chars))
(def n-blocks (one-or-more n-block))


(n-blocks "5hallo7ingbertrest")
;;  Ergebnis:  [["hallo" "ingbert"] (\r \e \s \t)]
