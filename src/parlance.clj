(ns parlance)


(defn fmap [f p]
  "Transform the result of parser p by applying function f."
  (fn [s]
    (let [[r s1] (p s)]
      [(f r) s1])))


(defn bind [p f]
  "Determine the parser to be invoked next by calling function f on the result
  of parser p (i.e. f must return a parser)."
  (fn [s]
    (let [[p1 s1] ((fmap f p) s)]
      (p1 s1))))


(defn return [v]
  "Make value v the result.  Don't consume any input."
  (fn [s]
    [v s]))


(defn epsilon [s]
  "Recognize an empty string."
  [[""] s])


(defn nothing [s]
  "Return an empty result."
  [[] s])


(defn eoi [s]
  "Recognize the end of the input string."
  (if (empty? s)
    [[] ""]
    (throw (ex-info "trailing characters!"
                    {:type :parsing-error
                     :cause :trailing-characters}))))


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
    (try (p1 s)
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
    (loop [acc []
           s s]
      (let [[acc1 s1] (try (p s)
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
  (or-else p nothing))


(def opt zero-or-one)


(defn char [cs]
  "Parse any of the characters in cs."
  (let [cs (set cs)]
    (fn [s]
      (if (contains? cs (first s))
        [[(str (first s))] (rest s)]
        (throw (ex-info (format "expected any of %s, found %s!" cs (first s))
                        {:type :parsing-error
                         :cause :excpected-character-not-found}))))))

(defn join [p]
  "Invoke parser p and join its result (a vector of strings) into a single
  string, wrapped in a vector."
  (fmap #(->> % (clojure.string/join) (vector)) p))


(defn word [cs]
  "Parse a consecutive word consisting of any characters in cs."
  (->> cs (char) (one-or-more) (join)))


(def space (char " "))
(def tab (char "\t"))
(def nl (char "\n"))
(def spaces (word " "))
(def tabs (word "\t"))
(def nls (word "\n"))
(def ws (word " \t\n"))
(def opt-ws (opt ws))

(def positive-digit (char "123456789"))
(def digit (char "1234567890"))
(def digits (join (one-or-more digit)))
(def positive-integer (join (chain positive-digit (opt digits))))
(def non-negative-integer (choice (char "0") positive-integer))
(def opt-sign (opt (char "-+")))
(def integer (join (chain opt-sign non-negative-integer)))
(def decimal (join (chain integer (opt (chain (char ".") digits)))))

(def lower-char (char "abcdefghijklmnopqrstuvwxyz"))
(def upper-char (char "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def letter (choice lower-char upper-char))
(def alphanumeric (choice lower-char upper-char digit))

(def lower-word (word "abcdefghijklmnopqrstuvwxyz"))
(def upper-word (word "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def capitalized (join (chain upper-char (opt lower-word))))
(def identifier (join (chain letter (zero-or-more alphanumeric))))


(defn pop-chars [n]
  "Read the next n characters and return them as result, joined into a single
  string."
  (fn [s]
    [[(clojure.string/join (take n s))] (drop n s)]))


(defn parse-int [[s]]
  "Parse the string in the vector into an integer."
  (Integer/parseInt s))


(def n-block (bind (fmap parse-int positive-integer) pop-chars))
(def n-blocks (one-or-more n-block))


(n-blocks "5hallo7ingbertrest")
;;  Ergebnis:  [["hallo" "ingbert"] ("r" "e" "s" "t")]
