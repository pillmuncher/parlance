(ns parlance)


(defn fmap
  "Transform the result of parser p by applying function f."
  [f p]
  (fn [s]
    (let [[r s1] (p s)]
      [(f r) s1])))


(defn bind
  "Determine the parser to be invoked next by calling function f on the result
  of parser p (i.e. f must return a parser)."
  [p f]
  (fn [s]
    (let [[p1 s1] ((fmap f p) s)]
      (p1 s1))))


(defn return
  "Make value v the result.  Don't consume any input."
  [v]
  (fn [s]
    [v s]))


(def nothing
  "Return an empty result."
  (return []))


(def epsilon
  "Recognize an empty string."
  (return [""]))


(defn eoi
  "Recognize the end of the input string."
  [s]
  (if (empty? s)
    [[] ""]
    (throw (ex-info "trailing characters!"
                    {:type :parsing-error
                     :cause :trailing-characters}))))


(defn and-then
  "Invoke parsers p1 and p2 in sequence.  p2 starts off where p1 stopped."
  [p1 p2]
  (fn [s]
    (let [[r1 s1] (p1 s)
          [r2 s2] (p2 s1)]
      [(into r1 r2) s2])))


(defn or-else
  "Invoke parsers p1 and p2 alternatively.  If p1 fails, invoke p2 instead at
  the same point in input."
  [p1 p2]
  (fn [s]
    (try (p1 s)
         (catch clojure.lang.ExceptionInfo e
           (case (-> e ex-data :type)
             :parsing-error (p2 s)
             (throw e))))))


(defn chain
  "Invoke parsers p... in sequence.  Each one starts off where the previous
  one stopped."
  [px py & ps]
  (reduce and-then (and-then px py) ps))


(defn choice
  "Invoke parsers p... alternatively at the same point in input, until one
  returns a result.  This becomes the choice parsers result."
  [px py & ps]
  (reduce or-else (or-else px py) ps))


(defn zero-or-more
  "Invoke parser p repeatedly until failure.  Collect and return all results.
  If p never succeeds, return an empty result."
  [p]
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


(defn one-or-more
  "Invoke parser p repeatedly until failure.  Collect and return all results.
  If p never succeeds, fail."
  [p]
  (and-then p (zero-or-more p)))


(defn zero-or-one
  "Invoke parser p once, but ignore failure."
  [p]
  (or-else p nothing))


(def opt zero-or-one)


(defn join
  "Invoke parser p and join its result (a vector of strings) into a vector
  containing a single string."
  [p]
  (fmap (comp vector clojure.string/join) p))


(defn char
  "Parse a single character, if it occurs in cs."
  [cs]
  (let [cset (set cs)]
    (fn [s]
      (let [c (first s)]
        (if (contains? cset c)
          [[(str c)] (rest s)]
          (throw (ex-info (format "expected any of \"%s\", found \\%s." cs c)
                          {:type :parsing-error
                           :cause :excpected-character-not-found})))))))


(def word
  "Parse a consecutive word consisting of any of the characters in the
  argument.  The argument must be a single, non-empty string"
  (comp join one-or-more char))


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
(def alpha-char (choice lower-char upper-char))
(def alphanum-char (choice lower-char upper-char digit))

(def lower-word (word "abcdefghijklmnopqrstuvwxyz"))
(def upper-word (word "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def alpha-word (one-or-more alpha-char))
(def alphanum-word (one-or-more alphanum-char))

(def capitalized (join (chain upper-char (opt lower-word))))
(def identifier (join (chain alpha-char (opt alphanum-word))))


(defn pop-chars
  "Read the next n characters and return them as result, joined into a single
  string."
  [n]
  (fn [s]
    [[(clojure.string/join (take n s))] (drop n s)]))


(defn parse-int
  "Parse the string in the vector into an integer."
  [[s]]
  (Integer/parseInt s))


(def n-block (bind (fmap parse-int positive-integer) pop-chars))
(def n-blocks (one-or-more n-block))


(n-blocks "5hallo7ingbertrest")
;;  Ergebnis:  [["hallo" "ingbert"] ("r" "e" "s" "t")]
