(ns parlance.core)  ;; namespace festlegen


(defn return [v]
  "Make value v the parsing result.  Don't consume any input."
  (fn [s]
    [[v] s]))


(defn bind [p f]
  "Determine the parser to be used next by calling function f on the parsing
  result of parser p (i.e.  f must return a parser)."
  (fn [s]
    (let [[r1 s1] (p s)
          p1 (apply f r1)]
      (p1 s1))))


(defn empty [s]
  "Recognize an empty string."
    [[] s])


(defn epsilon [s]
  "Recognize the end of the input string."
  (if (= s "")
    [[] ""]
    (throw (java.lang.Exception "trailing characters!"))))


(defn action [f p]
  "Transform the parsing result of parser p by applying function f."
  (fn [s]
    (let [[r s1] (p s)]
      [[(apply f r)] s1])))


(defn ignore [p]
  "Apply parser p but ignore its parsig result."
  (fn [s]
    (let [[r s1] (p s)]
      [[] s1])))


(defn and-then[p1 p2]
  "Apply parsers p1 and p2 in sequence.  p2 starts off where p1 stopped."
  (fn [s]
    (let [[r1 s1] (p1 s)
          [r2 s2] (p2 s1)]
      [(into r1 r2) s2])))


(defn or-else[p1 p2]
  "Apply parsers p1 and p2 alternatively.  If p1 fails, apply p2 instead from
   the same point in input."
  (fn [s]
    (try
      (p1 s)
      (catch java.lang.Exception e
        (p2 s)))))


(defn chain [px py & ps]
  "Apply parsers p...  in sequence.  Each one starts off where the previous
   one stopped."
  (reduce and-then (and-then px py) ps))


(defn alt [px py & ps]
  "Apply parsers p...  alternatively from the same point in input, until one
  returns a parsing result.  This becomes the alt parsers result."
  (reduce or-else (or-else px py) ps))


(defn zero-or-more [p]
  "Gather all parsing results of repeatedly applying parser p while consuming
   input.  If p never succeeds return an empty parsing result."
  (defn helper [acc s]
    (try
      (let [[acc1 s1] (p s)]
        (helper (into acc acc1) s1))
      (catch java.lang.Exception e
        [acc s])))
  (fn [s]
    (helper [] s)))


(defn one-or-more [p]
  "Gather all parsing results of repeatedly applying parser p.  p must succeed
  at least once, otherwise fail."
  (and-then p (zero-or-more p)))


(defn zero-or-once [p]
  "Apply parser p, but ignore failure."
  (or-else p empty))


(def opt zero-or-once)


(defn char [c]
  "Parse the caracter c."
  (fn [s]
    (if (= c (first s))
      [[(str c)] (rest s)]
      (throw
        (java.lang.Exception
          (format "expected %s, found %s!" c (first s)))))))


(defn word [cs]
  "Parse a consecutive word consisting of any characters in cs."
  (action str (one-or-more (reduce or-else (map char cs)))))


(def lower-word (word "abcdefghijklmnopqrstuvwxyz"))
(def integer (word "1234567890"))
(def decimal (action str (chain integer (char \.) integer)))


(defn pop-chars [n]
  "Read the next n characters and return them as parsing result.  Advance
   input pointer by n."
  (fn [s]
    [[(apply str (take n s))] (drop n s)]))


(def n-block (bind (action #(Integer/parseInt %) integer) pop-chars))
(def n-blocks (one-or-more n-block))


(n-blocks "5hallo7ingbertrest")
;;  Ergebnis:  [["hallo" "ingbert"] (\r \e \s \t)]
