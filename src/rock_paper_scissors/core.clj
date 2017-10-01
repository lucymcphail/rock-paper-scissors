(ns rock-paper-scissors.core
  (:gen-class))
       
(def items
  [{:name "rock"     :beats "scissors" :loses "paper"    :likelihood 1}   
   {:name "paper"    :beats "rock"     :loses "scissors" :likelihood 1}
   {:name "scissors" :beats "paper"    :loses "rock"     :likelihood 1}])

(def item-index
  {"rock"     0
   "paper"    1
   "scissors" 2})

(defn clear
  "Clear the terminal window"
  []
  (print (str (char 27) "[2J"))
  (print (str (char 27) "[;H")))

(defn take-input
  "Take an input from the user"
  [x]
  (let [input (clojure.string/lower-case x)]
    (cond
      (= input "r") "rock"
      (= input "p") "paper"
      (= input "s") "scissors"
      :else input)))

(defn computer-play
  "Choose whether to play rock, paper or scissors"
  ([]
   (let [sum (reduce + (map :likelihood items))
         choice (rand sum)]
     (loop [[item & remaining] items
            accumulated-likelihood (:likelihood item)]
       (if (> accumulated-likelihood choice)
         (:name item)
         (recur remaining (+ accumulated-likelihood (:likelihood item)))))))
  ([prev result]
   (if (= (rand-int 4) 0)
     (computer-play)
     (cond
       (= result "win")  (:beats (get items (get item-index prev)))  
       (= result "loss") (:loses (get items (get item-index prev)))
       :else (computer-play)))))

(defn find-winner
  "Find out the winner of a round, from the perspective of player1"
  [player1 player2]
  (cond
    (= (:beats (get items (get item-index player1))) player2) "win"
    (= (:loses (get items (get item-index player1))) player2) "loss"
    (= player1 player2) "draw"
    :else nil))

(defn inc-likelihood
  "Increment the likelihood of an item"
  [item items]
  (map #(if (= (:name %) item)
          (assoc %
                 :likelihood (inc (:likelihood %)))
          %)
       items))

(defn round
  "Play a round of Rock, Paper, Scissors"
  [player-score
   computer-score
   ai-input]
  (println "brucekly's rock, paper, scissors")
  (println "Type in r (rock), p (paper) or s (scissors)")
  (println "")
  (println (str "Your score: " player-score ", Computer score: " computer-score))
  (println "")
  (let [player-choice (take-input (read-line))
        computer-choice (apply computer-play ai-input)
        result (find-winner player-choice computer-choice)]
    (clear)
    (println (str "Your choice: " player-choice))
    (println (str "Computer choice: " computer-choice))
    (println (str "Result: " result))
    (println "")
    (vec [player-choice result computer-choice])))

(defn -main
  "Run the main game loop"
  [& args]
  (clear)
  (loop [player-score 0
         computer-score 0
         ai-input [nil, nil]
         items [{:name "rock"     :beats "scissors" :loses "paper"    :likelihood 1}   
                {:name "paper"    :beats "rock"     :loses "scissors" :likelihood 1}
                {:name "scissors" :beats "paper"    :loses "rock"     :likelihood 1}]]
    (let [[player-choice & round-output] (round player-score computer-score ai-input)]
      (if (nth round-output 0)
        (let [result (nth round-output 0)
              weighted-items (inc-likelihood (get items (get item-index player-choice)) items)]
          (cond
            (= result "win")
            (recur (inc player-score) computer-score round-output items)
            (= result "loss")
            (recur player-score (inc computer-score) round-output items)
            :else
            (recur player-score computer-score round-output items)))
        nil))))
 
