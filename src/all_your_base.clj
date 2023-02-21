(ns all-your-base)

(defn translate-to [number base]
   (loop [acc `()
          number number]
   (let [based (/ number base)] 
     (if (= based 0)
      (or  (not-empty acc) `(0))
      (recur (conj acc (rem number base))
             (int based))))))

(defn exp [number exponent]
  (reduce * (repeat exponent number)))

(defn translate-to-base-10 [list-of-numbers base]
    (->>
     list-of-numbers
     reverse
     vec
     (reduce-kv
      (fn [sum index elem]
        (+ sum (* elem (exp base index)))) 0)))

(defn invalid-input? [start-base list-of-numbers end-base]
  (let [invalid-start-base (>= 1 start-base)
        invalid-end-base (>= 1 end-base)
        invalid-list-of-numbers (some (fn [x] (or (> 0 x) (>= x start-base))) list-of-numbers)]
    (or invalid-start-base invalid-end-base invalid-list-of-numbers)))

(defn convert [start-base list-of-numbers end-base] 
  (if (invalid-input? start-base list-of-numbers end-base)
    nil
    (if (= list-of-numbers `())
      `()
      (-> 
       list-of-numbers
       (translate-to-base-10 start-base) 
       (translate-to end-base))))) 

