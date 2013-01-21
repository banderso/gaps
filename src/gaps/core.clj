(ns gaps.core)

(defrecord GapBuffer [pos before after])

(defn gap-buffer [string]
  (GapBuffer. 0 '() (seq string)))

(defn right [gbuf]
  (let [before (:before gbuf)
        after (:after gbuf)]
    (if (nil? (first after))
      gbuf
      (GapBuffer. (inc (:pos gbuf)) 
                  (conj before (first after))
                  (rest after)))))

(defn left [gbuf]
  (let [before (:before gbuf) 
        after (:after gbuf)]
    (if (nil? (first before))
      gbuf
      (GapBuffer. (dec (:pos gbuf))
                  (rest before)
                  (conj after (first before))))))

(defn insert [gbuf c]
  (let [before (:before gbuf)
        after (:after gbuf)]
    (GapBuffer. (inc (:pos gbuf))
                (conj before c)
                after)))

(defn delete [gbuf]
  (let [before (:before gbuf)
        after (:after gbuf)]
    (GapBuffer. (dec (:pos gbuf))
                (rest before)
                after)))
