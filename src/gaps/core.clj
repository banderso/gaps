(ns gaps.core)

(defprotocol GapBuffer
  "A generic gap buffer protocol."
  (right [this] "Move the gap one element right.")
  (left  [this] "Move the gap one element left.")
  (insert [this c] "Insert an object to the left of the current gap.")
  (delete [this]   "Delete an object to the left of the current gap."))

(defrecord DoubleListBuffer [pos before after]
  GapBuffer
  (right [this]
    (let [before (:before this)
          after (:after this)]
      (if (nil? (first after))
        this
        (DoubleListBuffer. (inc (:pos this)) 
                           (conj before (first after))
                           (rest after)))))
  
  (left  [this]
    (let [before (:before this) 
          after (:after this)]
      (if (nil? (first before))
        this
        (DoubleListBuffer. (dec (:pos this))
                           (rest before)
                           (conj after (first before))))))
  
  (insert [this c]
    (let [before (:before this)
          after (:after this)]
      (DoubleListBuffer. (inc (:pos this))
                         (conj before c)
                         after)))
  
  (delete [this]
    (let [before (:before this)
          after (:after this)]
      (DoubleListBuffer. (dec (:pos this))
                  (rest before)
                  after))))

(defn gap-buffer [string]
  (DoubleListBuffer. 0 '() (seq string)))


