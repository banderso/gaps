(ns gaps.core)

(defprotocol GapBuffer
  "A generic gap buffer protocol."
  (right [this] "Move the gap one element right.")
  (left  [this] "Move the gap one element left.")
  (insert [this c] "Insert an object to the left of the current gap.")
  (delete [this]   "Delete an object to the left of the current gap.")
  (getText [this] "Returns the contents as a string."))

(defprotocol GrowableBuffer
  "A buffer that can grow."
  (grow [this cap] "Grows the internal buffer to hold the given capacity."))

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
                  after)))

  (getText [this]
    (str (apply str (reverse before))
         (apply str after))))

(defn gap-buffer
  "Creates a double list based gap buffer."
  [string]
  (DoubleListBuffer. 0 '() (seq string)))


(def ^:const MAX_ARRAY_SIZE (int (- Integer/MAX_VALUE 8)))

(defn- hugeCap
  "Returns the maximum array size."
  ^long [^long minCap]
  (if (< minCap 0)
    (throw (java.lang.OutOfMemoryError.))
    (if (> minCap MAX_ARRAY_SIZE)
      Integer/MAX_VALUE
      MAX_ARRAY_SIZE)))

(defprotocol ArrayBufferInternal
  "Internal array buffer methods."
  (getBuf [this] "Returns the internal buffer.")
  (getSize [this] "Returns the size of the internal buffer.")
  (getPre [this] "Returns the location before the start of the gap.")
  (getPost [this] "Returns the location after the end of the gap.")
  
  (clength [this] "Returns the length of the cursor.")
  (tlength [this] "Returns the length of the text."))

(deftype ArrayBuffer [^{:tag chars :volatile-mutable true} buf
                      ^{:tag int :volatile-mutable true}   size
                      ^{:tag int :volatile-mutable true}   pre
                      ^{:tag int :volatile-mutable true}   post]
  
  ArrayBufferInternal
  (getBuf [this] buf)
  (getSize [this] size)
  (getPre [this] pre)
  (getPost [this] post)

  (clength [this]
    (if (= post pre)
      0
      (- (dec post) pre)))

  (tlength [this]
    (- (alength buf) (clength this)))
  
  GrowableBuffer
  (grow [this cap]
    (let [oldCap (alength buf)
          newCap (+ oldCap (bit-shift-right oldCap 1))
          newCap (if (< (- newCap cap) 0) cap newCap)
          newCap (if (> (- newCap MAX_ARRAY_SIZE) 0)
                   (hugeCap cap)
                   newCap)
          newBuf (char-array newCap)]
      (do (loop [i 0]
            (when (<= i pre)
              (do (aset newBuf i (aget buf i))
                  (recur (inc i)))))
          (set! post (int (if (= post size)
                            newCap
                            (loop [i (dec size)
                                   j (dec newCap)]
                              (if (>= i post)
                                (do (aset newBuf j (aget buf i))
                                    (recur (dec i) (dec j)))
                                (inc j))))))
          (set! buf newBuf)
          (set! size (int newCap))
          this)))
  
  GapBuffer
  (right [this]
    (if (< post (alength buf))
      (let [npre (inc pre)
            npost (inc post)]
        (do (aset-char buf npre (aget buf post))
            (set! pre (int npre))
            (set! post (int npost))
            this))
      this))
  
  (left  [this]
    (if (> pre -1)
      (let [npre (dec pre)
            npost (dec post)]
        (do (aset-char buf npost (aget buf pre))
            (set! post (int npost))
            (set! pre (int npre))
            this))
      this))
  
  (insert [this c]
    (let [this (if (<= (clength this) 0)
                 (grow this (inc size))
                 this)
          npre (inc pre)]
      (do (aset-char buf npre c)
          (set! pre (int npre))
          this)))
  
  (delete [this]
    (do (when (> pre 0) (set! pre (int (dec pre))))
        this))

  (getText [this]
    (let [ca (char-array (tlength this))]
      (do (loop [i 0]
            (when (<= i pre)
              (aset-char ca i (aget buf i))
              (recur (inc i))))
          (loop [i post
                 j (inc pre)]
            (when (< i (alength buf))
              (aset-char ca j (aget buf i))
              (recur (inc i) (inc j))))
          (String. ca)))))

(defn array-gap-buffer
  "Create an array based gap buffer."
  ([] (ArrayBuffer. (char-array 10) (int 10) (int -1) (int 10)))
  ([string]
      (let [size (count string)
            size (if (zero? size) 10 size)
            ca (char-array size)
            ab (ArrayBuffer. ca (int size) (int -1) (int size))]
        (loop [i 0]
          (if (= i size)
            ab
            (do (insert ab (nth string i))
                (recur (inc i))))))))
