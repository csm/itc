(ns itc.core
  (:refer-clojure :exclude [drop peek #?(:cljs -peek)])
  #?(:cljs (:require-macros itc.core)))

(def ^{:dynamic true :private true} *print-type* true)

(defprotocol INode
  (-leaf? [this])
  (-left [this])
  (-right [this]))

(defprotocol INormalize
  (-normalize [this]))

(defprotocol IJoin
  (-join [this that]))

(defprotocol IEvent
  (-value [this])
  (-lift [this n])
  (-drop [this n])
  (-min [this])
  (-max [this])
  (-max-depth [this] [this n]))

(defprotocol IId
  (-split [this])
  (-sum [this that])
  (-zero? [this])
  (-one? [this]))

(defprotocol IStamp
  (-fork [this])
  (-peek [this])
  (-get-id [this])
  (-get-event [this])
  (-event [this]))

(defprotocol ICmp
  (-leq [this that]))

(declare ->NodeEvent)
(declare ->LeafEvent)

(defrecord NodeEvent [value left right]
  INode
  (-leaf? [_] false)
  (-left [_] left)
  (-right [_] right)

  IEvent
  (-value [_] value)
  (-lift [_ n] (->NodeEvent (+ value n) left right))
  (-drop [_ n] (->NodeEvent (- value n) left right))
  (-min [_] (+ value (min (-min left) (-min right))))
  (-max [_] (+ value (max (-max left) (-max right))))
  (-max-depth [this] (-max-depth this 0))
  (-max-depth [this d] (max (-max-depth left (inc d))
                            (-max-depth right (inc d))))

  INormalize
  (-normalize [_]
    (if (and (-leaf? left) (-leaf? right) (= (-value left) (-value right)))
      (->LeafEvent (+ value (-value left)))
      (let [mn (min (-min left) (-min right))]
        (->NodeEvent (+ value mn) (-drop left mn) (-drop right mn)))))

  ICmp
  (-leq [this that]
    (if (-leaf? that)
      (and (<= value (-value that))
           (-leq (-lift left value) that)
           (-leq (-lift right value) that))
      (and (<= value (-value that))
           (-leq (-lift left value) (-lift (-left that) (-value that)))
           (-leq (-lift right value) (-lift (-right that) (-value that))))))

  IJoin
  (-join [this that]
    (cond
      (-leaf? that)
      (-join this (->NodeEvent (-value that) (->LeafEvent 0) (->LeafEvent 0)))

      (> value (-value that))
      (-join that this)

      :else
      (-normalize
        (->NodeEvent value
                     (-join left (-lift (-left that) (- (-value that) value)))
                     (-join right (-lift (-right that) (- (-value that) value))))))))

(defrecord LeafEvent [value]
  INode
  (-leaf? [_] true)
  (-left [_] nil)
  (-right [_] nil)

  IEvent
  (-value [_] value)
  (-lift [_ n] (->LeafEvent (+ value n)))
  (-drop [_ n] (->LeafEvent (- value n)))
  (-min [_] value)
  (-max [_] value)
  (-max-depth [this] (-max-depth this 0))
  (-max-depth [this n] n)

  INormalize
  (-normalize [this] this)

  ICmp
  (-leq [_ that] (<= value (-value that)))

  IJoin
  (-join [_ that]
    (if (-leaf? that)
      (->LeafEvent (max value (-value that)))
      (-join (->NodeEvent value (->LeafEvent 0) (->LeafEvent 0)) that))))

(declare ->NodeId)
(declare ->LeafId)

(defrecord NodeId [left right]
  INode
  (-leaf? [_] false)
  (-left [_] left)
  (-right [_] right)

  IId
  (-zero? [_] false)
  (-one? [_] false)
  (-split [_]
    (cond (-zero? left)
          (let [[r1 r2] (-split right)]
            [(->NodeId (->LeafId 0) r1)
             (->NodeId (->LeafId 0) r2)])

          (-zero? right)
          (let [[l1 l2] (-split left)]
            [(->NodeId l1 (->LeafId 0))
             (->NodeId l2 (->LeafId 0))])

          :else
          [(->NodeId left (->LeafId 0))
           (->NodeId (->LeafId 0) right)]))
  (-sum [this that]
    (cond (-zero? that)
          this

          (not (-leaf? that))
          (-normalize (->NodeId (-sum left (-left that)) (-sum right (-right that))))

          :else
          (throw (ex-info "Can't sum node with 1" {:this this}))))

  INormalize
  (-normalize [_]
    (let [norm-left (-normalize left)
          norm-right (-normalize right)]
      (cond (and (-zero? norm-left) (-zero? norm-right))
            (->LeafId 0)

            (and (-one? norm-left) (-one? norm-right))
            (->LeafId 1)

            :else
            (->NodeId norm-left norm-right)))))

(defrecord LeafId [value]
  INode
  (-leaf? [_] true)
  (-left [_] nil)
  (-right [_] nil)

  IId
  (-zero? [_] (zero? value))
  (-one? [_] (= 1 value))
  (-split [_]
    (if (zero? value)
      [(->LeafId 0) (->LeafId 0)]
      [(->NodeId (->LeafId 1) (->LeafId 0))
       (->NodeId (->LeafId 0) (->LeafId 1))]))
  (-sum [this that]
    (cond (-zero? this)
          that

          (-zero? that)
          this

          :else
          (throw (ex-info "Can't sum IDs" {:this this :that that}))))

  INormalize
  (-normalize [this] this))

(declare ->Stamp)

(defn- fill-event
  [id event]
  (cond (-leaf? id)
        (if (-zero? id)
          event
          (->LeafEvent (-max event)))

        (-leaf? event)
        event

        :else
        (cond (-one? (-left id))
              (let [filled-right (fill-event (-right id) (-right event))
                    mx (max (-max (-left event)) (-min filled-right))]
                (-normalize (->NodeEvent (-value event) (->LeafEvent mx) filled-right)))

              (-one? (-right id))
              (let [filled-left (fill-event (-left id) (-left event))
                    mx (max (-max (-right event)) (-min filled-left))]
                (-normalize (->NodeEvent (-value event) filled-left (->LeafEvent mx))))

              :else
              (->NodeEvent (-value event)
                           (fill-event (-left id) (-left event))
                           (fill-event (-right id) (-right event))))))

(defn- grow-event
  [id event]
  (if (-leaf? id)
    (if (and (-one? id) (-leaf? event))
      {:event (->LeafEvent (inc (-value event))) :cost 0}
      (throw (ex-info "can't grow event" {:id id :event event})))
    (cond (-leaf? event)
          (let [res (grow-event id (->NodeEvent (-value event) (->LeafEvent 0) (->LeafEvent 0)))]
            (update res :cost + (-max-depth event) 1))

          (-zero? (-left id))
          (let [right-grow (grow-event (-right id) (-right event))]
            {:event (->NodeEvent (-value event) (-left event) (:event right-grow))
             :cost (inc (:cost right-grow))})

          (-zero? (-right id))
          (let [left-grow (grow-event (-left id) (-left event))]
            {:event (->NodeEvent (-value event) (:event left-grow) (-right event))
             :cost (inc (:cost left-grow))})

          :else
          (let [left-grow (grow-event (-left id) (-left event))
                right-grow (grow-event (-right id) (-right event))]
            (if (< (:cost left-grow) (:cost right-grow))
              {:event (->NodeEvent (-value event) (:event left-grow) (-right event))
               :cost (inc (:cost left-grow))}
              {:event (->NodeEvent (-value event) (-left event) (:event right-grow))
               :cost (inc (:cost right-grow))})))))

(defrecord Stamp [id event]
  IStamp
  (-fork [_]
    (let [[id1 id2] (-split id)]
      [(->Stamp id1 event) (->Stamp id2 event)]))

  (-peek [_]
    [(->Stamp id event)
     (->Stamp (->LeafId 0) event)])

  (-get-id [_] id)
  (-get-event [_] event)

  (-event [_]
    (let [filled (fill-event id event)]
      (if (= event filled)
        (->Stamp id (:event (grow-event id event)))
        (->Stamp id filled))))

  ICmp
  (-leq [_ that]
    (-leq event (-get-event that)))

  IJoin
  (-join [_ that]
    (->Stamp (-sum id (-get-id that))
             (-join event (-get-event that))))

  #?(:clj Comparable)
  #?(:clj (compareTo [this that]
            (cond (-leq this that) -1
                  (-leq that this) 1
                  :else 0))))

#?(:clj
   (do
     (defmethod print-method LeafId
       [this w]
       (.write w (str (when *print-type* "#itc.core/id ") (pr-str (:value this)))))

     (defmethod print-method NodeId
       [this w]
       (.write w (str (when *print-type* "#itc.core/id ")
                      \( (binding [*print-type* false]
                           (pr-str (-left this)))
                      ", "
                      (binding [*print-type* false]
                        (pr-str (-right this)))
                      \))))

     (defmethod print-method LeafEvent
       [this w]
       (.write w (str
                   (when *print-type* "#itc.core/event ")
                   (pr-str (-value this)))))

     (defmethod print-method NodeEvent
       [this w]
       (.write w (str (when *print-type* "#itc.core/event ")
                      \(
                      (pr-str (-value this))
                      ", "
                      (binding [*print-type* false]
                        (pr-str (-left this)))
                      ", "
                      (binding [*print-type* false]
                        (pr-str (-right this)))
                      \))))

     (defmethod print-method Stamp
       [this w]
       (.write w (str "#itc.core/stamp ("
                      (binding [*print-type* false]
                        (pr-str (:id this)))
                      ", "
                      (binding [*print-type* false]
                        (pr-str (:event this)))
                      \)))))

   :cljs
   (extend-protocol IPrintWithWriter
     NodeEvent
     (-pr-writer [this writer _opts]
       (-write writer (str
                        (when *print-type* "#itc.core/event ")
                        \(
                        (pr-str (-value this))
                        ", "
                        (binding [*print-type* false]
                          (pr-str (-left this)))
                        ", "
                        (binding [*print-type* false]
                          (pr-str (-right this)))
                        \))))

     LeafEvent
     (-pr-writer [this writer _opts]
       (-write writer (str (when *print-type* "#itc.core/event ")
                           (pr-str (-value this)))))

     NodeId
     (-pr-writer [this writer _opts]
       (-write writer (str (when *print-type* "#itc.core/id ")
                           \(
                           (binding [*print-type* false]
                             (pr-str (-left this)))
                           ", "
                           (binding [*print-type* false]
                             (pr-str (-right this)))
                           \))))

     LeafId
     (-pr-writer [this writer _opts]
       (-write writer (str (when *print-type* "#itc.core/id ")
                           (pr-str (if (-zero? this) 0 1)))))

     Stamp
     (-pr-writer [this writer _opts]
       (-write writer (str "#itc.core/stamp ("
                           (binding [*print-type* false]
                             (pr-str (-get-id this)))
                           ", "
                           (binding [*print-type* false]
                             (pr-str (-get-event this)))
                           ")")))))

#?(:cljs
   (extend-protocol IComparable
     Stamp
     (-compare [this that]
       (cond (-leq this that) -1
             (-leq that this) 1
             :else 0))))

(defn ->id
  [form]
  (cond
    (and (seq? form) (= 2 (count form)))
    (let [[left right] form]
      (->NodeId (->id left) (->id right)))

    (#{0 1} form)
    (->LeafId form)

    :else
    (throw (ex-info "invalid ID, must be 0, 1 or pairs of IDs." {:value form}))))

(defn ->event
  [form]
  (cond
    (and (seq? form) (= 3 (count form)) (integer? (first form)))
    (let [[value left right] form]
      (->NodeEvent value (->event left) (->event right)))

    (integer? form)
    (->LeafEvent form)

    :else
    (throw (ex-info "invalid event, must be integer, or (value, left, right)" {:value form}))))

(defn ->stamp
  ([]
   (->Stamp (->LeafId 1) (->LeafEvent 0)))
  ([form]
   (let [[id event] form]
     (->Stamp (->id id) (->event event)))))

(defn fork
  [stamp]
  (-fork stamp))

(defn join
  [s1 s2]
  (-join s1 s2))

(defn event
  [stamp]
  (-event stamp))

(defn peek
  [stamp]
  (-peek stamp))