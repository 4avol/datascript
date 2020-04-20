(ns datascript.impl.sorted-set
    (:require [#?(:cljr clojure.core) :as c]
              [clojure.core.protocols :refer [CollReduce]])
    (:import [clojure.lang PersistentTreeSet]))

(defprotocol IPersistentSortedSet
    "有序集合接口。
    slice   对有序集合中大小在 [from to] 之间的元素顺序切片。
    rslice  对有序集合中大小在 [to from] 之间的元素逆序切片。"
    (slice [this from to] [this from to cmp])
    (rslice [this from to] [this from to cmp]))

;(defrecord PersistentSortedSet [sorted-set comparator])
(deftype PersistentSortedSet [^IPersistentCollection sorted-set comparator]
    clojure.lang.IPersistentCollection
    (count [this] (.count sorted-set))
    (cons [this x] (PersistentSortedSet. (.cons sorted-set x) comparator))
    (empty [this] (PersistentSortedSet. (.empty sorted-set) comparator))
    (equiv [this x] (.equiv sorted-set))

    clojure.lang.Seqable
    (seq [this] (seq sorted-set))
    )

(extend-type
    PersistentSortedSet
    IPersistentSortedSet
    (slice
     ([this from to] (slice this from to (.comparator this)))
     ([this from to cmp]
         (let [items (filter
                      (fn [v]
                          (and (or (nil? from) (>= 0 (cmp from v)))
                               (or (nil? to) (>= 0 (cmp v to)))))
                      this)]
             (when-not (empty? items)
                 (vec (apply sorted-set-by cmp items))))))
    (rslice
     ([this from to] (rslice this from to (.comparator this)))
     ([this from to cmp] nil))

    CollReduce
    (coll-reduce
     ([this f] (c/reduce f (.sorted-set this)))
     ([this f init] (c/reduce f init (.sorted-set this)))))

(defn sorted-set-by
    ([cmp]
     (->PersistentSortedSet (c/sorted-set) cmp))
    ([cmp & keys]
     (->PersistentSortedSet
      (into (c/sorted-set)
            (apply c/sorted-set-by cmp keys))
      cmp)))

(defn sorted-set
    ([] (sorted-set-by compare))
    ([& keys] (apply sorted-set-by compare keys)))

;(def sorted-set c/sorted-set)

(defn conj [set key comparator]
    (c/conj set key))

(defn disj [set key comparator]
    (c/disj set key))

(defn from-sorted-array
    [array])


#_(extend-type
      PersistentTreeSet
      IPersistentSortedSet
      (slice
       ([this from to]
           (slice this from to
                  (fn [x y]
                      (let [cmp (.comparator ^PersistentTreeSet this)]
                          (.Compare ^IComparer cmp x y)))))
       ([this from to cmp]
           (let [items (filter
                        (fn [v]
                            (and (or (nil? from) (>= 0 (cmp from v)))
                                 (or (nil? to) (>= 0 (cmp v to)))))
                        this)]
               (when-not (empty? items)
                   (vec (apply sorted-set-by cmp items))))))
      (rslice
       ([this from to] (rslice this from to c/compare))
       ([this from to cmp] nil)))
