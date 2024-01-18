(ns trebl.core
  (:refer-clojure :exclude [key pop])
  (:require
    [clojure.string :as str])
  (:import [org.jline.terminal TerminalBuilder Terminal]))


(defn interleave-with-default [xs ys default-value]
  (loop [res []
         [x & xs] xs
         [y & ys] ys]
    (if (or x y)
      (recur (conj res [(or x default-value) (or y default-value)]) xs ys)
      res)))


(defn line-diffs [is was]
  (loop [col 0
         diffs []
         ongoing nil
         [[is was] & pairs] (interleave-with-default is was nil)]

    (if (or is was)
      (if (= is was)
        (if ongoing
          (recur (inc col) (conj diffs ongoing) nil pairs)
          (recur (inc col) diffs nil pairs))
        (if ongoing
          (recur (inc col) diffs (update ongoing :value conj is) pairs)
          (recur (inc col) diffs (conj {:col col :value [is]}) pairs)))
      (if ongoing
        (conj diffs ongoing)
        diffs))))


(defn diff [is was]
  (let [line-pairs (interleave-with-default is was nil)]

    (loop [row 0
           diffs []
           [pair & pairs] line-pairs]
      (if pair
        (let [[is was] pair]
          (if (= is was)
            (recur (inc row) diffs pairs)
            (recur
              (inc row)
              (concat
                diffs
                (map (fn [col-diff] (merge {:row row} col-diff)) (line-diffs is was)))
              pairs)))
        diffs))))


(def ESC (str (char 27)))


(defn cursor-pos [row col]
  (str ESC "[" (inc row) ";" (inc col) "H"))


(defn clear-screen []
  (str ESC "[2J"))


(defn trace
  ([row s]
   (print (cursor-pos row 1))
   (println (format "%-20s" s))
   (flush)))


(defonce ^:private last-screen (atom nil))


(defn diff-value->str [xs]
  (str/join (map (fn [c] (if (nil? c) " " c))  xs)))


(defn diff-to-ansi [diff]
  (str (cursor-pos (:row diff) (:col diff)) (diff-value->str (:value diff))))


(defn diffs-to-ansi [diffs]
  (map diff-to-ansi diffs))


(defn print-screen!
  "Prints a screen"
  [cursor-row screen]
  (let [screen-with-cursor
        (map-indexed
          (fn [index line]
            (let [cursor (if (= index cursor-row) ">" " ")]
              (format "%s %s" cursor line)))
          screen)

        diffs (diff screen-with-cursor @last-screen)
        ansi-statements (diffs-to-ansi diffs)]

      (doseq [ansi-statement ansi-statements]
        (print ansi-statement))

      (print (cursor-pos cursor-row 0))
      (flush)
      (reset! last-screen screen-with-cursor)))


(defn ->str [v max-width]
  (let [s (pr-str v)
        s (str/replace s #"\s" " ")
        width (min max-width (count s))]
    (subs s 0 width)))


(defn apply-viewer [v brief-viewers]
  (reduce
    (fn [v [pred viewer]]
      (if (pred v) (viewer v) v))
    v
    brief-viewers))


(defn screen-table [width kv-table brief-viewers]
  ;; guard that at leact one char of k and v fits
  (let [ks (map first kv-table)
        ;xs (map second kv-table)

        separator " | "
        separator-width (count separator)
        max-key-width (- width separator-width 3)

        key-width (apply max (map #(count (pr-str %)) ks))
        key-width (min max-key-width key-width)

        val-width (max 0 (- width key-width separator-width))
        format-str (str "%-" key-width "s"
                        "%-" separator-width "s"
                        "%-" val-width "s")]

    (for [[k v] kv-table]
      (let [key-str (-> k (->str key-width))
            val-str (-> v (apply-viewer brief-viewers) (->str val-width))]
        (format format-str key-str separator val-str)))))


(defn kv-table
  "Turns v into a seq of [k v] pairs.
  If v is a map, k will be the key.
  If v is something else seqable?, k will be the index.
  If v is a Throwable, nil is returned
  If v is not seqable?, nil is returned"
  [v]
  (cond
    (and (map? v) (sorted? v)) (map vector (keys v) (vals v))
    (map? v) (sort-by (fn [[k _v]] (str k)) (seq v))
    (seqable? v) (map vector (range) v)
    (instance? java.lang.Throwable v) (kv-table (Throwable->map v))))


(defn new-terminal
  "creates new JLine3 Terminal.
  returns terminal object"
  ^Terminal [term-name]
  (let [terminal (-> (TerminalBuilder/builder)
                     (.jna true)
                     (.system true)
                     (.name term-name)
                     (.build))]
    {:terminal terminal
     :reader (.reader terminal)
     ; :writer?
     :width (.getWidth terminal)
     :height (.getHeight terminal)}))


(defn key [state]
  (first (nth (:table state) (:index state))))


(defn value [state]
  (second (nth (:table state) (:index state))))


(defn new-state
  ([height width data]
   (let [table (kv-table data)
         index 0]
     {:height height
      :width width
      :data data
      :table table
      :index index
      :cursor-row index
      :top-row index})))


(defn trace-state [row state]
  (print (cursor-pos row 0))
  (println (format ":height      %5d" (:height state)))
  (println (format ":width       %5d" (:width state)))
  (println (format ":index       %5d" (:index state)))
  (println (format ":cursor-row  %5d" (:cursor-row state)))
  (println (format ":top-row     %5d" (:top-row state))))


(defn index [state] (:index state))
(defn data [state] (:data state))
(defn table [state] (:table state))


(defn set-index [state n] (assoc state :index n))


(defn pushable? [state]
  (let [v (value state)]
    (boolean
      (or
       (instance? Throwable v)
       (and (seqable? v) (seq v))))))


(defn push [state]
  (->
    (new-state (:height state) (:width state) (value state))
    (update :stack (fn [stack] (cons state stack)))))


(defn poppable? [stack]
  (boolean (seq stack)))


(defn pop [state]
  (:stack state))

(defn noop [state]
  state)


(defn cursor-top? [state]
  (= 0 (:cursor-row state)))


(defn cursor-bottom? [state]
  (= (dec (:height state)) (:cursor-row state)))


(defn index-top? [state]
  (= 0 (:index state)))


(defn index-bottom? [state]
  (= (dec (count (:table state))) (:index state)))


(defn cursor-down [state]
  (if (not (cursor-bottom? state))
    (update state :cursor-row inc)
    state))


(defn cursor-up [state]
  (if (not (cursor-top? state))
    (update state :cursor-row dec)
    state))


(defn index-down [state]
  (if-not (index-bottom? state)
    (update state :index inc)
    state))


(defn index-up [state]
  (if-not (index-top? state)
    (update state :index dec)
    state))


(defn top-row-down [state]
  (if (cursor-bottom? state)
    (update state :top-row inc)
    state))


(defn top-row-up [state]
  (if (cursor-top? state)
    (update state :top-row dec)
    state))


(defn go-down [state]
  (->
    state
    (top-row-down)
    (index-down)
    (cursor-down)))


(defn go-up [state]
  (->
    state
    (top-row-up)
    (index-up)
    (cursor-up)))


(defn down? [state]
  (not (index-bottom? state)))


(defn up? [state]
  (not (index-top? state)))


#_(defn down [state]
    (if (down? state) (go-down state) (noop state)))


(defn down
  ([state]
   (down 1 state))

  ([n state]
   (reduce
     (fn [state _n]
       (if (down? state) (go-down state) (noop state)))
     state
     (range n))))


(defn up
  ([state]
   (up 1 state))

  ([n state]
   (reduce
     (fn [state _n]
       (if (up? state) (go-up state) (noop state)))
     state
     (range n))))


(defn left [state]
  (if (poppable? state)
    (pop state)
    state))


(defn right [state]
  (if (pushable? state)
    (push state)
    state))


(defn path [state]
  (->>
   (:stack state)
   (cons state)
   (map key)))


(defn help []
  {:j :down
   :k :up
   :h :left
   :l :right
   :ctrl-d :page-down
   :ctrl-u :page-up
   :q :quit-and-return-current-value
   :? :help})


(defn execute-loop [terminal data options]
  (let [width (:width @terminal)
        height (- (:height @terminal) 1)
        ;; trace-row (+ 2 height)
        data-width (- width 2)  ; Leave some cols for the cursor
        reader (:reader @terminal)]
    (loop [state (new-state height data-width data)
           stack (list)]

      (let [data (:data state)
            ;path (map :cursor-row state-stack)

            table (->> data (kv-table) (drop (:top-row state)) (take (:height state)))
            brief-viewers (:brief-viewers options)
            s-table (screen-table data-width table brief-viewers)]

        ;; (trace-state trace-row state)
        (print-screen! (:cursor-row state) s-table)

        (let [char-int (.read reader)
              c (char char-int)]
          ;; (trace 60 (int c))
          (if (= \q c)
            (do
              (print (cursor-pos height 1))  ; Set cursor to bottom
              (:data state))                 ; Return the current data
            (let [ch (char char-int)]
              (cond
                (= \j ch) (recur (down state) stack)
                (= \k ch)  (recur (up state) stack)
                (= \h ch) (if (poppable? stack)
                            (recur (first stack) (rest stack))
                            (recur state stack))
                (= \l ch) (if (pushable? state)
                            (recur (right state) (cons state stack))
                            (recur state stack))

                (= 4 char-int)  (recur (down (quot height 2) state) stack)
                (= 21 char-int)  (recur (up (quot height 2) state) stack)

                (= \? ch) (recur (new-state height width (help)) (cons state stack))

                :else (recur state stack)))))))))


(defn close-terminal [terminal]
  (.close (:reader @terminal))
  (.close (:terminal @terminal)))


(defn trebl
  ([data]
   (trebl data {}))

  ([data options]
   (let [terminal (atom (new-terminal "term"))]
     (.enterRawMode (:terminal @terminal))
     (try
       (print (clear-screen))
       (reset! last-screen nil)
       (execute-loop terminal data options)
       (catch Exception e
         (println "Not good...")
         (println (.getMessage e))
         (.printStackTrace e))
       (finally
         (close-terminal terminal))))))


(def example-data
  {nil nil
   :chart (with-meta [1 2 3] {:rebl/xy-chart {:title "My Stuff"}})
   :code '(defn foo [x] "Hello World")
   :empty-map {}
   :empty-vec []
   :exception (try (/ 1 0) (catch Exception e e))
   :keyed-pairs {:a [[1 3] [-3 5]] :b [[4 8]]}
   :nested-map {:name "Jane Doe"
                :address1
                {:street "Main Street 1"
                 :city "New Orleans"}
                :address2
                {:street "Main Street 2"
                 :city "New York"}}
   :pairs [[1 2] [-3 5]]
   :powers [0 1 4 9 16 25 36]
   :scalar "Hello World"
   :tuples [[1 2] [3 4] [5 6]]
   :uber
   (map-indexed
     (fn [index m] (merge {:index index} m))
     (repeatedly 120 (fn [] {:alpha (rand-int 100)
                             :beta (rand-int 100)})))
   "string key" "string value"
   'symbol-key 'symbol-value
   {:map :key} {:map :value}})


(comment
  (def options
    {:brief-viewers  ; A vector of [pred mapper] tuples. First one wins, if any.
     [[string? clojure.string/reverse]
      [map? keys]]})

  (trebl example-data options)

  (trebl example-data)

  (trebl {4 :d 1 :a 2 :b 3 :c})
  (trebl (into (sorted-map-by #(> %1 %2)) {4 :d 1 :a 2 :b 3 :c}))

  (trebl (ns-publics 'trebl.core))

  (instance? java.lang.Throwable (:exception example-data))


  ;; Testing line-diffs
  (line-diffs "foobar is the very best" "foogar is the best")

  ;; Testing diff

  (defn make-screen [data]
    (screen-table 40 (kv-table data) {}))

  (def s1 (make-screen example-data))
  (def s2 (make-screen (assoc-in example-data [:powers 4] 160)))
  (def s3 (make-screen (assoc-in example-data [:scalar] "Hello världen!")))

  (diff s2 s1)
  (diff s3 s1)
  (diff s3 [])
  (diff [] s3))


