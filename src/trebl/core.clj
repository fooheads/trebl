(ns trebl.core
  (:refer-clojure :exclude [key pop])
  (:import [org.jline.terminal TerminalBuilder Terminal]))


(def ESC (str (char 27)))


(defn cursor-pos [row col]
  (str ESC "[" row ";" col "H"))


(defn clear-screen []
  (str ESC "[2J"))


(defn print-screen!
  "Prints a screen from pos (1,1)"
  [cursor-row screen]
  (print (clear-screen))
  (print (cursor-pos 1 1))

  (let [screen-with-cursor
        (map-indexed
          (fn [index line]
            (let [cursor (if (= (inc index) cursor-row)
                           ">"
                           " ")]
              (format "%s %s" cursor line)))
          screen)]

    (doseq [line screen-with-cursor]
      (println line))))


(defn ->str [v max-width]
  (let [c (pr-str v)
        width (min max-width (count c))]
    (subs (pr-str v) 0 width)))


(defn screen-table [width kv-table]
  ;; guard that at leact one char of k and v fits
  (let [ks (map first kv-table)
        ;xs (map second kv-table)

        separator " | "
        separator-width (count separator)
        max-key-width (- width separator-width 3)

        key-width (apply max (map #(count (str %)) ks))
        key-width (min max-key-width key-width)

        val-width (max 0 (- width key-width separator-width))
        format-str (str "%-" key-width "s"
                        "%-" separator-width "s"
                        "%-" val-width "s")]

    (for [[k v] kv-table]
      (let [key-str       (->str k key-width)
            val-str       (->str v val-width)]
        (format format-str key-str separator val-str)))))


(defn kv-table
  "Turns v into a seq of [k v] pairs.
  If v is a map, k will be the key.
  If v is something else seqable?, k will be the index.
  If v is not seqable?, nil is returned"
  [v]
  (cond
    (map? v) (sort (seq v))
    (seqable? v) (map vector (range) v)))


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


(defn new-state [data]
  (let [table (kv-table data)
        index 0]
    {:data data
     :table table
     :index index}))

(defn index [state] (:index state))
(defn data [state] (:data state))
(defn table [state] (:table state))


(defn set-index [state n] (assoc state :index n))


(defn pushable? [state]
  (seqable? (value state)))


(defn push [state]
  (->
    (new-state (value state))
    (update :stack (fn [stack] (cons state stack)))))


(defn poppable? [stack]
  (boolean (seq stack)))


(defn pop [state]
  (:stack state))

(defn noop [state]
  state)


(defn down? [state]
  (< (:index state) (dec (count (:table state)))))


(defn go-down [state]
  (-> state (update :index inc)))


(defn go-up [state]
  (update state :index dec))


(defn up? [state]
  (< 0 (:index state)))


(defn down [state]
  (if (down? state) (go-down state) (noop state)))


(defn up [state]
  (if (up? state) (go-up state) (noop state)))


(defn left [state]
  (if (poppable? state)
    (pop state)
    state))


(defn trace [s]
  (print (cursor-pos 20 1))
  (println s)
  (flush))


(defn right [state]
  (if (pushable? state)
    (push state)
    state))


(defn path [state]
  (->>
   (:stack state)
   (cons state)
   (map key)))


(defn execute-loop [terminal data]
  (let [width (:width @terminal)
        height (:height @terminal)
        data-width (- width 4)  ; Leave 2 cols for the cursor
        reader (:reader @terminal)]
    (loop [state (new-state data)
           stack (list)]

      (let [data (:data state)
            index (:index state)
            ;path (map :cursor-row state-stack)

            table (kv-table data)
            s-table (screen-table data-width table)]

        (print-screen! (inc index) s-table)

        ; Print status
        ; (print (cursor-pos 60 1))
        ; (println (format "%d | %s"
        ;                  (:index state)
        ;                  (pr-str (path state))))

        ; Top row
        (print (cursor-pos 1 1))

        (let [char-int (.read reader)
              c (char char-int)]
          (if (= \q c)
            (do
              (print (cursor-pos height 1))  ; Set cursor to bottom
              (:data state))                 ; Return the current data
            (case (char char-int)
              \j (recur (down state) stack)
              \k (recur (up state) stack)
              \h (if (poppable? stack)
                   (recur (first stack) (rest stack))
                   (recur state stack))
              \l (if (pushable? state)
                   (recur (right state) (cons state stack))
                   (recur state stack))

              (recur state stack))))))))


(defn close-terminal [terminal]
  (.close (:reader @terminal))
  (.close (:terminal @terminal)))


(defn trebl [data]
  (let [terminal (atom (new-terminal "term"))]
    (.enterRawMode (:terminal @terminal))
    (try
      (execute-loop terminal data)
      (catch Exception e
        (println "Not good...")
        (println (.getMessage e))
        (.printStackTrace e))
      (finally
        (close-terminal terminal)))))


(def example-data
  {:chart (with-meta [1 2 3] {:rebl/xy-chart {:title "My Stuff"}})
   :code '(defn foo [x] "Hello World")
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
   :uber (repeatedly 40 (fn [] {:alpha (rand-int 100)
                                :beta (rand-int 100)}))})


(comment
  (trebl example-data)
  (trebl (ns-publics 'trebl.core)))

