(ns vicedecoder.core
  (:require [clojure.string :as str]))

(defn- read-content [file-path]
  (let [f (java.io.File. file-path)
        ary (byte-array (.length f))
        is (java.io.FileInputStream. f)]
    (.read is ary)
    (.close is)
    (vec (map #(Byte/toUnsignedInt %) ary))))

(defn- read-byte [snap pos]
  (get snap pos)
  )

(defn- read-word [snap pos]
  (+ (read-byte snap pos)
     (* 256 (read-byte snap (inc pos)))))

(defn- read-dword [snap pos]
(+ (read-word snap pos)
     (* 65536 (read-word snap (+ 2 pos)))))

(defn- read-string
  ([snap pos]
   (apply str (map char (take-while pos? (subvec snap pos)))))
  ([snap pos len]
   (apply str (map char  (subvec snap pos (+ pos len))))))

(defn- read-header [snap]
  {:magic (read-string snap 0 19)
   :major (read-byte snap 19)
   :minor (read-byte snap 20)
   :machine (read-string snap 21)
   :vice-version (read-byte snap 50)
   }
  )

(defn- module-info [snap]
  {:module-type (read-string snap 0)
   :major (read-byte snap 16)
   :minor (read-byte snap 17)
   })

(defn- read-properties [snap props offset]
  (loop [props props
         offset offset
         acc {}]
         (if (empty? props)
           acc
           (let [current-def (first props)
                 [prop-key prop-type item-count] current-def
                 read-fn (case prop-type
                           :byte read-byte
                           :word read-word
                           :dword read-dword)
                 item-size (case prop-type
                                :byte 1
                                :word 2
                                :dword 4)
                 mem-size (* (or item-count 1)
                              item-size)
                 ]
             (recur (rest props)
                    (+ offset mem-size)
                    (assoc acc prop-key
                           (if (and item-count (> item-count 1))
                             (vec (map #(read-fn snap (+ offset (* % item-size)))
                                   (range item-count)))
                             (read-fn snap offset))
                           ))))))

(defmulti read-module #(read-string % 0))

(defmethod read-module "C64MEM" [snap]
  (assoc (module-info snap)
         :cpu-data (read-byte snap 22)
         :cpu-dir (read-byte snap 23)
         :exrom (read-byte snap 24)
         :game (read-byte snap 25)
         :memory (subvec snap 26)
  ))

(defmethod read-module "C64ROM" [snap]
  (merge (module-info snap)
         (read-properties snap [[:kernal :byte 8192]
                                [:basic :byte 8192]
                                [:chargen :byte 4096]]
                          22)
  ))

(defmethod read-module "MAINCPU" [snap]
  (merge (module-info snap)
         (read-properties snap
                          [[:clk :dword]
                           [:ac :byte]
                           [:xr :byte]
                           [:yr :byte]
                           [:sp :byte]
                           [:pc :word]
                           [:st :byte]
                           [:last-op :dword]
                           [:irq-clk :dword]
                           [:nmi-clk :dword]]
                          22)))


(defn- read-6526
  [snap]
  (merge (module-info snap)
         {:memory (subvec snap 22)}
         (read-properties snap [[:pra :byte]
                                [:prb :byte]
                                [:ddra :byte]
                                [:ddrb :byte]
                                [:tac :word]
                                [:tbc :word]
                                [:tod-ten :byte]
                                [:tod-sec :byte]
                                [:tod-min :byte]
                                [:tod-hr :byte]
                                [:sdr :byte]
                                [:ier :byte]
                                [:cra :byte]
                                [:crb :byte]
                                [:tal :word]
                                [:tbl :word]
                                [:ifr :byte]
                                [:pbstate :byte]
                                [:srhbits :byte]
                                [:alarm-ten :byte]
                                [:alarm-sec :byte]
                                [:alarm-min :byte]
                                [:alarm-hr :byte]
                                [:readicr :byte]
                                [:todlatched :byte]
                                [:todl-ten :byte]
                                [:todl-sec :byte]
                                [:todl-min :byte]
                                [:todl-hr :byte]
                                [:tod-ticks :dword]
                                [:tastate :word]
                                [:tbstat :word]]
                          22)))

(defmethod read-module "CIA1" [snap]
  (read-6526 snap))

(defmethod read-module "CIA2" [snap]
  (read-6526 snap))

(defmethod read-module "VIC-II" [snap]
  (into (sorted-map)
        (merge (module-info snap)
         (read-properties snap [
                                [:allow-bad-lines :byte 1]
                                [:bad-line :byte 1]
                                [:blank :byte 1]
                                [:color-buf :byte 40]
                                [:color-ram :byte 1024]
                                [:idle-state :byte 1]
                                [:lp-trigger :byte 1]
                                [:lpx :byte 1]
                                [:lpy :byte 1]
                                [:matrix-buf :byte 40]
                                [:new-sprite-dma-mask :byte 1]

                                [:ram-base :dword 1]
                                [:raster-cycle :byte 1]
                                [:raster-line :word 1]
                                [:registers :byte 64]
                                [:sb-coll-mask :byte 1]
                                [:sprite-dma-mask :byte 1]
                                [:ss-coll-mask :byte 1]
                                [:vbank :byte 1]
                                [:vc :word 1]
                                [:vc-add :byte 1]

                                [:vc-base :word 1]
                                [:video-int :byte 1]


                                [:sprite-xmem-ptr1 :byte 1]
                                [:sprite-xmem-ptr-inc1 :byte 1]
                                [:sprite-xexp-flip-flop1 :byte 1]

                                [:sprite-xmem-ptr2 :byte 1]
                                [:sprite-xmem-ptr-inc2 :byte 1]
                                [:sprite-xexp-flip-flop2 :byte 1]

                                [:sprite-xmem-ptr3 :byte 1]
                                [:sprite-xmem-ptr-inc3 :byte 1]
                                [:sprite-xexp-flip-flop3 :byte 1]

                                [:sprite-xmem-ptr4 :byte 1]
                                [:sprite-xmem-ptr-inc4 :byte 1]
                                [:sprite-xexp-flip-flop4 :byte 1]

                                [:sprite-xmem-ptr5 :byte 1]
                                [:sprite-xmem-ptr-inc5 :byte 1]
                                [:sprite-xexp-flip-flop5 :byte 1]

                                [:sprite-xmem-ptr6 :byte 1]
                                [:sprite-xmem-ptr-inc6 :byte 1]
                                [:sprite-xexp-flip-flop6 :byte 1]

                                [:sprite-xmem-ptr7 :byte 1]
                                [:sprite-xmem-ptr-inc7 :byte 1]
                                [:sprite-xexp-flip-flop7 :byte 1]

                                [:sprite-xmem-ptr8 :byte 1]
                                [:sprite-xmem-ptr-inc8 :byte 1]
                                [:sprite-xexp-flip-flop8 :byte 1]


                                [:fetch-event-tick :dword 1]
                                [:fetch-event-type :byte 1]] 22))))



(defmethod read-module :default [snap]
  (module-info snap)
  )

(defn read-modules [snap-content]
  (loop [snap snap-content
         acc []
         ]
    (if (empty? snap)
      acc
      (let [module (read-module snap)
            module-size (read-dword snap 18)]
        (recur (subvec snap module-size) (conj acc module))
        )
        )))

(defn hex-dump
  ([mem byte-count bytes-per-line offset]
  (str/join "\n"
  (map-indexed (fn [index line]
                 (str (format "%04x" (+ offset (* index bytes-per-line)))
                      " "
                      (str/join  " " (map #(format "%02x" %) line)))
                 )
               (partition-all bytes-per-line (subvec mem 0 (min (count mem) byte-count))
                              ))))
  ([mem count]
   (hex-dump mem count 8 0)))


(defn read-cia1 [snap offset]
    (get-in snap ["CIA1" :memory (mod (- offset 0xdc00) 0x10)]))

(defn read-cia2 [snap offset]
    (get-in snap ["CIA2" :memory (mod (- offset 0xdd00) 0x10)]))

(defn read-vic [snap offset]
  (let [reg-off (mod (- offset 0xd000) 0x40)]
    (cond (> reg-off 0x2f) 0xff
          (> reg-off 0x1f) (bit-or (get-in snap ["VIC-II" :registers reg-off]) 0xf0)
          :else (get-in snap ["VIC-II" :registers reg-off]))))

(defn read-color-ram [snap offset]
  (if (>= offset 0xdbe8)
    0
    (get-in snap ["VIC-II" :color-ram (- offset 0xd800)])))

(defn read-zero-page [snap offset]
  (case offset
    0 (get-in snap ["C64MEM" :cpu-dir])
    1 (get-in snap ["C64MEM" :cpu-data])
    (get-in snap ["C64MEM" :memory offset])
    )
  )

(defn- basic-rom-enabled? [snap]
  (pos? (and (get-in snap ["C64MEM" :cpu-data]) 0x1)))

(defn- kernal-rom-enabled? [snap]
  (pos? (and (get-in snap ["C64MEM" :cpu-data]) 0x2)))

(defn- chargen-rom-enabled? [snap]
  (zero? (and (get-in snap ["C64MEM" :cpu-data]) 0x4)))

(defn- vic-bank
  [snap]
  (- 3 (bit-and (get-in snap ["CIA2" :pra]) 3)))

(defn- vic-base [snap]
  (* 0x4000 (vic-bank snap)))

(defn vic-mem [snap]
  (+ (vic-base snap) (* 64 (bit-and (read-vic snap 0xd018) 0xf0))))

(defn char-mem [snap]
  (+ (vic-base snap) (* 1024 (bit-and (read-vic snap 0xd018) 0x0e))))

(defn charset-from-rom? [snap]
  (let [descriptor (/ (bit-and (read-vic snap 0xd018) 0x0e) 2)
        bank (vic-bank snap)]
    (and (or (= bank 0)
             (= bank 2))
         (or (= descriptor 3)
             (= descriptor 4)))))

(defn read-mem
  ([snap offset]
  (condp > offset
    0x100  (read-zero-page snap offset)
    0x8000 (get-in snap ["C64MEM" :memory offset])
    0xa000 (get-in snap ["C64MEM" :memory offset])
    0xc000 (if (basic-rom-enabled? snap)
             (get-in snap ["C64ROM" :basic (- offset 0xa000)])
             (get-in snap ["C64MEM" :memory offset]))
    0xd000 (get-in snap ["C64MEM" :memory offset])
    0xd400 (if (chargen-rom-enabled? snap)
             (get-in snap ["C64ROM" :chargen (- offset 0xd000)])
             (read-vic snap offset))
    0xd800 (if (chargen-rom-enabled? snap)
             (get-in snap ["C64ROM" :chargen (- offset 0xd000)])
             (get-in snap ["C64MEM" :memory offset]))
    0xdc00 (read-color-ram snap offset)
    0xdd00 (read-cia1 snap offset)
    0xde00 (read-cia2 snap offset)
    0xdf00 (get-in snap ["C64MEM" :memory offset])
    0xe000 (get-in snap ["C64MEM" :memory offset])
    (if (kernal-rom-enabled? snap)
      (get-in snap ["C64ROM" :kernal (- offset 0xe000)])
      (get-in snap ["C64MEM" :memory offset]))
    ))
  ([snap offset byte-count]
   (vec (map (partial read-mem snap) (range offset (+ offset byte-count))))))

(comment
 (let [modules (read-modules (subvec (read-content "basic.vsf") 58))
       by-type (zipmap (map :module-type modules) modules)]
   (hex-dump (subvec (get-in by-type ["C64MEM" :memory]) 53272) 64 16 53272)
   by-type
   (hex-dump (read-mem by-type 0xd000 0x100)
             0x100 16 0xd000)
   )
 )
