(ns vicedecoder.core
  (:require [clojure.string :as str]
            [vicedecoder.vic :as vic
             :refer
             [char-mem charset-from-rom? multicolor-mode? read-color-ram read-vic]])
  (:import java.awt.Color
           java.awt.image.BufferedImage
           javax.imageio.ImageIO
           java.io.File
           )
  )

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

(defn extract-characters [snap]
  (let [char-definitions (if (charset-from-rom? snap)
                           (get-in snap ["C64ROM" :chargen])
                           (read-mem snap (char-mem snap) 4096))]
        (vec (partition-all 8 char-definitions))))

(defn- char-mono-bit-set? [char-byte pos]
  (bit-test char-byte pos))

(defn- char-multi-bit-set? [char-byte pos]
  (or (bit-test char-byte (* 2 pos)))
      (bit-test char-byte (inc (* 2 pos))))

(defn char-to-ascii [multi-color char-def]
  (str/join "\n"
            (map (fn [line]
                   (apply str
                          (map #(if ((if multi-color char-multi-bit-set? char-mono-bit-set?)
                                     line %)
                                  "*"
                                  " ")
                               (range (if multi-color 3 7) -1 -1))))
                 char-def)))

(def color-lookup
  {0  (Color. 0 0 0)
   1  (Color. 0xff 0xff 0xff)
   2  (Color. 0x88 0x00 0x00)
   3  (Color. 0xaa 0xff 0xee)
   4  (Color. 0xcc 0x44 0xcc)
   5  (Color. 0x00 0xcc 0x55)
   6  (Color. 0x00 0x00 0xaa)
   7  (Color. 0xee 0xee 0x77)
   8  (Color. 0xdd 0x88 0x55)
   9  (Color. 0x66 0x44 0x00)
   10 (Color. 0xff 0x77 0x77)
   11 (Color. 0x33 0x33 0x33)
   12 (Color. 0x77 0x77 0x77)
   13 (Color. 0xaa 0xff 0x66)
   14 (Color. 0x00 0x88 0xff)
   15 (Color. 0xbb 0xbb 0xbb)
   })

(defn- paint-char
  (
   [g x y multi-color? colors scale char-def]
  (doall (map-indexed (fn [line char-byte]
                   (doseq [bit (if multi-color? (range 4) (range 8))]
                     (if multi-color?
                       (let [color-index (colors (bit-and (bit-shift-right char-byte (* 2 bit)) 0x3))
                             color (color-lookup color-index)]
                         (.setColor g color)
                         (.fillRect g
                                    (+ x (* 2 scale (- 3 bit)))
                                    (+ y (* scale line)) (* 2 scale) scale))
                       (let [color-index (colors (if (char-mono-bit-set? char-byte bit)
                                                   1 0))
                             color (color-lookup color-index
                                                 )]
                         (.setColor g color)
                         (.fillRect g (+ x (* scale (- 7 bit)))
                                    (+ y (* scale line)) scale scale)
                         )
                       )
                     )
                   )
                 char-def
                 )))
  ([g multi-color? colors scale char-def]
   (paint-char g 0 0 multi-color? colors scale char-def)))

(defn char-to-image [multi-color? colors scale char-def]
  (let [bi (BufferedImage. (* 8 scale) (* 8 scale) BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics bi)]
    (paint-char g multi-color? colors scale char-def)
    bi
    ))

(defn screen-shot [snap scale]
  (let [multi-color? (multicolor-mode? snap)
        base-colors (if multi-color?
                      [(vic/bg-color snap 0)
                       (vic/bg-color snap 1)
                       (vic/bg-color snap 2)]
                      [(vic/bg-color snap 0)])
        chars-rom   (extract-characters snap)
        bi          (BufferedImage. (* 40 8 scale) (* 8 25 scale) BufferedImage/TYPE_INT_ARGB)
        g           (.createGraphics bi)
        screen      (vic/vic-mem snap)
        ]
    (doseq [row (range 25)]
      (doseq [col (range 40)]
        (let [offset        (+ (* 40 row) col)
              char-at-point (read-mem snap  (+ screen offset))
              char-def      (get chars-rom char-at-point)
              color         (vic/read-color-ram snap (+ 0xd800 offset))
              x-pos         (* col 8 scale)
              y-pos         (* row 8 scale)

              ]
          (paint-char g x-pos y-pos multi-color? (conj base-colors color) scale char-def)
          )
        ))
    bi))


(comment
  (let [modules (read-modules (subvec (read-content "basic.vsf") 58))
       snap (zipmap (map :module-type modules) modules)]
   (str/join "\n--------\n" (map (partial char-to-ascii (multicolor-mode? snap))
                                 (extract-characters snap)))
   ;; (doall
   ;;  (map-indexed #(ImageIO/write (char-to-image
   ;;                                (vic/multicolor-mode? snap)
   ;;                                [(vic/bg-color snap 0)
   ;;                                (vic/bg-color snap 1)
   ;;                                (vic/bg-color snap 2)
   ;;                                (vic/bg-color snap 3)]
   ;;                                40
   ;;                                %2)
   ;;                               "png"
   ;;                               (File. (format "/tmp/testchar%03d.png" %1)))
   ;;               (extract-characters snap)))
   (ImageIO/write (screen-shot snap 4) "png" (File. "/tmp/screen.png"))
   )
 )
