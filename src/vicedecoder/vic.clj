(ns vicedecoder.vic)

(defn read-vic [snap offset]
  (let [reg-off (mod (- offset 0xd000) 0x40)]
    (cond (> reg-off 0x2f) 0xff
          (> reg-off 0x1f) (bit-or (get-in snap ["VIC-II" :registers reg-off]) 0xf0)
          :else (get-in snap ["VIC-II" :registers reg-off]))))

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
         (or (= descriptor 2)
             (= descriptor 3)))))

(defn multicolor-mode? [snap]
  (bit-test (read-vic snap 0xd016) 4))

(defn read-color-ram [snap offset]
  (if (>= offset 0xdbe8)
    0
    (get-in snap ["VIC-II" :color-ram (- offset 0xd800)])))

(defn border-color [snap]
  (bit-and (read-vic snap 0xd020) 0xf))

(defn bg-color [snap index]
  (bit-and (read-vic snap (+ 0xd021 index)) 0xf))

(defn sprite-multicolor1 [snap]
  (read-vic snap 0xd025))

(defn sprite-multicolor2 [snap]
  (read-vic snap 0xd026))

(defn sprite-color [snap sprite]
  (read-vic snap (+ 0xd027 sprite)))
