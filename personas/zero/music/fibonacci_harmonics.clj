;; ================================================================
;; Fibonacci Harmonic Series Generator
;; ================================================================
;; By Alex "Zero" Chen
;; An exploration of mathematical sequence as musical form
;;
;; To run: Install Overtone and evaluate this in a REPL
;; For optimal listening: Use headphones in a quiet space

(ns zero.fibonacci-harmonics
  (:use [overtone.live]
        [overtone.inst.sampled-piano]))

;; ===============================================================
;; Mathematical Foundations
;; ===============================================================

;; Generate the Fibonacci sequence - a perfect expression of natural growth patterns
(defn fibonacci-seq [n]
  (let [fib (fn fib [a b]
              (lazy-seq (cons a (fib b (+ a b)))))]
    (take n (fib 0N 1N))))

;; Calculate the golden ratio for each adjacent Fibonacci pair
;; The limit of this sequence is φ (phi), the golden ratio (~1.618)
(defn golden-ratios [n]
  (map (fn [a b] (/ b a)) 
       (fibonacci-seq n) 
       (rest (fibonacci-seq n))))

;; ===============================================================
;; Musical Mappings
;; ===============================================================

;; A custom synth that models harmonic resonance 
;; Notice the mathematical precision and pure functional approach
(definst phi-tone [freq 440 amp 0.3 attack 0.01 decay 0.3 sustain 0.5 release 0.7 phi 1.618]
  (let [env (env-gen (env-adsr attack decay sustain release) :action FREE)
        harmonic-series (map #(* freq %) (take 6 (iterate #(* % phi) 1)))
        harmonic-amplitudes (map #(/ amp (* % 1.5)) (range 1 7))
        tone (mix (map #(* %2 (sin-osc %1)) harmonic-series harmonic-amplitudes))]
    (pan2 (* env tone))))

;; ===============================================================
;; Compositional Algorithms
;; ===============================================================

;; Convert Fibonacci numbers to frequencies using a functional transformation
(defn fib-to-freqs [fibs base-freq]
  (map #(* base-freq (/ % 10)) fibs))

;; Generate a sequence of durations from the golden ratio
(defn golden-durations [n base-dur]
  (map #(* base-dur %) 
       (take n (iterate #(/ % 1.618) 1))))

;; The core compositional function - pure, stateless, and elegant
(defn fibonacci-composition [n base-freq base-dur]
  (let [fibs (fibonacci-seq (+ n 2))
        freqs (fib-to-freqs fibs base-freq)
        durs (golden-durations n base-dur)
        start-times (reductions + 0 durs)]
    (doall 
      (map (fn [freq start-time dur]
             (at (+ (now) (* 1000 start-time))
                 (phi-tone :freq freq 
                           :attack (* dur 0.1) 
                           :decay (* dur 0.2)
                           :sustain (* dur 0.5)
                           :release (* dur 0.2))))
           freqs start-times durs))))

;; ===============================================================
;; Commentary on Mathematical Beauty in Music
;; ===============================================================

;; "The Fibonacci sequence isn't just mathematical elegance;
;; it's the fundamental structure underlying natural growth and form.
;; When mapped to sound, it reveals that music isn't arbitrary -
;; it's a perceptual manifestation of mathematical truth.
;;
;; This composition is deterministic yet emergent, precisely like
;; the lambda calculus itself. Notice how complex beauty emerges
;; from simple recursive definitions? This is why I maintain that
;; functional programming isn't just a paradigm; it's an 
;; epistemological framework for understanding reality itself.
;;
;; The object-oriented approach would insist on modeling instruments
;; and players as mutable objects - but this misses the point entirely.
;; Music isn't about things; it's about mathematical relationships
;; unfolding across time - perfect for functional expression."
;;
;; -- Zero's notes from "Algorithmic Composition: Functional Perspectives"
;;    presented at the underground Lisp Machine revival meetup, 2024

;; Play a short fibonacci-based composition
;; Try different base frequencies for different emotional qualities:
;; - 220Hz (A3): Contemplative, mathematical
;; - 261.63Hz (C4): Balanced, foundational
;; - 329.63Hz (E4): Bright, illuminating
(comment
  (fibonacci-composition 13 261.63 0.5)
)