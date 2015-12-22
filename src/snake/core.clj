;; A simple snake game in Clojure created to kill time in
;; the long flight from NYC to SF on 22 Dec 2015.
;; Adds a few minor features to the base implementation
;; provided in the "Programming Clojure 2nd Ed by Stuart Hollaway"

(ns snake.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener))
  (:use snake.import-static)
  (:gen-class))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

(def width 50)
(def height 30)      ;; dimensions of the canvas
(def point-size 10)  ;; the size of the point
(def turn-millis 60) ;; the heartbeat of the game
(def win-length 10)  ;; turns it take to win

(def LEFT  [-1  0])
(def RIGHT [ 1  0])
(def UP    [ 0 -1])
(def DOWN  [ 0  1])

;; directions of movement
(def dirs {VK_LEFT  [-1  0]
           VK_RIGHT [ 1  0]
           VK_UP    [ 0 -1]
           VK_DOWN  [ 0  1]})

(defn add-points
  [& pts]
  "add a series of points [x y] pairs"
  (vec (apply map + pts)))

(defn point-to-screen-rect
  [pt]
  "convert a point to a rect [x y width height]"
  (map #(* point-size %)
       [(pt 0) (pt 1) 1 1]))

(defn create-apple []
  "a single point on the game screen"
  {:location [(rand-int width) (rand-int height)]
   :color (Color. 210 50 90)
   :type :apple})

(defn create-snake []
  "a list of points pointing in a direction"
  {:body (list [1 1])
   :dir (dirs VK_RIGHT)
   :type :snake
   :color (Color. 15 160 70)})

(defn move
  "moves a snake. takes an optional :grow to grow the snake"
  [{:keys [body dir] :as snake} & grow]
  (assoc snake :body (cons (add-points (first body) dir)
                           (if grow body (butlast body)))))
(defn win?
  [{body :body}]
  "determines if the game has been won"
  (>= (count body) win-length))

(defn head-overlaps-body?
  "checks if the head overlaps the body"
  [{body :body}]
  (let [head (first body)
        body (rest body)]
    (contains? (set body) head)))

(defn has-touched-end?
  "checks if the snake has collided with a canvas boundary"
  [{[snake-head] :body}]
  (let [[x y] snake-head]
    (or (= x (dec 0))
        (= y (dec 0))
        (= x (inc width))
        (= y (inc height)))))

(defn lose?
  "determines when the game loses"
  [snake]
  (or (head-overlaps-body? snake)
      (has-touched-end? snake)))

(defn eats?
  "determines if a snake eats an apple"
  [{[snake-head] :body} {apple :location}]
  (= snake-head apple))

(defn turn
  "turns the snake in the new dir"
  [snake newdir]
  (assoc snake :dir newdir))

;;;;;;;;;;;;;;;;;;;;;;;
;; MUTABLE FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn reset-game
  "resets the game"
  [snake apple]
  (dosync (ref-set apple (create-apple))
          (ref-set snake (create-snake)))
  nil)

;; setting the initial (nil) state - for debugging
(def test-snake (ref nil))
(def test-apple (ref nil))

(def valid-turns ;; turns that are allowed
  {LEFT  #{UP   DOWN}
   RIGHT #{UP   DOWN}
   UP    #{LEFT RIGHT}
   DOWN  #{LEFT RIGHT}})

(defn update-direction
  "updates the direction of the snake"
  [snake newdir]
  (when (and newdir
             (contains? (valid-turns (:dir @snake))
                        newdir))
    (dosync (alter snake turn newdir))))

(defn update-positions
  "updates the positions based on the position of snake and apple"
  [snake apple]
  (dosync
    (if (eats? @snake @apple)
      (do                              ;; snake eats the apple
        (ref-set apple (create-apple)) ;; create a new apple
        (alter snake move :grow))      ;; make it grow
      (alter snake move)))             ;; the snake simply moves
  nil)

;;;;;;;;;;;;;;;;;;;
;; THE SNAKE GUI ;;
;;;;;;;;;;;;;;;;;;;
(defn fill-point
  [g pt color] ;; g is an instance of java.awt.Graphics instance
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color)
    (.fillRect g x y width height)))

;; a polymorphic method that knows how to paint the `object`
(defmulti paint (fn [g object & _] (:type object)))

;; paint an apple
(defmethod paint :apple [g {:keys [location color]}]
  (fill-point g location color))

;; paint the snake
(defmethod paint :snake [g {:keys [body color]}]
  (doseq [point body]
    (fill-point g point color)))

;; describe the GUI and setup callbacks
(defn game-panel [frame snake apple]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g] ;; draws the jframe
      (proxy-super paintComponent g) ;; call the parent constructor
      (paint g @snake)
      (paint g @apple))
    (actionPerformed [e] ;; called on every timer tick
      (update-positions snake apple)
      (when (lose? @snake)
        (reset-game snake apple)
        (JOptionPane/showMessageDialog frame "You lose!"))
      (when (win? @snake)
        (reset-game snake apple)
        (JOptionPane/showMessageDialog frame "You win!"))
      (.repaint this))
    (keyPressed [e] ;; called when a key is pressed
      (update-direction snake (dirs (.getKeyCode e))))
    (getPreferredSize []
      (Dimension. (* (inc width) point-size)
                  (* (inc height) point-size)))
    (keyReleased [e]) ;; dont care about these two callbacks
    (keyTyped [e])))

;; finally setting the game
(defn game []
  (let [snake (ref (create-snake))
        apple (ref (create-apple))
        frame (JFrame. "Snake")
        panel (game-panel frame snake apple)
        timer (Timer. turn-millis panel)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer)
    [snake, apple, timer]))

(defn -main [& args]
  (game))
