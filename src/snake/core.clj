(ns snake.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener))
  (:use snake.import-static)
  (:gen-class))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)


(def width 75)
(def height 50)      ;; dimensions of the canvas
(def point-size 10)  ;; the size of the point
(def turn-millis 60) ;; the heartbeat of the game
(def win-length 10)  ;; turns it take to win

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

;; a better name for the previous function
(def lose? head-overlaps-body?)

;; determines if a snake eats an apple
(defn eats? [{[snake-head] :body} {apple :location}]
  (= snake-head apple))

(def valid-turns
  {VK_LEFT [VK_UP VK_DOWN]
   VK_RIGHT [VK_UP VK_DOWN]
   VK_DOWN [VK_LEFT VK_RIGHT]
   VK_UP [VK_LEFT VK_RIGHT]})

(defn turn
  "turns the snake in the new dir"
  [snake newdir]
  (assoc snake :dir newdir))

;; MUTABLE FUNCTIONS
(defn reset-game [snake apple]
  (dosync (ref-set apple (create-apple))
          (ref-set snake (create-snake)))
  nil)

;; setting the initial (nil) state
(def test-snake (ref nil))
(def test-apple (ref nil))

;; let's begin by resetting the game
(reset-game test-snake test-apple)

(defn update-direction [snake newdir]
  (when newdir (dosync (alter snake turn newdir))))

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
