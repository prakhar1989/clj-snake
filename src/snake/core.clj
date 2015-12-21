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
(def turn-millis 75) ;; the heartbeat of the game
(def win-length 5)   ;; turns it take to win

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
  "convert a point to a rect on the screen"
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

(defn turn
  "turns the snake in the new dir"
  [snake newdir]
  (assoc snake :dir newdir))


(turn (create-snake) (dirs VK_LEFT))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
