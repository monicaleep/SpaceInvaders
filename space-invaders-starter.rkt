;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 3)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))
(define TANK-TURNR (- WIDTH (/ (image-width TANK) 2)))
(define TANK-TURNL (/ (image-width TANK) 2))
(define INVADER-TURNR (- WIDTH (/ (image-width INVADER) 2)))
(define INVADER-TURNL (/ (image-width INVADER) 2))
(define MISSILE (ellipse 5 15 "solid" "red"))


;; =======================================================================================
;; Data Definitions:

(define-struct game (invaders missiles t))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-t s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;; - empty
;; (cons Invader ListOfInvader)
(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (fn-for-invader (first loi))
                   (fn-for-loi (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)

(define LOM1 empty)
(define LOM2 (cons M1 empty))
(define LOM3 (cons M2 (cons M3 empty)))
(define LOM4 (list M1 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (fn-for-missile (first lom))
                   (fn-for-lom (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =============================================================================

;; Functions

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main g)
  (big-bang g                         ; G
            (on-tick   tock)          ; G -> G
            (to-draw   render)        ; G -> Image
            (stop-when over?)         ; G -> Boolean
            (on-key    handle-key)))  ; G KeyEvent -> G

;; Game -> Game
;; produce the next game state:
;;  change position of missles, if firing
;;         remove missile if off screen
;;         remove missile if hit an invader
;;  change position of invaders
;;         remove invader if hit
;;         produce new invader
;;  change position of tank
(check-expect (tock (make-game (list (make-invader 100 100 1)) (list (make-missile 100 100)) (make-tank 50 1)))
              (make-game empty empty (make-tank (+ 50 TANK-SPEED) 1)))
;(define (tock g) g) ;stub


(define (tock g)
  (make-game (add-invader (update-invaders (filter-invaders (game-invaders g) (game-missiles g))))
             (update-missiles (filter-missiles (game-missiles g) (game-invaders g)))
             (move-tank (game-t g))))


;;ListOfInvaders -> ListOfInvaders
;; randomly add 1 invader at random X coordinate
(check-random (add-invader empty) (cond [(> INVADE-RATE (random 5000)) (cons (make-invader (random WIDTH) 10 1) empty)]
                                        [else empty]))
(check-random (add-invader LOI2) (cond [(> INVADE-RATE (random 5000)) (cons (make-invader (random WIDTH) 10 1) LOI2)]
                                       [else LOI2]))

;(define (add-invader loi) loi)

(define (add-invader loi)
  (cond [(> INVADE-RATE (random 5000)) (cons (make-invader (random WIDTH) 10 1) loi)]
        [else loi]))


;; ListOfInvaders -> ListOfInvaders
;; updates list of invaders by moving them downward
(check-expect (update-invaders LOI1) LOI1)
(check-expect (update-invaders LOI2) (cons (make-invader (+ 150 (* 12 INVADER-X-SPEED))
                                                         (+ 100 (* 12 INVADER-Y-SPEED))
                                                         12)
                                           empty))

;(define (update-invaders loi) loi)
(define (update-invaders loi)
  (cond [(empty? loi) loi]
        [else (cons (update-invader (first loi))
                    (update-invaders (rest loi)))]))



;; Invader -> Invader
;; Updates 1 invader position by INVADER-X-SPEED AND INVADER-Y-SPEED, if hit wall, reverse direction
(check-expect (update-invader I1) (make-invader (+ 150 (* 12 INVADER-X-SPEED))
                                                (+ 100 (* 12 INVADER-Y-SPEED))
                                                12))
(check-expect (update-invader (make-invader 100 100 -1)) (make-invader (+ 100 (* -1 INVADER-X-SPEED))
                                                                       (+ 100 (* 1 INVADER-Y-SPEED))
                                                                       -1))
(check-expect (update-invader (make-invader INVADER-TURNR 50 1)) (make-invader INVADER-TURNR
                                                                               (+ 50 (* 1 INVADER-Y-SPEED))
                                                                               -1))
(check-expect (update-invader (make-invader INVADER-TURNL 100 -1)) (make-invader INVADER-TURNL
                                                                                 (+ 100 (* 1 INVADER-Y-SPEED))
                                                                                 1))

;(define (update-invader i) i)
(define (update-invader i)
  (cond [(> INVADER-TURNL (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)))
         (make-invader INVADER-TURNL
                       (+ (* INVADER-Y-SPEED (abs (invader-dx i)))
                          (invader-y i))
                       (-(invader-dx i)))]
        [(< INVADER-TURNR (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)))
         (make-invader INVADER-TURNR
                       (+ (* INVADER-Y-SPEED (abs (invader-dx i)))
                          (invader-y i))
                       (-(invader-dx i)))]
        [else (make-invader (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i)))
                            (+ (invader-y i) (* INVADER-Y-SPEED (abs (invader-dx i))))
                            (invader-dx i))]))


;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; produces a list of invaders which are not hit by missiles
(check-expect (filter-invaders (cons (make-invader 100 100 1) empty) (cons (make-missile 50 50) empty))
              (cons (make-invader 100 100 1) empty))
(check-expect (filter-invaders (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty))
                               (cons (make-missile 100 100) (cons (make-missile 150 150) empty)))
              (cons (make-invader 50 50 -1) empty))
(check-expect (filter-invaders (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty))
                               (cons (make-missile 100 110) (cons (make-missile 150 150) empty)))
              (cons (make-invader 50 50 -1) empty))
(check-expect (filter-invaders (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty))
                               (cons (make-missile 105 110) (cons (make-missile 150 150) empty)))
              (cons (make-invader 50 50 -1) empty))
(check-expect (filter-invaders (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty))
                               (cons (make-missile 110 111) (cons (make-missile 150 150) empty)))
              (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty)))


;(define (filter-invaders loi lom) loi)

(define (filter-invaders loi lom)
  (cond [(empty? loi) empty]
        [else (if (is-invader-hit? (first loi) lom)
                  (filter-invaders (rest loi) lom)
                  (cons (first loi) (filter-invaders (rest loi) lom)))]))


;; Invader ListOfMissile -> Boolean
;; Produce true if Invader is hit by any missile in listofmissile
(check-expect (is-invader-hit? (make-invader 100 100 1) (cons (make-missile 150 150) (cons (make-missile 50 50) empty)))
              false)
(check-expect (is-invader-hit? (make-invader 100 100 1) (cons (make-missile 100 110) (cons (make-missile 50 50) empty)))
              true)
(check-expect (is-invader-hit? (make-invader 100 100 -1) (cons (make-missile 150 150) (cons (make-missile 105 110) empty)))
              true)

; (define (is-invader-hit? i lom) false)

(define (is-invader-hit? i lom)
  (cond [(empty? lom) false]
        [else (or (is-hit? i (first lom))
                  (is-invader-hit? i (rest lom)))]))


;; Invader Missile -> Boolean
;; if invader and missile are intercepted produce true
;; hit is defined delta x <= 10 AND delta y <=10
(check-expect (is-hit? (make-invader 100 100 1) (make-missile 50 50)) false)
(check-expect (is-hit? (make-invader 100 100 1) (make-missile 100 100)) true)
(check-expect (is-hit? (make-invader 100 100 -1) (make-missile 105 110)) true)
(check-expect (is-hit? (make-invader 100 100 -1) (make-missile 99 110)) true)
(check-expect (is-hit? (make-invader 100 100 2) (make-missile 100 120)) false)
(check-expect (is-hit? (make-invader 100 100 -2) (make-missile 89 100)) false)

;(define (is-hit? i m) false)

(define (is-hit? i m)
  (and (<= (abs (- (invader-x i)  (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))



;; ListOfMissile -> ListOfMissile
;; produce new list of missiles
;; update position of ListOfMissile by decrementing it's position by MISSILE-SPEED
;; removes missile if not on screen
(check-expect (update-missiles empty) empty)
(check-expect (update-missiles (cons (make-missile 100 100) empty)) (cons (make-missile 100 (- 100 MISSILE-SPEED)) empty))
(check-expect (update-missiles (cons (make-missile 50 50) (cons (make-missile 100 100) empty)))
              (cons (make-missile 50 (- 50 MISSILE-SPEED)) (cons (make-missile 100 (- 100 MISSILE-SPEED)) empty)))
(check-expect (update-missiles (cons (make-missile 100 -100) (cons (make-missile 100 100) empty)))
              (cons (make-missile 100 (- 100 MISSILE-SPEED)) empty))

;(define (update-missiles lom) lom) ;stub

(define (update-missiles lom)
  (cond [(empty? lom) empty]
        [else (if (off? (first lom))
                  (update-missiles (rest lom))
                  (cons (update-missile (first lom))
                        (update-missiles (rest lom))))]))


;; Missile -> Missile
;; update position of one missile, decrease in y-coordinate by MISSILE-SPEED
(check-expect (update-missile (make-missile 100 200)) (make-missile 100 (- 200 MISSILE-SPEED)))
(check-expect (update-missile (make-missile 10 100)) (make-missile 10 (- 100 MISSILE-SPEED)))

;(define (update-missile m) m) ;stub

(define (update-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))



;; Missile -> Boolean
;; produce false if missile is off screen
(check-expect (off? (make-missile 100 100)) false)
(check-expect (off? (make-missile 100 0)) false)
(check-expect (off? (make-missile 100 (- (/ (image-height MISSILE) 2)))) true)
(check-expect (off? (make-missile 10 -40)) true)

;(define (off? m) false) ;stub

(define (off? m)
  (<= (missile-y m) (- (/ (image-height MISSILE) 2))))




;; ListOfMissile ListOfInvader -> ListOfMissile
;; produce listofmissiles by filtering list of missiles
;; if they hit any invader, remove from list
(check-expect (filter-missiles (cons (make-missile 50 50) empty) (cons (make-invader 100 100 1) empty))
              (cons (make-missile 50 50) empty))
(check-expect (filter-missiles (cons (make-missile 100 100) (cons (make-missile 150 150) empty))
                               (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty)))
              (cons (make-missile 150 150) empty))
(check-expect (filter-missiles (cons (make-missile 100 110) (cons (make-missile 150 150) empty))
                               (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty)))
              (cons (make-missile 150 150) empty))
(check-expect (filter-missiles (cons (make-missile 105 110) (cons (make-missile 150 150) empty))
                               (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty)))
              (cons (make-missile 150 150) empty))
(check-expect (filter-missiles (cons (make-missile 110 111) (cons (make-missile 150 150) empty))
                               (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty)))
              (cons (make-missile 110 111) (cons (make-missile 150 150) empty)))

;(define (filter-missiles lom loi) lom)

(define (filter-missiles lom loi)
  (cond [(empty? lom) empty]
        [else (if (is-missile-hit? (first lom) loi)
                  (filter-missiles (rest lom) loi)
                  (cons (first lom) (filter-missiles (rest lom) loi)))]))


;; Missile ListOfInvader -> Boolean
;; produce true if missile has hit any single invader
(check-expect (is-missile-hit? (make-missile 100 100) (cons (make-invader 50 50 1) empty)) false)
(check-expect (is-missile-hit? (make-missile 100 100) (cons (make-invader 105 105 1) empty)) true)
(check-expect (is-missile-hit? (make-missile 100 100) (cons (make-invader 99 101 1) empty)) true)
(check-expect (is-missile-hit? (make-missile 100 100) (cons (make-invader 50 50 -1) (cons (make-invader 200 200 1) empty))) false)
(check-expect (is-missile-hit? (make-missile 100 100) (cons (make-invader 50 50 -1) (cons (make-invader 101 101 -2) empty))) true)

;(define (is-missile-hit? m loi) false)

(define (is-missile-hit? m loi)
  (cond [(empty? loi) false]
        [else (or (is-hit? (first loi) m)
                  (is-missile-hit? m (rest loi)))]))


;;Tank -> Tank
;; Move the tank along screen by changing (tank-x t) by TANK-SPEED, direction determined by (tank-dir t)
(check-expect (move-tank T0) (make-tank (+ (* 1 TANK-SPEED) (/ WIDTH 2)) 1))  
(check-expect (move-tank T2) (make-tank (+ (* -1 TANK-SPEED) 50) -1)) 
(check-expect (move-tank (make-tank TANK-TURNR 1)) (make-tank TANK-TURNR 1)) 
(check-expect (move-tank (make-tank TANK-TURNL -1)) (make-tank TANK-TURNL -1)) 

;(define (move-tank t) t) ;stub
(define (move-tank t)
  (cond [(> (+ (tank-x t) (* TANK-SPEED (tank-dir t))) TANK-TURNR)
         (make-tank TANK-TURNR (tank-dir t))]
        [(< (+ (tank-x t) (* TANK-SPEED (tank-dir t))) TANK-TURNL)
         (make-tank TANK-TURNL (tank-dir t))]
        [else (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))


;; ===================================================================================
;; Rendering Functions


;; Game -> Image
;; render all invaders, missiles and the tank on screen, in appropriate places 
;; !!!
(check-expect (render G1)  (place-image TANK 50 TANK-Y BACKGROUND))
(check-expect (render G2)  (place-image TANK 50 TANK-Y 
                                        (place-image MISSILE 150 300 
                                                     (place-image INVADER 150 100 BACKGROUND))))


;(define (render g) BACKGROUND)


(define (render g)
  (render-tank-on (game-t g)
                  (render-missiles-on (game-missiles g)
                                      (render-invaders (game-invaders g)))))

;; Tank Image -> Image
;; produce and image with tank on the image in appropriate place
(check-expect (render-tank-on T1 BACKGROUND) (place-image TANK 50 TANK-Y BACKGROUND))
(check-expect (render-tank-on T1 (render-invaders LOI2)) (place-image TANK 50 TANK-Y (render-invaders LOI2)))
(check-expect (render-tank-on T0 (render-missiles-on LOM4 (render-invaders LOI2)))
              (place-image TANK 150 TANK-Y
                           (render-missiles-on LOM4 (render-invaders LOI2))))

;(define (render-tank-on t img) img)
(define (render-tank-on t img)
  (place-image TANK (tank-x t) TANK-Y img))

;; ListOfMissiles Image -> Image
;; produce image with ListOfMissiles displayed on top
(check-expect (render-missiles-on LOM1 BACKGROUND) BACKGROUND)
(check-expect (render-missiles-on LOM1 (render-invaders LOI2)) (render-invaders LOI2))
(check-expect (render-missiles-on LOM2 (render-invaders LOI2)) (place-image MISSILE 150 300 (render-invaders LOI2)))
(check-expect (render-missiles-on LOM4 BACKGROUND) (place-image MISSILE 150 300
                                                                (place-image MISSILE 150 105 BACKGROUND)))
(check-expect (render-missiles-on LOM4 (render-invaders LOI3)) (place-image MISSILE 150 300
                                                                            (place-image MISSILE 150 105 (render-invaders LOI3))))

;(define (render-missiles-on lom img) BACKGROUND)

(define (render-missiles-on lom img)
  (cond [(empty? lom) img]
        [else (add-missile-on (first lom)
                              (render-missiles-on (rest lom) img))]))

;; Missile Image -> Image
;; produce image with missile on top of given image
(check-expect (add-missile-on (make-missile 100 100) BACKGROUND) (place-image MISSILE 100 100 BACKGROUND))
(check-expect (add-missile-on (make-missile 50 50) (render-invaders LOI3)) (place-image MISSILE 50 50 (render-invaders LOI3)))


;(define (add-missile-on m img) img)
(define (add-missile-on m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))

;; ListOfInvaders -> Image
;; render all invaders, in appropriate places 
(check-expect (render-invaders  empty) BACKGROUND)
(check-expect (render-invaders  (cons I1 empty)) (place-image INVADER 150 100 BACKGROUND))
(check-expect (render-invaders  (cons I1 (cons I2 empty))) (place-image INVADER 150 100
                                                                        (place-image INVADER 150 HEIGHT
                                                                                     BACKGROUND)))
                     
;(define (render loi) BACKGROUND) ;stub

(define (render-invaders loi)
  (cond [(empty? loi) BACKGROUND]
        [else (add-invader-on (first loi)
                              (render-invaders (rest loi)))]))

;; Invader -> Image
;; add invader to the background
(check-expect (add-invader-on I1 BACKGROUND) (place-image INVADER 150 100 BACKGROUND))

;(define (add-invader-on i img) BACKGROUND) ;stub

(define (add-invader-on i img)
  (place-image INVADER (invader-x i) (invader-y i) img))


;; ===================================================================================
;; Stop-When Functions

;; Game -> Boolean
;; Produce true if the game is over
(check-expect (over? G0) false)
(check-expect (over? G3) true)
(check-expect (over? (make-game (list (make-invader 100 100 1) (make-invader 10 HEIGHT 1))
                                (list (make-missile 50 50) (make-missile 1 1))
                                (make-tank 50 1))) true)

(define (over? g)
  (touchdown? (game-invaders g)))
      

;; LOI -> Boolean
;; Produce true if any of the list of invaders has touched bottom
(check-expect (touchdown?  LOI2) false)
(check-expect (touchdown? LOI3) true)

;(define (touchdown? loi) false)

(define (touchdown? loi)
  (cond [(empty? loi) false ]
        [else (or (is-touching? (first loi))
                  (touchdown? (rest loi)))]))

;; Invader-> Boolean
;; produce true if invader is touching bottom of screen
(check-expect (is-touching? I1) false)
(check-expect (is-touching? (make-invader 10 HEIGHT 1)) true)
(check-expect (is-touching? (make-invader 100 (+ 10 HEIGHT) -1 )) true)


;(define (is-touching? i) false)

(define (is-touching? invader)
  (>=  (invader-y invader) HEIGHT))


;; ===================================================================================
;; On-Key Functions
;; Game KeyEvent -> Game
;; handles different key press
;; left changes tank direction to left
;; 'right' changes tank direction to right
;; 'space' fires a missile
(check-expect (handle-key G3 "r") G3)
(check-expect (handle-key G3 "left") (make-game (list (make-invader 150 100 12)
                                                      (make-invader 150 500 -10))
                                                (list (make-missile 150 300)
                                                      (make-missile 150 110))
                                                (make-tank 50 -1)))
(check-expect (handle-key G3 "right") (make-game (list (make-invader 150 100 12)
                                                       (make-invader 150 500 -10))
                                                 (list (make-missile 150 300)
                                                       (make-missile 150 110))
                                                 (make-tank 50 1)))
(check-expect (handle-key (make-game empty empty (make-tank 100 -1)) "left")
              (make-game empty empty (make-tank 100 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 100 1)) "left")
              (make-game empty empty (make-tank 100 -1)))
(check-expect (handle-key G0 " ") (make-game empty (list (make-missile 150 HEIGHT)) (make-tank 150 1)))
(check-expect (handle-key G3 " ") (make-game (list (make-invader 150 100 12)
                                                       (make-invader 150 500 -10))
                                                 (list (make-missile 50 HEIGHT)
                                                       (make-missile 150 300)
                                                       (make-missile 150 110))
                                                 (make-tank 50 1)))
                                                      

;(define (handle-key g ke) g)


(define (handle-key g ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders g) (cons (make-missile (tank-x (game-t g)) HEIGHT) (game-missiles g)) (game-t g))]
        [(and (= 1 (tank-dir (game-t g)))(key=? ke "left"))
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-t g)) -1))]
        [(and (= -1 (tank-dir (game-t g))) (key=? ke "right"))
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-t g)) 1))]
        [else g]))
