;; WorkoutChain: Fitness Routine and Exercise Program Exchange Platform
;; Version: 1.0.0

(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-WORKOUT-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-PUBLISHED (err u3))
(define-constant ERR-INVALID-STATUS (err u4))
(define-constant ERR-INVALID-DURATION (err u5))
(define-constant ERR-INVALID-FITNESS-TYPE (err u6))
(define-constant ERR-INVALID-INTENSITY (err u7))
(define-constant ERR-INVALID-WORKOUT-TITLE (err u8))
(define-constant ERR-INVALID-ROUTINE (err u9))

(define-constant MIN-DURATION u10)

(define-data-var next-workout-id uint u1)

(define-map fitness-library
    uint
    {
        trainer: principal,
        workout-title: (string-utf8 50),
        routine: (string-utf8 200),
        fitness-type: (string-utf8 15),
        intensity: (string-utf8 10),
        availability-status: (string-utf8 15),
        duration-minutes: uint
    })

(define-private (validate-fitness-type (fitness-type (string-utf8 15)))
    (or 
        (is-eq fitness-type u"Strength")
        (is-eq fitness-type u"Cardio")
        (is-eq fitness-type u"Flexibility")
        (is-eq fitness-type u"HIIT")
        (is-eq fitness-type u"Yoga")
        (is-eq fitness-type u"Pilates")
    ))

(define-private (validate-intensity (intensity (string-utf8 10)))
    (or 
        (is-eq intensity u"Light")
        (is-eq intensity u"Moderate")
        (is-eq intensity u"Vigorous")
        (is-eq intensity u"High")
        (is-eq intensity u"Extreme")
    ))

(define-private (validate-text-structure (text (string-utf8 200)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    ))

(define-public (create-workout 
    (workout-title (string-utf8 50))
    (routine (string-utf8 200))
    (fitness-type (string-utf8 15))
    (intensity (string-utf8 10))
    (duration-minutes uint))
    (let
        (
            (workout-id (var-get next-workout-id))
        )
        (asserts! (validate-text-structure workout-title u3 u50) ERR-INVALID-WORKOUT-TITLE)
        (asserts! (validate-text-structure routine u10 u200) ERR-INVALID-ROUTINE)
        (asserts! (>= duration-minutes MIN-DURATION) ERR-INVALID-DURATION)
        (asserts! (validate-fitness-type fitness-type) ERR-INVALID-FITNESS-TYPE)
        (asserts! (validate-intensity intensity) ERR-INVALID-INTENSITY)
        
        (map-set fitness-library workout-id {
            trainer: tx-sender,
            workout-title: workout-title,
            routine: routine,
            fitness-type: fitness-type,
            intensity: intensity,
            availability-status: u"active",
            duration-minutes: duration-minutes
        })
        (var-set next-workout-id (+ workout-id u1))
        (ok workout-id)
    ))

(define-public (archive-workout (workout-id uint))
    (let
        (
            (workout (unwrap! (map-get? fitness-library workout-id) ERR-WORKOUT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get trainer workout)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get availability-status workout) u"active") ERR-INVALID-STATUS)
        (ok (map-set fitness-library workout-id (merge workout { availability-status: u"archived" })))
    ))

(define-read-only (get-workout (workout-id uint))
    (ok (map-get? fitness-library workout-id)))

(define-read-only (get-trainer (workout-id uint))
    (ok (get trainer (unwrap! (map-get? fitness-library workout-id) ERR-WORKOUT-NOT-FOUND))))

(define-read-only (get-total-workouts)
    (ok (- (var-get next-workout-id) u1)))

(define-read-only (get-availability-status (workout-id uint))
    (ok (get availability-status (unwrap! (map-get? fitness-library workout-id) ERR-WORKOUT-NOT-FOUND))))