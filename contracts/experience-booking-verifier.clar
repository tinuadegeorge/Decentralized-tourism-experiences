;; Experience Booking Verifier - Tourism Platform
;; Lists experiences, manages bookings, verifies guide credentials, processes payments, and handles dispute resolution

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-not-verified (err u104))

(define-data-var experience-nonce uint u0)
(define-data-var booking-nonce uint u0)
(define-data-var dispute-nonce uint u0)

;; Guide Registry
(define-map guides
  { guide: principal }
  {
    verified: bool,
    rating: uint,
    total-bookings: uint,
    total-earnings: uint,
    active: bool,
    joined-at: uint
  }
)

;; Experiences Listing
(define-map experiences
  { exp-id: uint }
  {
    guide: principal,
    title: (string-ascii 100),
    description: (string-ascii 300),
    price: uint,
    location: (string-ascii 100),
    duration: uint,
    max-travelers: uint,
    verified: bool,
    active: bool,
    created-at: uint
  }
)

;; Bookings
(define-map bookings
  { booking-id: uint }
  {
    exp-id: uint,
    traveler: principal,
    date: uint,
    travelers-count: uint,
    total-payment: uint,
    status: (string-ascii 20),
    created-at: uint,
    completed-at: uint
  }
)

;; Reviews
(define-map reviews
  { booking-id: uint }
  {
    rating: uint,
    comment: (string-ascii 200),
    reviewed-at: uint
  }
)

;; Disputes
(define-map disputes
  { dispute-id: uint }
  {
    booking-id: uint,
    raised-by: principal,
    reason: (string-ascii 300),
    status: (string-ascii 20),
    resolution: (optional (string-ascii 200)),
    created-at: uint
  }
)

;; Experience Availability
(define-map experience-dates
  { exp-id: uint, date: uint }
  { available: bool, booked-count: uint }
)

;; Read-Only Functions

(define-read-only (get-experience (exp-id uint))
  (map-get? experiences { exp-id: exp-id })
)

(define-read-only (get-guide (guide principal))
  (map-get? guides { guide: guide })
)

(define-read-only (get-booking (booking-id uint))
  (map-get? bookings { booking-id: booking-id })
)

(define-read-only (get-review (booking-id uint))
  (map-get? reviews { booking-id: booking-id })
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (check-availability (exp-id uint) (date uint))
  (map-get? experience-dates { exp-id: exp-id, date: date })
)

;; Public Functions

;; Register as Guide
(define-public (register-guide)
  (begin
    (asserts! (is-none (get-guide tx-sender)) err-unauthorized)
    (map-set guides
      { guide: tx-sender }
      {
        verified: false,
        rating: u0,
        total-bookings: u0,
        total-earnings: u0,
        active: true,
        joined-at: block-height
      }
    )
    (ok true)
  )
)

;; Verify Guide
(define-public (verify-guide (guide principal))
  (let
    ((guide-data (unwrap! (get-guide guide) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set guides
      { guide: guide }
      (merge guide-data { verified: true })
    )
    (ok true)
  )
)

;; Create Experience
(define-public (create-experience
  (title (string-ascii 100))
  (description (string-ascii 300))
  (price uint)
  (location (string-ascii 100))
  (duration uint)
  (max-travelers uint))
  (let
    ((new-id (+ (var-get experience-nonce) u1))
     (guide-data (unwrap! (get-guide tx-sender) err-not-found)))
    (asserts! (get verified guide-data) err-not-verified)
    (asserts! (get active guide-data) err-unauthorized)
    (asserts! (> price u0) err-invalid-amount)
    (map-set experiences
      { exp-id: new-id }
      {
        guide: tx-sender,
        title: title,
        description: description,
        price: price,
        location: location,
        duration: duration,
        max-travelers: max-travelers,
        verified: true,
        active: true,
        created-at: block-height
      }
    )
    (var-set experience-nonce new-id)
    (ok new-id)
  )
)

;; Create Booking
(define-public (create-booking
  (exp-id uint)
  (date uint)
  (travelers-count uint))
  (let
    ((new-id (+ (var-get booking-nonce) u1))
     (exp (unwrap! (get-experience exp-id) err-not-found))
     (guide-data (unwrap! (get-guide (get guide exp)) err-not-found))
     (total-cost (* (get price exp) travelers-count)))
    (asserts! (get active exp) err-unauthorized)
    (asserts! (<= travelers-count (get max-travelers exp)) err-invalid-amount)
    (asserts! (> travelers-count u0) err-invalid-amount)
    (map-set bookings
      { booking-id: new-id }
      {
        exp-id: exp-id,
        traveler: tx-sender,
        date: date,
        travelers-count: travelers-count,
        total-payment: total-cost,
        status: "confirmed",
        created-at: block-height,
        completed-at: u0
      }
    )
    (map-set guides
      { guide: (get guide exp) }
      (merge guide-data {
        total-bookings: (+ (get total-bookings guide-data) u1),
        total-earnings: (+ (get total-earnings guide-data) total-cost)
      })
    )
    (map-set experience-dates
      { exp-id: exp-id, date: date }
      {
        available: true,
        booked-count: (+ (default-to u0 
          (get booked-count (map-get? experience-dates { exp-id: exp-id, date: date }))) travelers-count)
      }
    )
    (var-set booking-nonce new-id)
    (ok new-id)
  )
)

;; Complete Booking
(define-public (complete-booking (booking-id uint))
  (let
    ((booking (unwrap! (get-booking booking-id) err-not-found)))
    (asserts! (is-eq tx-sender (get traveler booking)) err-unauthorized)
    (asserts! (is-eq (get status booking) "confirmed") err-unauthorized)
    (map-set bookings
      { booking-id: booking-id }
      (merge booking {
        status: "completed",
        completed-at: block-height
      })
    )
    (ok true)
  )
)

;; Submit Review
(define-public (submit-review
  (booking-id uint)
  (rating uint)
  (comment (string-ascii 200)))
  (let
    ((booking (unwrap! (get-booking booking-id) err-not-found))
     (exp (unwrap! (get-experience (get exp-id booking)) err-not-found))
     (guide-data (unwrap! (get-guide (get guide exp)) err-not-found)))
    (asserts! (is-eq tx-sender (get traveler booking)) err-unauthorized)
    (asserts! (is-eq (get status booking) "completed") err-unauthorized)
    (asserts! (<= rating u100) err-invalid-amount)
    (map-set reviews
      { booking-id: booking-id }
      {
        rating: rating,
        comment: comment,
        reviewed-at: block-height
      }
    )
    (map-set guides
      { guide: (get guide exp) }
      (merge guide-data {
        rating: (/ (+ (* (get rating guide-data) (get total-bookings guide-data)) rating) 
                  (get total-bookings guide-data))
      })
    )
    (ok true)
  )
)

;; Raise Dispute
(define-public (raise-dispute
  (booking-id uint)
  (reason (string-ascii 300)))
  (let
    ((new-id (+ (var-get dispute-nonce) u1))
     (booking (unwrap! (get-booking booking-id) err-not-found)))
    (asserts! (is-eq tx-sender (get traveler booking)) err-unauthorized)
    (map-set disputes
      { dispute-id: new-id }
      {
        booking-id: booking-id,
        raised-by: tx-sender,
        reason: reason,
        status: "open",
        resolution: none,
        created-at: block-height
      }
    )
    (var-set dispute-nonce new-id)
    (ok new-id)
  )
)

;; Resolve Dispute
(define-public (resolve-dispute
  (dispute-id uint)
  (resolution (string-ascii 200)))
  (let
    ((dispute (unwrap! (get-dispute dispute-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute {
        status: "resolved",
        resolution: (some resolution)
      })
    )
    (ok true)
  )
)

;; Update Experience Status
(define-public (update-experience-status (exp-id uint) (active bool))
  (let
    ((exp (unwrap! (get-experience exp-id) err-not-found)))
    (asserts! (is-eq tx-sender (get guide exp)) err-unauthorized)
    (map-set experiences
      { exp-id: exp-id }
      (merge exp { active: active })
    )
    (ok true)
  )
)
