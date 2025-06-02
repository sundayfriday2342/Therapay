(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-SESSION-EXISTS (err u102))
(define-constant ERR-NO-SESSION (err u103))
(define-constant ERR-ALREADY-CONFIRMED (err u104))
(define-constant ERR-NOT-PATIENT (err u105))
(define-constant ERR-NOT-THERAPIST (err u106))
(define-constant ERR-INSUFFICIENT-BALANCE (err u107))

(define-data-var contract-owner principal tx-sender)
(define-data-var fee-percentage uint u5)

(define-map therapists principal
  {
    verified: bool,
    rate: uint,
    sessions-completed: uint
  }
)

(define-map sessions uint
  {
    patient: principal,
    therapist: principal,
    amount: uint,
    status: (string-ascii 20),
    timestamp: uint,
    completed: bool
  }
)

(define-map session-counter principal uint)

(define-public (register-therapist (rate uint))
  (let
    (
      (caller tx-sender)
    )
    (ok (map-set therapists caller {
      verified: false,
      rate: rate,
      sessions-completed: u0
    }))
  )
)

(define-public (verify-therapist (therapist principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (map-set therapists therapist 
      (merge (unwrap-panic (map-get? therapists therapist))
        { verified: true }
      )
    ))
  )
)

(define-public (book-session (therapist principal))
  (let
    (
      (patient tx-sender)
      (therapist-data (unwrap! (map-get? therapists therapist) ERR-NOT-AUTHORIZED))
      (session-id (default-to u0 (map-get? session-counter patient)))
      (new-session-id (+ session-id u1))
    )
    (asserts! (get verified therapist-data) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? (get rate therapist-data) tx-sender (as-contract tx-sender)))
    (map-set session-counter patient new-session-id)
    (ok (map-set sessions new-session-id
      {
        patient: patient,
        therapist: therapist,
        amount: (get rate therapist-data),
        status: "booked",
        timestamp: stacks-block-height,
        completed: false
      }
    ))
  )
)

(define-public (complete-session (session-id uint))
  (let
    (
      (session (unwrap! (map-get? sessions session-id) ERR-NO-SESSION))
      (therapist-data (unwrap! (map-get? therapists (get therapist session)) ERR-NOT-AUTHORIZED))
    )
    (asserts! (is-eq tx-sender (get therapist session)) ERR-NOT-THERAPIST)
    (asserts! (not (get completed session)) ERR-ALREADY-CONFIRMED)
    (try! (as-contract (stx-transfer? 
      (get amount session)
      tx-sender
      (get therapist session))))
    (map-set therapists (get therapist session)
      (merge therapist-data
        { sessions-completed: (+ (get sessions-completed therapist-data) u1) }
      ))
    (ok (map-set sessions session-id
      (merge session { completed: true, status: "completed" })))
  )
)

(define-read-only (get-session (session-id uint))
  (ok (map-get? sessions session-id))
)

(define-read-only (get-therapist-info (therapist principal))
  (ok (map-get? therapists therapist))
)

(define-read-only (get-session-count (patient principal))
  (ok (default-to u0 (map-get? session-counter patient)))
)

(define-public (update-rate (new-rate uint))
  (let
    (
      (therapist tx-sender)
      (therapist-data (unwrap! (map-get? therapists therapist) ERR-NOT-AUTHORIZED))
    )
    (ok (map-set therapists therapist
      (merge therapist-data { rate: new-rate })))
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-owner new-owner))
  )
)