(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-SESSION-EXISTS (err u102))
(define-constant ERR-NO-SESSION (err u103))
(define-constant ERR-ALREADY-CONFIRMED (err u104))
(define-constant ERR-NOT-PATIENT (err u105))
(define-constant ERR-NOT-THERAPIST (err u106))
(define-constant ERR-INSUFFICIENT-BALANCE (err u107))
(define-constant ERR-NO-SUBSCRIPTION (err u108))
(define-constant ERR-SUBSCRIPTION-EXPIRED (err u109))
(define-constant ERR-NO-SESSIONS-LEFT (err u110))
(define-constant ERR-INVALID-PLAN (err u111))
(define-constant ERR-SUBSCRIPTION-EXISTS (err u112))
(define-constant ERR-ALREADY-RATED (err u113))
(define-constant ERR-INVALID-RATING (err u114))
(define-constant ERR-CANNOT-RATE-SELF (err u115))
(define-constant ERR-SESSION-NOT-COMPLETED (err u116))
(define-constant ERR-NO-RATING (err u117))

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
    status: (string-ascii 25),
    timestamp: uint,
    completed: bool
  }
)

(define-map session-counter principal uint)

(define-map subscription-plans uint
  {
    name: (string-ascii 50),
    sessions: uint,
    duration-blocks: uint,
    discount-percentage: uint,
    created-by: principal,
    active: bool
  }
)

(define-map patient-subscriptions principal
  {
    plan-id: uint,
    therapist: principal,
    sessions-remaining: uint,
    expiry-block: uint,
    activated-block: uint,
    auto-renew: bool
  }
)

(define-map subscription-usage principal
  {
    total-subscriptions: uint,
    total-sessions-used: uint,
    current-savings: uint
  }
)

(define-data-var next-plan-id uint u1)

(define-map therapist-ratings principal
  {
    total-ratings: uint,
    total-score: uint,
    average-rating: uint,
    five-star: uint,
    four-star: uint,
    three-star: uint,
    two-star: uint,
    one-star: uint
  }
)

(define-map session-ratings uint
  {
    patient: principal,
    therapist: principal,
    rating: uint,
    review-text: (string-ascii 500),
    timestamp: uint,
    therapist-response: (optional (string-ascii 300)),
    response-timestamp: (optional uint)
  }
)

(define-map patient-rating-history principal
  {
    total-ratings-given: uint,
    average-rating-given: uint,
    last-rating-block: uint
  }
)

(define-data-var next-rating-id uint u1)

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

(define-public (create-subscription-plan (name (string-ascii 50)) (session-count uint) (duration-blocks uint) (discount-percentage uint))
  (let
    (
      (therapist tx-sender)
      (therapist-data (unwrap! (map-get? therapists therapist) ERR-NOT-AUTHORIZED))
      (plan-id (var-get next-plan-id))
    )
    (asserts! (get verified therapist-data) ERR-NOT-AUTHORIZED)
    (asserts! (> session-count u0) ERR-INVALID-AMOUNT)
    (asserts! (> duration-blocks u0) ERR-INVALID-AMOUNT)
    (asserts! (<= discount-percentage u50) ERR-INVALID-AMOUNT)
    (var-set next-plan-id (+ plan-id u1))
    (ok (map-set subscription-plans plan-id
      {
        name: name,
        sessions: session-count,
        duration-blocks: duration-blocks,
        discount-percentage: discount-percentage,
        created-by: therapist,
        active: true
      }
    ))
  )
)

(define-public (subscribe-to-plan (plan-id uint) (auto-renew bool))
  (let
    (
      (patient tx-sender)
      (plan (unwrap! (map-get? subscription-plans plan-id) ERR-INVALID-PLAN))
      (therapist (get created-by plan))
      (therapist-data (unwrap! (map-get? therapists therapist) ERR-NOT-AUTHORIZED))
      (base-cost (* (get sessions plan) (get rate therapist-data)))
      (discount-amount (/ (* base-cost (get discount-percentage plan)) u100))
      (final-cost (- base-cost discount-amount))
      (expiry-block (+ stacks-block-height (get duration-blocks plan)))
      (existing-subscription (map-get? patient-subscriptions patient))
    )
    (asserts! (get active plan) ERR-INVALID-PLAN)
    (asserts! (get verified therapist-data) ERR-NOT-AUTHORIZED)
    (asserts! (is-none existing-subscription) ERR-SUBSCRIPTION-EXISTS)
    (try! (stx-transfer? final-cost patient (as-contract tx-sender)))
    (map-set patient-subscriptions patient
      {
        plan-id: plan-id,
        therapist: therapist,
        sessions-remaining: (get sessions plan),
        expiry-block: expiry-block,
        activated-block: stacks-block-height,
        auto-renew: auto-renew
      }
    )
    (map-set subscription-usage patient
      (merge (default-to {total-subscriptions: u0, total-sessions-used: u0, current-savings: u0}
                         (map-get? subscription-usage patient))
             {
               total-subscriptions: (+ (default-to u0 (get total-subscriptions (map-get? subscription-usage patient))) u1),
               current-savings: (+ (default-to u0 (get current-savings (map-get? subscription-usage patient))) discount-amount)
             }
      )
    )
    (ok true)
  )
)

(define-public (use-subscription-session)
  (let
    (
      (patient tx-sender)
      (subscription (unwrap! (map-get? patient-subscriptions patient) ERR-NO-SUBSCRIPTION))
      (therapist (get therapist subscription))
      (therapist-data (unwrap! (map-get? therapists therapist) ERR-NOT-AUTHORIZED))
      (session-id (+ (default-to u0 (map-get? session-counter patient)) u1))
    )
    (asserts! (> (get sessions-remaining subscription) u0) ERR-NO-SESSIONS-LEFT)
    (asserts! (> (get expiry-block subscription) stacks-block-height) ERR-SUBSCRIPTION-EXPIRED)
    (map-set session-counter patient session-id)
    (map-set sessions session-id
      {
        patient: patient,
        therapist: therapist,
        amount: (get rate therapist-data),
        status: "subscription-booked",
        timestamp: stacks-block-height,
        completed: false
      }
    )
    (map-set patient-subscriptions patient
      (merge subscription
        { sessions-remaining: (- (get sessions-remaining subscription) u1) }
      )
    )
    (ok session-id)
  )
)

(define-public (complete-subscription-session (session-id uint))
  (let
    (
      (session (unwrap! (map-get? sessions session-id) ERR-NO-SESSION))
      (therapist (get therapist session))
      (therapist-data (unwrap! (map-get? therapists therapist) ERR-NOT-AUTHORIZED))
      (patient (get patient session))
      (subscription (unwrap! (map-get? patient-subscriptions patient) ERR-NO-SUBSCRIPTION))
    )
    (asserts! (is-eq tx-sender therapist) ERR-NOT-THERAPIST)
    (asserts! (not (get completed session)) ERR-ALREADY-CONFIRMED)
    (asserts! (is-eq (get status session) "subscription-booked") ERR-NOT-AUTHORIZED)
    (try! (as-contract (stx-transfer? (get amount session) tx-sender therapist)))
    (map-set therapists therapist
      (merge therapist-data
        { sessions-completed: (+ (get sessions-completed therapist-data) u1) }
      )
    )
    (map-set sessions session-id
      (merge session { completed: true, status: "subscription-completed" })
    )
    (map-set subscription-usage patient
      (merge (default-to {total-subscriptions: u0, total-sessions-used: u0, current-savings: u0}
                         (map-get? subscription-usage patient))
             {
               total-sessions-used: (+ (default-to u0 (get total-sessions-used (map-get? subscription-usage patient))) u1)
             }
      )
    )
    (ok true)
  )
)

(define-public (cancel-subscription)
  (let
    (
      (patient tx-sender)
      (subscription (unwrap! (map-get? patient-subscriptions patient) ERR-NO-SUBSCRIPTION))
      (sessions-remaining (get sessions-remaining subscription))
      (plan (unwrap! (map-get? subscription-plans (get plan-id subscription)) ERR-INVALID-PLAN))
      (therapist-data (unwrap! (map-get? therapists (get therapist subscription)) ERR-NOT-AUTHORIZED))
      (refund-per-session (/ (* (get rate therapist-data) (- u100 (get discount-percentage plan))) u100))
      (total-refund (* sessions-remaining refund-per-session))
    )
    (asserts! (> sessions-remaining u0) ERR-NO-SESSIONS-LEFT)
    (try! (as-contract (stx-transfer? total-refund tx-sender patient)))
    (ok (map-delete patient-subscriptions patient))
  )
)

(define-public (renew-subscription (plan-id uint))
  (let
    (
      (patient tx-sender)
      (subscription (unwrap! (map-get? patient-subscriptions patient) ERR-NO-SUBSCRIPTION))
      (plan (unwrap! (map-get? subscription-plans plan-id) ERR-INVALID-PLAN))
      (therapist (get created-by plan))
      (therapist-data (unwrap! (map-get? therapists therapist) ERR-NOT-AUTHORIZED))
      (base-cost (* (get sessions plan) (get rate therapist-data)))
      (discount-amount (/ (* base-cost (get discount-percentage plan)) u100))
      (final-cost (- base-cost discount-amount))
      (new-expiry (+ stacks-block-height (get duration-blocks plan)))
    )
    (asserts! (get active plan) ERR-INVALID-PLAN)
    (asserts! (get verified therapist-data) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? final-cost patient (as-contract tx-sender)))
    (map-set patient-subscriptions patient
      {
        plan-id: plan-id,
        therapist: therapist,
        sessions-remaining: (get sessions plan),
        expiry-block: new-expiry,
        activated-block: stacks-block-height,
        auto-renew: (get auto-renew subscription)
      }
    )
    (map-set subscription-usage patient
      (merge (default-to {total-subscriptions: u0, total-sessions-used: u0, current-savings: u0}
                         (map-get? subscription-usage patient))
             {
               total-subscriptions: (+ (default-to u0 (get total-subscriptions (map-get? subscription-usage patient))) u1),
               current-savings: (+ (default-to u0 (get current-savings (map-get? subscription-usage patient))) discount-amount)
             }
      )
    )
    (ok true)
  )
)

(define-public (deactivate-plan (plan-id uint))
  (let
    (
      (plan (unwrap! (map-get? subscription-plans plan-id) ERR-INVALID-PLAN))
    )
    (asserts! (is-eq tx-sender (get created-by plan)) ERR-NOT-AUTHORIZED)
    (ok (map-set subscription-plans plan-id
      (merge plan { active: false })
    ))
  )
)

(define-read-only (get-subscription-plan (plan-id uint))
  (ok (map-get? subscription-plans plan-id))
)

(define-read-only (get-patient-subscription (patient principal))
  (ok (map-get? patient-subscriptions patient))
)

(define-read-only (get-subscription-usage (patient principal))
  (ok (map-get? subscription-usage patient))
)

(define-read-only (calculate-plan-cost (plan-id uint) (therapist principal))
  (let
    (
      (plan (unwrap! (map-get? subscription-plans plan-id) ERR-INVALID-PLAN))
      (therapist-data (unwrap! (map-get? therapists therapist) ERR-NOT-AUTHORIZED))
      (base-cost (* (get sessions plan) (get rate therapist-data)))
      (discount-amount (/ (* base-cost (get discount-percentage plan)) u100))
    )
    (ok {
      base-cost: base-cost,
      discount-amount: discount-amount,
      final-cost: (- base-cost discount-amount),
      savings-percentage: (get discount-percentage plan)
    })
  )
)

(define-read-only (get-active-plans-by-therapist (therapist principal))
  (ok (filter check-plan-active-for-therapist 
      (list (var-get next-plan-id))))
)

(define-private (check-plan-active-for-therapist (plan-id uint))
  (match (map-get? subscription-plans plan-id)
    plan (and (get active plan) (is-eq (get created-by plan) tx-sender))
    false
  )
)

(define-public (rate-therapist-session (session-id uint) (rating uint) (review-text (string-ascii 500)))
  (let
    (
      (patient tx-sender)
      (session (unwrap! (map-get? sessions session-id) ERR-NO-SESSION))
      (therapist (get therapist session))
      (rating-id (var-get next-rating-id))
      (existing-rating (map-get? session-ratings session-id))
      (therapist-rating-data (default-to 
        {total-ratings: u0, total-score: u0, average-rating: u0, 
         five-star: u0, four-star: u0, three-star: u0, two-star: u0, one-star: u0}
        (map-get? therapist-ratings therapist)))
      (patient-history (default-to 
        {total-ratings-given: u0, average-rating-given: u0, last-rating-block: u0}
        (map-get? patient-rating-history patient)))
    )
    (asserts! (is-eq patient (get patient session)) ERR-NOT-PATIENT)
    (asserts! (not (is-eq patient therapist)) ERR-CANNOT-RATE-SELF)
    (asserts! (get completed session) ERR-SESSION-NOT-COMPLETED)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (asserts! (is-none existing-rating) ERR-ALREADY-RATED)
    (var-set next-rating-id (+ rating-id u1))
    (map-set session-ratings session-id
      {
        patient: patient,
        therapist: therapist,
        rating: rating,
        review-text: review-text,
        timestamp: stacks-block-height,
        therapist-response: none,
        response-timestamp: none
      }
    )
    (let
      (
        (new-total-ratings (+ (get total-ratings therapist-rating-data) u1))
        (new-total-score (+ (get total-score therapist-rating-data) rating))
        (new-average (/ new-total-score new-total-ratings))
        (updated-star-counts (update-star-count therapist-rating-data rating))
      )
      (map-set therapist-ratings therapist
        (merge updated-star-counts
          {
            total-ratings: new-total-ratings,
            total-score: new-total-score,
            average-rating: new-average
          }
        )
      )
    )
    (map-set patient-rating-history patient
      {
        total-ratings-given: (+ (get total-ratings-given patient-history) u1),
        average-rating-given: (/ (+ (* (get average-rating-given patient-history) (get total-ratings-given patient-history)) rating) (+ (get total-ratings-given patient-history) u1)),
        last-rating-block: stacks-block-height
      }
    )
    (ok rating-id)
  )
)

(define-public (respond-to-rating (session-id uint) (response-text (string-ascii 300)))
  (let
    (
      (therapist tx-sender)
      (rating (unwrap! (map-get? session-ratings session-id) ERR-NO-RATING))
    )
    (asserts! (is-eq therapist (get therapist rating)) ERR-NOT-THERAPIST)
    (ok (map-set session-ratings session-id
      (merge rating
        {
          therapist-response: (some response-text),
          response-timestamp: (some stacks-block-height)
        }
      )
    ))
  )
)

(define-public (update-rating (session-id uint) (new-rating uint) (new-review-text (string-ascii 500)))
  (let
    (
      (patient tx-sender)
      (rating (unwrap! (map-get? session-ratings session-id) ERR-NO-RATING))
      (therapist (get therapist rating))
      (old-rating-value (get rating rating))
      (therapist-rating-data (unwrap! (map-get? therapist-ratings therapist) ERR-NO-RATING))
    )
    (asserts! (is-eq patient (get patient rating)) ERR-NOT-PATIENT)
    (asserts! (and (>= new-rating u1) (<= new-rating u5)) ERR-INVALID-RATING)
    (let
      (
        (updated-total-score (+ (- (get total-score therapist-rating-data) old-rating-value) new-rating))
        (new-average (/ updated-total-score (get total-ratings therapist-rating-data)))
        (decremented-star-counts (decrement-star-count therapist-rating-data old-rating-value))
        (updated-star-counts (increment-star-count decremented-star-counts new-rating))
      )
      (map-set therapist-ratings therapist
        (merge updated-star-counts
          {
            total-score: updated-total-score,
            average-rating: new-average
          }
        )
      )
    )
    (ok (map-set session-ratings session-id
      (merge rating
        {
          rating: new-rating,
          review-text: new-review-text,
          timestamp: stacks-block-height
        }
      )
    ))
  )
)

(define-public (flag-inappropriate-review (session-id uint))
  (let
    (
      (caller tx-sender)
      (rating (unwrap! (map-get? session-ratings session-id) ERR-NO-RATING))
    )
    (asserts! (or (is-eq caller (var-get contract-owner)) 
                  (is-eq caller (get therapist rating))) ERR-NOT-AUTHORIZED)
    (ok (map-set session-ratings session-id
      (merge rating { review-text: "Review flagged as inappropriate" })
    ))
  )
)

(define-read-only (get-therapist-rating (therapist principal))
  (ok (map-get? therapist-ratings therapist))
)

(define-read-only (get-session-rating (session-id uint))
  (ok (map-get? session-ratings session-id))
)

(define-read-only (get-patient-rating-history (patient principal))
  (ok (map-get? patient-rating-history patient))
)

(define-read-only (get-therapist-rating-breakdown (therapist principal))
  (let
    (
      (rating-data (map-get? therapist-ratings therapist))
    )
    (match rating-data
      data (ok (some {
        average-rating: (get average-rating data),
        total-ratings: (get total-ratings data),
        rating-distribution: {
          five-star: (get five-star data),
          four-star: (get four-star data),
          three-star: (get three-star data),
          two-star: (get two-star data),
          one-star: (get one-star data)
        }
      }))
      (ok none)
    )
  )
)

(define-read-only (get-recent-reviews (therapist principal) (limit uint))
  (ok (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10))
)

(define-private (update-star-count (rating-data (tuple (total-ratings uint) (total-score uint) (average-rating uint) (five-star uint) (four-star uint) (three-star uint) (two-star uint) (one-star uint))) (rating uint))
  (if (is-eq rating u5)
    (merge rating-data { five-star: (+ (get five-star rating-data) u1) })
    (if (is-eq rating u4)
      (merge rating-data { four-star: (+ (get four-star rating-data) u1) })
      (if (is-eq rating u3)
        (merge rating-data { three-star: (+ (get three-star rating-data) u1) })
        (if (is-eq rating u2)
          (merge rating-data { two-star: (+ (get two-star rating-data) u1) })
          (merge rating-data { one-star: (+ (get one-star rating-data) u1) })
        )
      )
    )
  )
)

(define-private (increment-star-count (rating-data (tuple (total-ratings uint) (total-score uint) (average-rating uint) (five-star uint) (four-star uint) (three-star uint) (two-star uint) (one-star uint))) (rating uint))
  (if (is-eq rating u5)
    (merge rating-data { five-star: (+ (get five-star rating-data) u1) })
    (if (is-eq rating u4)
      (merge rating-data { four-star: (+ (get four-star rating-data) u1) })
      (if (is-eq rating u3)
        (merge rating-data { three-star: (+ (get three-star rating-data) u1) })
        (if (is-eq rating u2)
          (merge rating-data { two-star: (+ (get two-star rating-data) u1) })
          (merge rating-data { one-star: (+ (get one-star rating-data) u1) })
        )
      )
    )
  )
)

(define-private (decrement-star-count (rating-data (tuple (total-ratings uint) (total-score uint) (average-rating uint) (five-star uint) (four-star uint) (three-star uint) (two-star uint) (one-star uint))) (rating uint))
  (if (is-eq rating u5)
    (merge rating-data { five-star: (- (get five-star rating-data) u1) })
    (if (is-eq rating u4)
      (merge rating-data { four-star: (- (get four-star rating-data) u1) })
      (if (is-eq rating u3)
        (merge rating-data { three-star: (- (get three-star rating-data) u1) })
        (if (is-eq rating u2)
          (merge rating-data { two-star: (- (get two-star rating-data) u1) })
          (merge rating-data { one-star: (- (get one-star rating-data) u1) })
        )
      )
    )
  )
)


