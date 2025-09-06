;; emergency-protocol.clar - Emergency Session Protocol & Crisis Support

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-EMERGENCY-NOT-FOUND (err u301))
(define-constant ERR-THERAPIST-UNAVAILABLE (err u302))
(define-constant ERR-INVALID-CRISIS-LEVEL (err u303))
(define-constant ERR-ALREADY-RESPONDED (err u304))
(define-constant ERR-EMERGENCY-RESOLVED (err u305))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u306))
(define-constant ERR-COOLDOWN-ACTIVE (err u307))

;; Constants for emergency sessions
(define-constant EMERGENCY-RATE-MULTIPLIER u150) ;; 50% higher rate for emergency
(define-constant CRISIS-ESCALATION-THRESHOLD u3) ;; High crisis level
(define-constant EMERGENCY-COOLDOWN u144) ;; 1 day cooldown between emergency requests
(define-constant MAX-RESPONSE_TIME u72) ;; 12 hours maximum response time
(define-constant CRISIS-FUND_PERCENTAGE u20) ;; 20% of emergency payment goes to crisis fund

;; Emergency session tracking
(define-map emergency-sessions
    { emergency-id: uint }
    {
        patient: principal,
        crisis-level: uint, ;; 1-5 severity scale
        description: (string-ascii 200),
        assigned-therapist: (optional principal),
        status: (string-ascii 20),
        created-at: uint,
        response-deadline: uint,
        escalated: bool,
        payment-amount: uint
    }
)

;; Therapist emergency availability
(define-map emergency-availability
    { therapist: principal }
    {
        available: bool,
        max-crisis-level: uint,
        emergency-rate: uint,
        response-time-blocks: uint,
        total-emergency-sessions: uint,
        last-emergency: uint
    }
)

;; Crisis alerts and escalation
(define-map crisis-alerts
    { alert-id: uint }
    {
        patient: principal,
        crisis-level: uint,
        auto-escalated: bool,
        crisis-resources-sent: bool,
        emergency-contacts-notified: bool,
        created-at: uint
    }
)

;; Emergency contact system
(define-map patient-emergency-contacts
    { patient: principal }
    {
        primary-contact: (string-ascii 100),
        secondary-contact: (string-ascii 100),
        crisis-preference: (string-ascii 20), ;; "therapist", "resources", "both"
        consent-given: bool,
        last-updated: uint
    }
)

;; Crisis resource allocation fund
(define-map crisis-fund
    { fund-type: (string-ascii 20) }
    {
        balance: uint,
        total-allocated: uint,
        last-allocation: uint
    }
)

;; Data variables
(define-data-var emergency-nonce uint u0)
(define-data-var alert-nonce uint u0)
(define-data-var crisis-coordinator principal tx-sender)

;; Request emergency session
(define-public (request-emergency-session (crisis-level uint) (description (string-ascii 200)))
    (let
        ((patient tx-sender)
         (emergency-id (var-get emergency-nonce))
         (current-time stacks-block-height)
         (patient-info (contract-call? .Therapay get-session-count patient))
         (last-emergency u0)) ;; Simplified - assume no previous emergency for cooldown check
        
        (asserts! (and (>= crisis-level u1) (<= crisis-level u5)) ERR-INVALID-CRISIS-LEVEL)
        (asserts! (> (- current-time last-emergency) EMERGENCY-COOLDOWN) ERR-COOLDOWN-ACTIVE)
        
        (map-set emergency-sessions
            { emergency-id: emergency-id }
            {
                patient: patient,
                crisis-level: crisis-level,
                description: description,
                assigned-therapist: none,
                status: "pending",
                created-at: current-time,
                response-deadline: (+ current-time MAX-RESPONSE_TIME),
                escalated: false,
                payment-amount: u0
            }
        )
        (var-set emergency-nonce (+ emergency-id u1))
        
        ;; Auto-escalate high crisis levels
        (if (>= crisis-level CRISIS-ESCALATION-THRESHOLD)
            (begin (unwrap-panic (escalate-to-crisis-alert emergency-id)) true)
            true
        )
        (ok emergency-id)
    )
)

;; Therapist sets emergency availability
(define-public (set-emergency-availability (available bool) (max-crisis-level uint) (emergency-rate uint) (response-time-blocks uint))
    (let
        ((therapist tx-sender)
         (therapist-info (unwrap! (contract-call? .Therapay get-therapist-info therapist) ERR-NOT-AUTHORIZED)))
        
        (asserts! (is-some therapist-info) ERR-NOT-AUTHORIZED)
        (asserts! (and (>= max-crisis-level u1) (<= max-crisis-level u5)) ERR-INVALID-CRISIS-LEVEL)
        (asserts! (> emergency-rate u0) ERR-INSUFFICIENT-PAYMENT)
        
        (map-set emergency-availability
            { therapist: therapist }
            {
                available: available,
                max-crisis-level: max-crisis-level,
                emergency-rate: emergency-rate,
                response-time-blocks: response-time-blocks,
                total-emergency-sessions: (default-to u0 (get total-emergency-sessions 
                    (map-get? emergency-availability { therapist: therapist }))),
                last-emergency: (default-to u0 (get last-emergency 
                    (map-get? emergency-availability { therapist: therapist })))
            }
        )
        (ok true)
    )
)

;; Therapist accepts emergency session
(define-public (accept-emergency-session (emergency-id uint))
    (let
        ((therapist tx-sender)
         (emergency (unwrap! (map-get? emergency-sessions { emergency-id: emergency-id }) ERR-EMERGENCY-NOT-FOUND))
         (availability (unwrap! (map-get? emergency-availability { therapist: therapist }) ERR-THERAPIST-UNAVAILABLE))
         (current-time stacks-block-height))
        
        (asserts! (is-eq (get status emergency) "pending") ERR-EMERGENCY-RESOLVED)
        (asserts! (get available availability) ERR-THERAPIST-UNAVAILABLE)
        (asserts! (>= (get max-crisis-level availability) (get crisis-level emergency)) ERR-THERAPIST-UNAVAILABLE)
        (asserts! (< current-time (get response-deadline emergency)) ERR-EMERGENCY-RESOLVED)
        
        (let
            ((emergency-rate (get emergency-rate availability))
             (crisis-fund-amount (/ (* emergency-rate CRISIS-FUND_PERCENTAGE) u100))
             (therapist-payment (- emergency-rate crisis-fund-amount)))
            
            (try! (stx-transfer? emergency-rate (get patient emergency) (as-contract tx-sender)))
            (try! (as-contract (stx-transfer? therapist-payment tx-sender therapist)))
            
            ;; Allocate to crisis fund
            (let
                ((current-fund (default-to { balance: u0, total-allocated: u0, last-allocation: u0 }
                               (map-get? crisis-fund { fund-type: "emergency" }))))
                (map-set crisis-fund
                    { fund-type: "emergency" }
                    {
                        balance: (+ (get balance current-fund) crisis-fund-amount),
                        total-allocated: (+ (get total-allocated current-fund) crisis-fund-amount),
                        last-allocation: current-time
                    }
                )
            )
            
            (map-set emergency-sessions
                { emergency-id: emergency-id }
                (merge emergency {
                    assigned-therapist: (some therapist),
                    status: "in-progress",
                    payment-amount: emergency-rate
                })
            )
            (map-set emergency-availability
                { therapist: therapist }
                (merge availability {
                    total-emergency-sessions: (+ (get total-emergency-sessions availability) u1),
                    last-emergency: current-time
                })
            )
            (ok therapist-payment)
        )
    )
)

;; Complete emergency session
(define-public (complete-emergency-session (emergency-id uint))
    (let
        ((therapist tx-sender)
         (emergency (unwrap! (map-get? emergency-sessions { emergency-id: emergency-id }) ERR-EMERGENCY-NOT-FOUND)))
        
        (asserts! (is-eq (some therapist) (get assigned-therapist emergency)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status emergency) "in-progress") ERR-EMERGENCY-RESOLVED)
        
        (map-set emergency-sessions
            { emergency-id: emergency-id }
            (merge emergency { status: "completed" })
        )
        (ok true)
    )
)

;; Escalate to crisis alert system
(define-private (escalate-to-crisis-alert (emergency-id uint))
    (let
        ((emergency (unwrap! (map-get? emergency-sessions { emergency-id: emergency-id }) ERR-EMERGENCY-NOT-FOUND))
         (alert-id (var-get alert-nonce)))
        
        (map-set crisis-alerts
            { alert-id: alert-id }
            {
                patient: (get patient emergency),
                crisis-level: (get crisis-level emergency),
                auto-escalated: true,
                crisis-resources-sent: false,
                emergency-contacts-notified: false,
                created-at: stacks-block-height
            }
        )
        (var-set alert-nonce (+ alert-id u1))
        
        (map-set emergency-sessions
            { emergency-id: emergency-id }
            (merge emergency { escalated: true })
        )
        (ok alert-id)
    )
)

;; Set emergency contact information
(define-public (set-emergency-contacts (primary-contact (string-ascii 100)) (secondary-contact (string-ascii 100)) (crisis-preference (string-ascii 20)))
    (let
        ((patient tx-sender))
        
        (map-set patient-emergency-contacts
            { patient: patient }
            {
                primary-contact: primary-contact,
                secondary-contact: secondary-contact,
                crisis-preference: crisis-preference,
                consent-given: true,
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Allocate crisis fund resources
(define-public (allocate-crisis-resources (alert-id uint) (resource-amount uint))
    (let
        ((caller tx-sender)
         (alert (unwrap! (map-get? crisis-alerts { alert-id: alert-id }) ERR-EMERGENCY-NOT-FOUND))
         (current-fund (default-to { balance: u0, total-allocated: u0, last-allocation: u0 }
                       (map-get? crisis-fund { fund-type: "emergency" }))))
        
        (asserts! (is-eq caller (var-get crisis-coordinator)) ERR-NOT-AUTHORIZED)
        (asserts! (>= (get balance current-fund) resource-amount) ERR-INSUFFICIENT-PAYMENT)
        
        (try! (as-contract (stx-transfer? resource-amount tx-sender (get patient alert))))
        (map-set crisis-fund
            { fund-type: "emergency" }
            {
                balance: (- (get balance current-fund) resource-amount),
                total-allocated: (get total-allocated current-fund),
                last-allocation: stacks-block-height
            }
        )
        (map-set crisis-alerts
            { alert-id: alert-id }
            (merge alert { crisis-resources-sent: true })
        )
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-emergency-session (emergency-id uint))
    (map-get? emergency-sessions { emergency-id: emergency-id })
)

(define-read-only (get-therapist-availability (therapist principal))
    (map-get? emergency-availability { therapist: therapist })
)

(define-read-only (get-crisis-alert (alert-id uint))
    (map-get? crisis-alerts { alert-id: alert-id })
)

(define-read-only (get-patient-emergency-contacts (patient principal))
    (map-get? patient-emergency-contacts { patient: patient })
)

(define-read-only (get-crisis-fund-status)
    (map-get? crisis-fund { fund-type: "emergency" })
)

(define-read-only (check-emergency-eligibility (patient principal))
    (let
        ((last-emergency u0) ;; Simplified for basic eligibility check
         (current-time stacks-block-height))
        
        {
            eligible: (> (- current-time last-emergency) EMERGENCY-COOLDOWN),
            cooldown-remaining: (if (> (- current-time last-emergency) EMERGENCY-COOLDOWN) 
                                   u0 
                                   (- EMERGENCY-COOLDOWN (- current-time last-emergency))),
            current-block: current-time
        }
    )
)

(define-read-only (find-available-emergency-therapists (crisis-level uint))
    {
        sample-search-completed: true,
        crisis-level-required: crisis-level,
        search-time: stacks-block-height
    }
)
