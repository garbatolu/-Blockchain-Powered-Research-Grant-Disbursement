(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-GRANT-NOT-FOUND (err u102))
(define-constant ERR-MILESTONE-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-REVIEWED (err u104))
(define-constant ERR-INSUFFICIENT-REVIEWS (err u105))

(define-data-var admin principal tx-sender)
(define-data-var min-reviewers uint u3)
(define-data-var review-threshold uint u2)

(define-map grants 
    { grant-id: uint } 
    {
        researcher: principal,
        total-amount: uint,
        remaining-amount: uint,
        milestone-count: uint,
        status: (string-ascii 20)
    }
)

(define-map milestones 
    { grant-id: uint, milestone-id: uint } 
    {
        description: (string-ascii 256),
        amount: uint,
        status: (string-ascii 20),
        review-count: uint,
        approved-count: uint
    }
)

(define-map reviews
    { grant-id: uint, milestone-id: uint, reviewer: principal }
    { approved: bool }
)

(define-data-var grant-nonce uint u0)

(define-public (create-grant (researcher principal) (total-amount uint) (milestone-count uint))
    (let ((grant-id (var-get grant-nonce)))
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (asserts! (> total-amount u0) ERR-INVALID-AMOUNT)
        (map-set grants
            { grant-id: grant-id }
            {
                researcher: researcher,
                total-amount: total-amount,
                remaining-amount: total-amount,
                milestone-count: milestone-count,
                status: "ACTIVE"
            }
        )
        (var-set grant-nonce (+ grant-id u1))
        (ok grant-id)
    )
)

(define-public (add-milestone (grant-id uint) (milestone-id uint) (description (string-ascii 256)) (amount uint))
    (let ((grant (unwrap! (map-get? grants { grant-id: grant-id }) ERR-GRANT-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (asserts! (<= amount (get remaining-amount grant)) ERR-INVALID-AMOUNT)
        (map-set milestones
            { grant-id: grant-id, milestone-id: milestone-id }
            {
                description: description,
                amount: amount,
                status: "PENDING",
                review-count: u0,
                approved-count: u0
            }
        )
        (ok true)
    )
)

(define-public (submit-review (grant-id uint) (milestone-id uint) (approved bool))
    (let (
        (milestone (unwrap! (map-get? milestones { grant-id: grant-id, milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
        (existing-review (map-get? reviews { grant-id: grant-id, milestone-id: milestone-id, reviewer: tx-sender }))
    )
        (asserts! (is-none existing-review) ERR-ALREADY-REVIEWED)
        (map-set reviews
            { grant-id: grant-id, milestone-id: milestone-id, reviewer: tx-sender }
            { approved: approved }
        )
        (map-set milestones
            { grant-id: grant-id, milestone-id: milestone-id }
            {
                description: (get description milestone),
                amount: (get amount milestone),
                status: (get status milestone),
                review-count: (+ (get review-count milestone) u1),
                approved-count: (if approved (+ (get approved-count milestone) u1) (get approved-count milestone))
            }
        )
        (ok true)
    )
)

(define-public (release-milestone-funds (grant-id uint) (milestone-id uint))
    (let (
        (milestone (unwrap! (map-get? milestones { grant-id: grant-id, milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
        (grant (unwrap! (map-get? grants { grant-id: grant-id }) ERR-GRANT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (asserts! (>= (get review-count milestone) (var-get min-reviewers)) ERR-INSUFFICIENT-REVIEWS)
        (asserts! (>= (get approved-count milestone) (var-get review-threshold)) ERR-INSUFFICIENT-REVIEWS)
        (map-set milestones
            { grant-id: grant-id, milestone-id: milestone-id }
            {
                description: (get description milestone),
                amount: (get amount milestone),
                status: "RELEASED",
                review-count: (get review-count milestone),
                approved-count: (get approved-count milestone)
            }
        )
        (map-set grants
            { grant-id: grant-id }
            {
                researcher: (get researcher grant),
                total-amount: (get total-amount grant),
                remaining-amount: (- (get remaining-amount grant) (get amount milestone)),
                milestone-count: (get milestone-count grant),
                status: (get status grant)
            }
        )
        (ok true)
    )
)

(define-public (set-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (var-set admin new-admin)
        (ok true)
    )
)