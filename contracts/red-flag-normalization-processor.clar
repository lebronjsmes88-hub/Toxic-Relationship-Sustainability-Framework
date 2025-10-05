;; Red Flag Normalization Processor Smart Contract
;; Converts obvious warning signs into "quirky personality traits"

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-INPUT (err u101))
(define-constant ERR-FLAG-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-NORMALIZED (err u103))
(define-constant ERR-INSUFFICIENT-NORMALIZATION-POINTS (err u104))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data variables
(define-data-var normalization-counter uint u0)
(define-data-var total-flags-processed uint u0)
(define-data-var active-normalizations uint u0)

;; Data maps
(define-map red-flag-patterns
    { pattern-id: uint }
    {
        original-flag: (string-ascii 256),
        normalized-trait: (string-ascii 256),
        severity-level: uint,
        normalization-effectiveness: uint,
        usage-count: uint,
        creator: principal,
        is-active: bool
    }
)

(define-map user-normalizations
    { user: principal }
    {
        total-normalizations: uint,
        normalization-points: uint,
        reputation-score: uint,
        last-activity: uint,
        is-premium: bool
    }
)

(define-map normalization-history
    { normalization-id: uint }
    {
        user: principal,
        pattern-id: uint,
        timestamp: uint,
        effectiveness-rating: uint,
        relationship-context: (string-ascii 128)
    }
)

(define-map pattern-categories
    { category: (string-ascii 64) }
    {
        description: (string-ascii 256),
        pattern-count: uint,
        average-effectiveness: uint,
        is-premium-category: bool
    }
)

;; Private functions
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (calculate-normalization-points (severity uint) (effectiveness uint))
    (+ (* severity u10) (* effectiveness u5))
)

(define-private (validate-string-length (input (string-ascii 256)))
    (and (> (len input) u0) (<= (len input) u256))
)

(define-private (update-category-stats (category (string-ascii 64)) (effectiveness uint))
    (let (
        (current-category (default-to 
            { description: "", pattern-count: u0, average-effectiveness: u0, is-premium-category: false }
            (map-get? pattern-categories { category: category })
        ))
        (new-count (+ (get pattern-count current-category) u1))
        (new-average (/ (+ (* (get average-effectiveness current-category) (get pattern-count current-category)) effectiveness) new-count))
    )
        (map-set pattern-categories { category: category }
            (merge current-category {
                pattern-count: new-count,
                average-effectiveness: new-average
            })
        )
    )
)

(define-private (award-normalization-points (user principal) (points uint))
    (let (
        (current-user (default-to 
            { total-normalizations: u0, normalization-points: u0, reputation-score: u0, last-activity: u0, is-premium: false }
            (map-get? user-normalizations { user: user })
        ))
        (new-total (+ (get total-normalizations current-user) u1))
        (new-points (+ (get normalization-points current-user) points))
        (new-reputation (+ (get reputation-score current-user) (/ points u2)))
    )
        (map-set user-normalizations { user: user }
            (merge current-user {
                total-normalizations: new-total,
                normalization-points: new-points,
                reputation-score: new-reputation,
                last-activity: stacks-block-height
            })
        )
    )
)

;; Public functions
(define-public (create-normalization-pattern 
    (original-flag (string-ascii 256))
    (normalized-trait (string-ascii 256))
    (severity-level uint)
    (category (string-ascii 64))
)
    (let (
        (pattern-id (+ (var-get normalization-counter) u1))
        (effectiveness (+ (* severity-level u15) u50))
        (points (calculate-normalization-points severity-level effectiveness))
    )
        (asserts! (validate-string-length original-flag) ERR-INVALID-INPUT)
        (asserts! (validate-string-length normalized-trait) ERR-INVALID-INPUT)
        (asserts! (and (>= severity-level u1) (<= severity-level u10)) ERR-INVALID-INPUT)
        
        (map-set red-flag-patterns { pattern-id: pattern-id }
            {
                original-flag: original-flag,
                normalized-trait: normalized-trait,
                severity-level: severity-level,
                normalization-effectiveness: effectiveness,
                usage-count: u0,
                creator: tx-sender,
                is-active: true
            }
        )
        
        (update-category-stats category effectiveness)
        (award-normalization-points tx-sender points)
        (var-set normalization-counter pattern-id)
        (var-set total-flags-processed (+ (var-get total-flags-processed) u1))
        
        (ok pattern-id)
    )
)

(define-public (normalize-red-flag 
    (pattern-id uint)
    (relationship-context (string-ascii 128))
    (effectiveness-rating uint)
)
    (let (
        (pattern (unwrap! (map-get? red-flag-patterns { pattern-id: pattern-id }) ERR-FLAG-NOT-FOUND))
        (normalization-id (+ (var-get active-normalizations) u1))
        (user-data (default-to 
            { total-normalizations: u0, normalization-points: u0, reputation-score: u0, last-activity: u0, is-premium: false }
            (map-get? user-normalizations { user: tx-sender })
        ))
    )
        (asserts! (get is-active pattern) ERR-ALREADY-NORMALIZED)
        (asserts! (and (>= effectiveness-rating u1) (<= effectiveness-rating u10)) ERR-INVALID-INPUT)
        (asserts! (>= (get normalization-points user-data) u10) ERR-INSUFFICIENT-NORMALIZATION-POINTS)
        
        ;; Record the normalization
        (map-set normalization-history { normalization-id: normalization-id }
            {
                user: tx-sender,
                pattern-id: pattern-id,
                timestamp: stacks-block-height,
                effectiveness-rating: effectiveness-rating,
                relationship-context: relationship-context
            }
        )
        
        ;; Update pattern usage
        (map-set red-flag-patterns { pattern-id: pattern-id }
            (merge pattern {
                usage-count: (+ (get usage-count pattern) u1),
                normalization-effectiveness: (/ (+ (* (get normalization-effectiveness pattern) (get usage-count pattern)) effectiveness-rating) (+ (get usage-count pattern) u1))
            })
        )
        
        ;; Award points based on effectiveness
        (award-normalization-points tx-sender (+ effectiveness-rating u5))
        (var-set active-normalizations normalization-id)
        
        (ok {
            original-flag: (get original-flag pattern),
            normalized-trait: (get normalized-trait pattern),
            effectiveness: (get normalization-effectiveness pattern)
        })
    )
)

(define-public (upgrade-to-premium)
    (let (
        (user-data (default-to 
            { total-normalizations: u0, normalization-points: u0, reputation-score: u0, last-activity: u0, is-premium: false }
            (map-get? user-normalizations { user: tx-sender })
        ))
    )
        (asserts! (>= (get normalization-points user-data) u100) ERR-INSUFFICIENT-NORMALIZATION-POINTS)
        (asserts! (not (get is-premium user-data)) ERR-ALREADY-NORMALIZED)
        
        (map-set user-normalizations { user: tx-sender }
            (merge user-data {
                is-premium: true,
                normalization-points: (- (get normalization-points user-data) u100)
            })
        )
        
        (ok true)
    )
)

(define-public (deactivate-pattern (pattern-id uint))
    (let (
        (pattern (unwrap! (map-get? red-flag-patterns { pattern-id: pattern-id }) ERR-FLAG-NOT-FOUND))
    )
        (asserts! (or (is-contract-owner) (is-eq tx-sender (get creator pattern))) ERR-NOT-AUTHORIZED)
        
        (map-set red-flag-patterns { pattern-id: pattern-id }
            (merge pattern { is-active: false })
        )
        
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-pattern (pattern-id uint))
    (map-get? red-flag-patterns { pattern-id: pattern-id })
)

(define-read-only (get-user-stats (user principal))
    (map-get? user-normalizations { user: user })
)

(define-read-only (get-normalization-history (normalization-id uint))
    (map-get? normalization-history { normalization-id: normalization-id })
)

(define-read-only (get-category-stats (category (string-ascii 64)))
    (map-get? pattern-categories { category: category })
)

(define-read-only (get-contract-stats)
    {
        total-patterns: (var-get normalization-counter),
        total-flags-processed: (var-get total-flags-processed),
        active-normalizations: (var-get active-normalizations)
    }
)

(define-read-only (calculate-toxicity-score (user principal))
    (let (
        (user-data (default-to 
            { total-normalizations: u0, normalization-points: u0, reputation-score: u0, last-activity: u0, is-premium: false }
            (map-get? user-normalizations { user: user })
        ))
    )
        (+ (* (get total-normalizations user-data) u3)
           (* (get reputation-score user-data) u2)
           (if (get is-premium user-data) u50 u0)
        )
    )
)

;; title: red-flag-normalization-processor
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

