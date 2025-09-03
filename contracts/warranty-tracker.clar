
;; title: warranty-tracker
;; version: 1.0.0
;; summary: Product Warranty Management System
;; description: A smart contract for managing product warranties, tracking warranty claims, and coordinating repairs

;; constants
;;
(define-constant ERR_NOT_AUTHORIZED (err u1000))
(define-constant ERR_EXPIRED_WARRANTY (err u1001))
(define-constant ERR_INVALID_PRODUCT (err u1002))
(define-constant ERR_CLAIM_NOT_FOUND (err u1003))
(define-constant ERR_ALREADY_CLAIMED (err u1004))
(define-constant ERR_REPAIR_NOT_NEEDED (err u1005))
(define-constant ERR_RETAILER_NOT_FOUND (err u1006))
(define-constant ERR_PRODUCT_EXISTS (err u1007))

;; data vars
;;
(define-data-var last-product-id uint u0)
(define-data-var last-claim-id uint u0)
(define-data-var last-retailer-id uint u0)

;; data maps
;;

;; Product details
(define-map products uint {
  name: (string-ascii 100),
  manufacturer: principal,
  creation-time: uint,
  warranty-duration: uint,  ;; warranty duration in days
  retailer-id: uint
})

;; Retailer details
(define-map retailers uint {
  name: (string-ascii 100),
  address: (string-ascii 200),
  principal: principal
})

;; Warranty claims
(define-map warranty-claims uint {
  product-id: uint,
  owner: principal,
  claim-time: uint,
  description: (string-utf8 500),
  status: (string-ascii 20),  ;; "pending", "approved", "rejected", "completed"
  repair-assigned: bool
})

;; Product ownership
(define-map product-ownership {product-id: uint} {
  owner: principal,
  purchase-time: uint,
  warranty-active: bool
})

;; Repair services
(define-map repair-services {claim-id: uint} {
  service-provider: principal,
  scheduled-time: uint,
  notes: (string-utf8 500),
  completed: bool
})

;; public functions
;;

;; Add a retailer to the system
(define-public (register-retailer (name (string-ascii 100)) (address (string-ascii 200)))
  (let
    (
      (retailer-id (+ (var-get last-retailer-id) u1))
    )
    (asserts! (is-eq tx-sender contract-caller) ERR_NOT_AUTHORIZED)
    (map-insert retailers retailer-id {
      name: name,
      address: address,
      principal: tx-sender
    })
    (ok (var-set last-retailer-id retailer-id))
  )
)

;; Register a new product with warranty information
(define-public (register-product
    (name (string-ascii 100))
    (warranty-duration uint)
    (retailer-id uint)
  )
  (let
    (
      (product-id (+ (var-get last-product-id) u1))
    )
    ;; Verify retailer exists
    (asserts! (is-some (map-get? retailers retailer-id)) ERR_RETAILER_NOT_FOUND)
    
    ;; Add product to the registry
    (map-insert products product-id {
      name: name,
      manufacturer: tx-sender,
      creation-time: stacks-block-height,
      warranty-duration: warranty-duration,
      retailer-id: retailer-id
    })
    
    ;; Update the last product ID
    (var-set last-product-id product-id)
    (ok product-id)
  )
)

;; Record product purchase and activate warranty
(define-public (record-purchase (product-id uint))
  (let
    (
      (product (unwrap! (map-get? products product-id) ERR_INVALID_PRODUCT))
      (ownership-data {
        owner: tx-sender,
        purchase-time: stacks-block-height,
        warranty-active: true
      })
    )
    ;; Make sure the product isn't already owned
    (asserts! (is-none (map-get? product-ownership {product-id: product-id})) ERR_PRODUCT_EXISTS)
    
    ;; Record ownership and activate warranty
    (map-insert product-ownership {product-id: product-id} ownership-data)
    (ok true)
  )
)

;; File a warranty claim
(define-public (file-warranty-claim (product-id uint) (description (string-utf8 500)))
  (let
    (
      (product (unwrap! (map-get? products product-id) ERR_INVALID_PRODUCT))
      (ownership (unwrap! (map-get? product-ownership {product-id: product-id}) ERR_INVALID_PRODUCT))
      (current-time stacks-block-height)
      (warranty-end-time (+ (get purchase-time ownership) (get warranty-duration product)))
      (claim-id (+ (var-get last-claim-id) u1))
    )
    ;; Make sure the claimer is the owner
    (asserts! (is-eq (get owner ownership) tx-sender) ERR_NOT_AUTHORIZED)
    
    ;; Check warranty is still active and not expired
    (asserts! (get warranty-active ownership) ERR_EXPIRED_WARRANTY)
    (asserts! (<= current-time warranty-end-time) ERR_EXPIRED_WARRANTY)
    
    ;; Create the claim
    (map-insert warranty-claims claim-id {
      product-id: product-id,
      owner: tx-sender,
      claim-time: current-time,
      description: description,
      status: "pending",
      repair-assigned: false
    })
    
    ;; Update the last claim ID
    (var-set last-claim-id claim-id)
    (ok claim-id)
  )
)

;; Approve or reject a warranty claim (manufacturer only)
(define-public (process-warranty-claim (claim-id uint) (approve bool))
  (let
    (
      (claim (unwrap! (map-get? warranty-claims claim-id) ERR_CLAIM_NOT_FOUND))
      (product (unwrap! (map-get? products (get product-id claim)) ERR_INVALID_PRODUCT))
      (new-status (if approve "approved" "rejected"))
    )
    ;; Only the manufacturer can approve/reject claims
    (asserts! (is-eq tx-sender (get manufacturer product)) ERR_NOT_AUTHORIZED)
    
    ;; Update claim status
    (map-set warranty-claims claim-id (merge claim {status: new-status}))
    (ok true)
  )
)

;; Assign repair service to an approved claim
(define-public (assign-repair-service 
    (claim-id uint) 
    (service-provider principal) 
    (scheduled-time uint) 
    (notes (string-utf8 500))
  )
  (let
    (
      (claim (unwrap! (map-get? warranty-claims claim-id) ERR_CLAIM_NOT_FOUND))
      (product (unwrap! (map-get? products (get product-id claim)) ERR_INVALID_PRODUCT))
    )
    ;; Only manufacturer can assign repairs
    (asserts! (is-eq tx-sender (get manufacturer product)) ERR_NOT_AUTHORIZED)
    
    ;; Claim must be in approved status
    (asserts! (is-eq (get status claim) "approved") ERR_REPAIR_NOT_NEEDED)
    
    ;; Can't assign a repair if already assigned
    (asserts! (not (get repair-assigned claim)) ERR_ALREADY_CLAIMED)
    
    ;; Assign repair service
    (map-set repair-services {claim-id: claim-id} {
      service-provider: service-provider,
      scheduled-time: scheduled-time,
      notes: notes,
      completed: false
    })
    
    ;; Update claim to show repair is assigned
    (map-set warranty-claims claim-id (merge claim {repair-assigned: true}))
    (ok true)
  )
)

;; Mark repair as completed (repair service provider only)
(define-public (complete-repair (claim-id uint))
  (let
    (
      (claim (unwrap! (map-get? warranty-claims claim-id) ERR_CLAIM_NOT_FOUND))
      (repair (unwrap! (map-get? repair-services {claim-id: claim-id}) ERR_REPAIR_NOT_NEEDED))
    )
    ;; Only assigned service provider can mark as complete
    (asserts! (is-eq tx-sender (get service-provider repair)) ERR_NOT_AUTHORIZED)
    
    ;; Update repair service as completed
    (map-set repair-services {claim-id: claim-id} (merge repair {completed: true}))
    
    ;; Update claim status to completed
    (map-set warranty-claims claim-id (merge claim {status: "completed"}))
    (ok true)
  )
)

;; read only functions
;;

;; Get product details
(define-read-only (get-product (product-id uint))
  (map-get? products product-id)
)

;; Get warranty status for a product
(define-read-only (get-warranty-status (product-id uint))
  (let
    (
      (product (map-get? products product-id))
      (ownership (map-get? product-ownership {product-id: product-id}))
    )
    (if (and (is-some product) (is-some ownership))
      (let
        (
          (product-data (unwrap-panic product))
          (ownership-data (unwrap-panic ownership))
          (purchase-time (get purchase-time ownership-data))
          (warranty-duration (get warranty-duration product-data))
          (warranty-end-time (+ purchase-time warranty-duration))
          (current-time stacks-block-height)
          (time-remaining (if (> warranty-end-time current-time)
                           (- warranty-end-time current-time)
                           u0))
          (is-active (and (get warranty-active ownership-data)
                         (>= warranty-end-time current-time)))
        )
        (some {
          is-active: is-active,
          purchase-time: purchase-time,
          warranty-end-time: warranty-end-time,
          time-remaining: time-remaining
        })
      )
      none
    )
  )
)

;; Get claim details
(define-read-only (get-claim (claim-id uint))
  (map-get? warranty-claims claim-id)
)

;; Get repair service details
(define-read-only (get-repair-service (claim-id uint))
  (map-get? repair-services {claim-id: claim-id})
)

;; Get retailer details
(define-read-only (get-retailer (retailer-id uint))
  (map-get? retailers retailer-id)
)

;; Get product ownership details
(define-read-only (get-product-ownership (product-id uint))
  (map-get? product-ownership {product-id: product-id})
)

;; Check if a user owns a product
(define-read-only (is-product-owner (product-id uint) (owner principal))
  (match (map-get? product-ownership {product-id: product-id})
    ownership (is-eq (get owner ownership) owner)
    false
  )
)

