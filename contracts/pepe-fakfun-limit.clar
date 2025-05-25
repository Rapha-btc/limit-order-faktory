;; SP...limit-order-contract (with built-in signature verification)

(define-constant structured-data-prefix 0x534950303138)
(define-constant message-domain {name: "LIMIT_ORDER_FAKTORY", version: "v1.0", chain-id: chain-id})
(define-constant message-domain-hash (sha256 (unwrap-panic (to-consensus-buff? message-domain))))
(define-constant structured-data-header (concat structured-data-prefix message-domain-hash))

(define-constant ERR_INSUFFICIENT_BALANCE (err u400))
(define-constant ERR_INVALID_SIGNATURE (err u401))
(define-constant ERR_UUID_SUBMITTED (err u402))
(define-constant ERR_CONSENSUS_BUFF (err u403))


;; Replay protection
(define-map submitted-uuids (string-ascii 36) bool)

;; Balance tracking
(define-map sbtc-balances principal uint)
(define-map pepe-balances principal uint)

;; === SIGNATURE VERIFICATION ===
(define-private (hash-message
    (intent (string-ascii 32))
    (amount uint)
    (min-out uint)
    (uuid (string-ascii 36)))
  (sha256 (concat structured-data-header (sha256 
    (unwrap! (to-consensus-buff? {
      intent: intent,
      amount: amount,
      min-out: min-out,
      uuid: uuid
    }) ERR_CONSENSUS_BUFF)))))

(define-private (verify-signature
    (signature (buff 65))
    (intent (string-ascii 32))
    (amount uint)
    (min-out uint)
    (uuid (string-ascii 36)))
  (let ((message-hash (hash-message intent amount min-out uuid)))
    (match (secp256k1-recover? message-hash signature)
      public-key (principal-of? public-key)
      error ERR_INVALID_SIGNATURE)))

;; === DEPOSIT FUNCTIONS ===
(define-public (deposit-sbtc (amount uint))
  (let ((sender tx-sender)
        (prev-bal (get-sbtc-balance sender)))
    (try! (contract-call? 'SM3...sbtc-token transfer 
           amount sender (as-contract tx-sender) none))
    (map-set sbtc-balances sender (+ prev-bal amount))
    (ok true)))

;; === LIMIT ORDER EXECUTION ===
(define-public (execute-limit-buy
    (signature (buff 65))
    (sbtc-amount uint)
    (min-pepe-out uint)
    (uuid (string-ascii 36)))
  (begin
    ;; Check UUID hasn't been used
    (asserts! (is-none (map-get? submitted-uuids uuid)) ERR_UUID_SUBMITTED)
    
    ;; Verify signature and get signer
    (let ((signer (try! (verify-signature signature "LIMIT_BUY_SBTC" sbtc-amount min-pepe-out uuid)))
          (user-sbtc-bal (get-sbtc-balance signer)))
      
      ;; Mark UUID as used
      (map-set submitted-uuids uuid true)
      
      ;; Check balance
      (asserts! (>= user-sbtc-bal sbtc-amount) ERR_INSUFFICIENT_BALANCE)
      
      ;; Deduct from user's balance
      (map-set sbtc-balances signer (- user-sbtc-bal sbtc-amount))
      
      ;; Execute swap
      (let ((swap-result (try! (contract-call? 'SP...sBTC-PEPE-pool 
                                swap-a-to-b sbtc-amount min-pepe-out))))
        (let ((pepe-received (get dy swap-result)))
          ;; Credit user with PEPE
          (map-set pepe-balances signer 
                   (+ (get-pepe-balance signer) pepe-received))
          (ok pepe-received))))))

(define-public (execute-limit-sell
    (signature (buff 65))
    (pepe-amount uint)
    (min-sbtc-out uint)
    (uuid (string-ascii 36)))
  (begin
    ;; Check UUID hasn't been used
    (asserts! (is-none (map-get? submitted-uuids uuid)) ERR_UUID_SUBMITTED)
    
    ;; Verify signature and get signer
    (let ((signer (try! (verify-signature signature "LIMIT_SELL_PEPE" pepe-amount min-sbtc-out uuid)))
          (user-pepe-bal (get-pepe-balance signer)))
      
      ;; Mark UUID as used
      (map-set submitted-uuids uuid true)
      
      ;; Check balance
      (asserts! (>= user-pepe-bal pepe-amount) ERR_INSUFFICIENT_BALANCE)
      
      ;; Deduct from user's balance
      (map-set pepe-balances signer (- user-pepe-bal pepe-amount))
      
      ;; Execute swap
      (let ((swap-result (try! (contract-call? 'SP...sBTC-PEPE-pool 
                                swap-b-to-a pepe-amount min-sbtc-out))))
        (let ((sbtc-received (get dy swap-result)))
          ;; Credit user with sBTC
          (map-set sbtc-balances signer 
                   (+ (get-sbtc-balance signer) sbtc-received))
          (ok sbtc-received))))))

;; === WITHDRAW FUNCTIONS ===
(define-public (withdraw-sbtc (amount uint))
  (let ((sender tx-sender)
        (user-bal (get-sbtc-balance sender)))
    (asserts! (>= user-bal amount) ERR_INSUFFICIENT_BALANCE)
    (map-set sbtc-balances sender (- user-bal amount))
    (try! (as-contract (contract-call? 'SM3...sbtc-token transfer 
           amount tx-sender sender none)))
    (ok true)))

(define-public (withdraw-pepe (amount uint))
  (let ((sender tx-sender)
        (user-bal (get-pepe-balance sender)))
    (asserts! (>= user-bal amount) ERR_INSUFFICIENT_BALANCE)
    (map-set pepe-balances sender (- user-bal amount))
    (try! (as-contract (contract-call? 'SP...pepe-token transfer 
           amount tx-sender sender none)))
    (ok true)))

;; === HELPER FUNCTIONS ===
(define-read-only (get-sbtc-balance (user principal))
  (default-to u0 (map-get? sbtc-balances user)))

(define-read-only (get-pepe-balance (user principal))
  (default-to u0 (map-get? pepe-balances user)))

(define-read-only (check (uuid (string-ascii 36)))
  (is-some (map-get? submitted-uuids uuid))
)