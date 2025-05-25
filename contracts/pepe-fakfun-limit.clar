;; SP...limit-order-contract
(define-constant ERR_INSUFFICIENT_BALANCE (err u400))
(define-constant ERR_UNAUTHORIZED (err u401))

;; Internal balances for deposited tokens
(define-map sbtc-balances principal uint)
(define-map pepe-balances principal uint)

;; === DEPOSIT FUNCTIONS ===
(define-public (deposit-sbtc (amount uint))
  (let ((sender tx-sender)
        (prev-bal (get-sbtc-balance sender)))
    (try! (contract-call? 'SM3...sbtc-token transfer 
           amount sender (as-contract tx-sender) none))
    (map-set sbtc-balances sender (+ prev-bal amount))
    (ok true)))

(define-public (deposit-pepe (amount uint))
  (let ((sender tx-sender)
        (prev-bal (get-pepe-balance sender)))
    (try! (contract-call? 'SP...pepe-token transfer 
           amount sender (as-contract tx-sender) none))
    (map-set pepe-balances sender (+ prev-bal amount))
    (ok true)))

;; === LIMIT ORDER EXECUTION (using existing AMM pool) ===
(define-public (execute-limit-buy
    (signature (buff 65))
    (sbtc-amount uint)
    (uuid (string-ascii 36))
    (min-pepe-out uint))
  (let (
    ;; Blaze verifies signature and returns user
    (signer (try! (contract-call? 'SP...blaze-v1 execute 
                   signature "LIMIT_BUY_SBTC" none 
                   (some sbtc-amount) none uuid)))
    (user-sbtc-bal (get-sbtc-balance signer)))
    
    ;; Check user has enough deposited sBTC
    (asserts! (>= user-sbtc-bal sbtc-amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Deduct from user's balance
    (map-set sbtc-balances signer (- user-sbtc-bal sbtc-amount))
    
    ;; Swap using existing AMM pool
    (let ((swap-result (try! (contract-call? 'SP...sBTC-PEPE-pool 
                              swap-a-to-b sbtc-amount min-pepe-out))))
      
      ;; Credit user with received PEPE
      (let ((pepe-received (get dy swap-result)))
        (map-set pepe-balances signer 
                 (+ (get-pepe-balance signer) pepe-received))
        (ok pepe-received)))))

(define-public (execute-limit-sell
    (signature (buff 65))
    (pepe-amount uint)
    (uuid (string-ascii 36))
    (min-sbtc-out uint))
  (let (
    (signer (try! (contract-call? 'SP...blaze-v1 execute 
                   signature "LIMIT_SELL_PEPE" none 
                   (some pepe-amount) none uuid)))
    (user-pepe-bal (get-pepe-balance signer)))
    
    (asserts! (>= user-pepe-bal pepe-amount) ERR_INSUFFICIENT_BALANCE)
    (map-set pepe-balances signer (- user-pepe-bal pepe-amount))
    
    ;; Swap using existing AMM pool  
    (let ((swap-result (try! (contract-call? 'SP...sBTC-PEPE-pool 
                              swap-b-to-a pepe-amount min-sbtc-out))))
      (let ((sbtc-received (get dy swap-result)))
        (map-set sbtc-balances signer 
                 (+ (get-sbtc-balance signer) sbtc-received))
        (ok sbtc-received)))))

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