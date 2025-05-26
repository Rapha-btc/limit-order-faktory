;; Pepe Faktory order - Blaze Subnet Integration Version
;; Uses existing sbtc-token-subnet-v1 and pepe-token-subnet-v1 for token management
;; Private until execution - MEV bots can't predict when/where orders will hit
;; Users sign intents off-chain (no public mempool exposure)
;; Eliminates front-running and sandwich attacks
;; Built-in slippage protection with `min_out` parameters
;; Predictable execution - get the price you expect
;; The user is signing the full trade terms including slippage protection, 
;; so operators can't execute at terrible prices!

;; Use blaze-v1 for signature verification and replay protection
(define-constant ERR_INSUFFICIENT_BALANCE (err u400))
(define-constant ERR_TOO_MUCH_SLIPPAGE (err u407))

;; Subnet contract references
(define-constant SBTC-SUBNET 'SP2ZNGJ85ENDY6QRHQ5P2D4FXKGZWCKTB2T0Z55KS.sbtc-token-subnet-v1)
(define-constant PEPE-SUBNET 'SP2ZNGJ85ENDY6QRHQ5P2D4FXKGZWCKTB2T0Z55KS.pepe-token-subnet-v1)
(define-constant BLAZE-V1 'SP2ZNGJ85ENDY6QRHQ5P2D4FXKGZWCKTB2T0Z55KS.blaze-v1)

;; === LIMIT ORDER EXECUTION ===
(define-public (execute-limit-buy
    (sbtc-amount uint)
    (min-pepe-out-as-bytes (optional (buff 16)))
    (uuid (string-ascii 36))
    (signature (buff 65)))
  (let ((signer (try! (contract-call? BLAZE-V1 execute signature "LIMIT_BUY" min-pepe-out-as-bytes (some sbtc-amount) (some (as-contract tx-sender)) uuid))) 
        (user-sbtc-bal (unwrap! (contract-call? SBTC-SUBNET get-balance signer) (err u500)))
        (min-pepe-out u100000) ;; decode min-pepe-out-as-bytes into min-pepe-out
        (swap-result (try! (as-contract (contract-call? 'SP6SA6BTPNN5WDAWQ7GWJF1T5E2KWY01K9SZDBJQ.pepe-faktory-pool 
                                        swap-a-to-b sbtc-amount min-pepe-out))))) ;; post condition baked in pool
    (try! (contract-call? SBTC-SUBNET x-transfer signature sbtc-amount uuid (as-contract tx-sender)))
    ;; then this contract must withdraw from sbtc subnet
    ;; swap a to b must happen after the above line
    (try! (as-contract (contract-call? PEPE-SUBNET deposit pepe-received (some signer))))
    (print {event: "execute-limit-buy", 
            operator: tx-sender, 
            signer: signer, 
            token-a: 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token,
            amount-a: sbtc-amount,
            token-b: 'SP1Z92MPDQEWZXW36VX71Q25HKF5K2EPCJ304F275.tokensoft-token-v4k68639zxz,
            amount-b: pepe-received})
    (ok pepe-received)))

(define-public (execute-limit-sell
    (pepe-amount uint)
    (min-sbtc-out-as-bytes (optional (buff 16)))
    (uuid (string-ascii 36))
    (signature (buff 65)))
  (let ((signer (try! (contract-call? BLAZE-V1 execute signature "LIMIT_SELL" min-sbtc-out-as-bytes (some pepe-amount) (some (as-contract tx-sender)) uuid)))
        (user-pepe-bal (unwrap! (contract-call? PEPE-SUBNET get-balance signer) (err u500)))
        (min-sbtc-out u100000) ;; decode 
        (swap-result (try! (as-contract (contract-call? 'SP6SA6BTPNN5WDAWQ7GWJF1T5E2KWY01K9SZDBJQ.pepe-faktory-pool 
                                        swap-b-to-a pepe-amount))))
        (sbtc-received (get dy swap-result)))
    (asserts! (>= user-pepe-bal pepe-amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (>= sbtc-received min-sbtc-out) ERR_TOO_MUCH_SLIPPAGE)
    (try! (contract-call? PEPE-SUBNET x-transfer signature pepe-amount uuid (as-contract tx-sender)))
    ;; no here we need to withdraw from pepe to this contract
    ;; then we need to swap b to a
    (try! (as-contract (contract-call? SBTC-SUBNET deposit sbtc-received (some signer))))
    (print {event: "execute-limit-sell", 
            operator: tx-sender, 
            signer: signer, 
            token-a: 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token,
            amount-a: sbtc-received,
            token-b: 'SP1Z92MPDQEWZXW36VX71Q25HKF5K2EPCJ304F275.tokensoft-token-v4k68639zxz,
            amount-b: pepe-amount})
    (ok sbtc-received)))

;; === HELPER FUNCTIONS ===
(define-read-only (get-sbtc-balance (user principal))
  (contract-call? SBTC-SUBNET get-balance user))

(define-read-only (get-pepe-balance (user principal))
  (contract-call? PEPE-SUBNET get-balance user))

(define-read-only (check-uuid (uuid (string-ascii 36)))
  (contract-call? BLAZE-V1 check uuid))