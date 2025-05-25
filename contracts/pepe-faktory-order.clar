;; Pepe Faktory order
;; Private until execution - MEV bots can't predict when/where orders will hit
;; Users sign intents off-chain (no public mempool exposure)
;; Eliminates front-running and sandwich attacks
;; Built-in slippage protection with `min_out` parameters
;; Predictable execution - get the price you expect
 
(define-constant structured-data-prefix 0x534950303138) ;; SIP-018 
(define-constant message-domain {name: "PEPE_FAKTORY_ORDER", version: "v1.0", chain-id: chain-id})
(define-constant message-domain-hash (sha256 (unwrap-panic (to-consensus-buff? message-domain))))
(define-constant structured-data-header (concat structured-data-prefix message-domain-hash))

(define-constant ERR_INSUFFICIENT_BALANCE (err u400))
(define-constant ERR_INVALID_SIGNATURE (err u401))
(define-constant ERR_UUID_SUBMITTED (err u402))
(define-constant ERR_CONSENSUS_BUFF (err u403))
(define-constant ERR_TOO_MUCH_SLIPPAGE (err u407))

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

(define-private (who-signed
    (intent (string-ascii 32))
    (amount uint)
    (min-out uint)
    (uuid (string-ascii 36))
    (signature (buff 65)))
  (let ((message-hash (hash-message intent amount min-out uuid)))
    (match (secp256k1-recover? message-hash signature)
      public-key (principal-of? public-key)
      error ERR_INVALID_SIGNATURE)))

;; === DEPOSIT FUNCTIONS ===
(define-public (deposit-sbtc (amount uint) (recipient (optional principal)))
  (let ((sender tx-sender)
        (recv   (default-to sender recipient))
        (prev-bal (sbtc-balance-of recv)))
    (try! (contract-call? 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token transfer 
           amount sender (as-contract tx-sender) none))
    (map-set sbtc-balances recv (+ prev-bal amount))
    (print {event: "deposit", sender: sender, recipient: recv, amount: amount, token: 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token, domain: .pepe-faktory-order})
    (ok true)))

(define-public (deposit-pepe (amount uint) (recipient (optional principal)))
  (let ((sender tx-sender)
        (recv   (default-to sender recipient))
        (prev-bal (pepe-balance-of recv)))
    (try! (contract-call? 'SP1Z92MPDQEWZXW36VX71Q25HKF5K2EPCJ304F275.tokensoft-token-v4k68639zxz transfer 
           amount sender (as-contract tx-sender) none))
    (map-set pepe-balances recv (+ prev-bal amount))
    (print {event: "deposit", sender: sender, recipient: recv, amount: amount, token: 'SP1Z92MPDQEWZXW36VX71Q25HKF5K2EPCJ304F275.tokensoft-token-v4k68639zxz, domain: .pepe-faktory-order})
    (ok true)))

;; === LIMIT ORDER EXECUTION ===
(define-public (execute-limit-buy
    (sbtc-amount uint)
    (min-pepe-out uint)
    (uuid (string-ascii 36))
    (signature (buff 65)))
    (if (map-insert submitted-uuids uuid true)
            (let ((signer (try! (who-signed "LIMIT_BUY" sbtc-amount min-pepe-out uuid signature)))
                  (user-sbtc-bal (sbtc-balance-of signer))
                  (user-pepe-bal (pepe-balance-of signer))
                  ((swap-result (try! (as-contract (contract-call? 'SP6SA6BTPNN5WDAWQ7GWJF1T5E2KWY01K9SZDBJQ.pepe-faktory-pool 
                                          swap-a-to-b sbtc-amount min-pepe-out)))))
                  (pepe-received (get dy swap-result)))
                 (asserts! (>= user-sbtc-bal sbtc-amount) ERR_INSUFFICIENT_BALANCE)
                 (map-set sbtc-balances signer (- user-sbtc-bal sbtc-amount))
                 (map-set pepe-balances signer (+ user-pepe-bal pepe-received))
                 (print {event: "execute-limit-buy", 
                         operator: tx-sender, 
                         signer: signer, 
                         token-a: 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token,
                         amount-a: sbtc-amount,
                         token-b: 'SP1Z92MPDQEWZXW36VX71Q25HKF5K2EPCJ304F275.tokensoft-token-v4k68639zxz
                         amount-b: pepe-received})
                 (ok pepe-received))
            ERR_UUID_SUBMITTED))

(define-public (execute-limit-sell
    (pepe-amount uint)
    (min-sbtc-out uint)
    (uuid (string-ascii 36))
    (signature (buff 65)))
    (if (map-insert submitted-uuids uuid true)
    (let ((signer (try! (who-signed "LIMIT_SELL" pepe-amount min-sbtc-out uuid signature)))
          (user-pepe-bal (pepe-balance-of signer))
          (user-sbtc-bal (sbtc-balance-of signer))
          ((swap-result (try! (contract-call? 'SP6SA6BTPNN5WDAWQ7GWJF1T5E2KWY01K9SZDBJQ.pepe-faktory-pool 
                                swap-b-to-a pepe-amount))))
          (sbtc-received (get dy swap-result)))
         (asserts! (>= user-pepe-bal pepe-amount) ERR_INSUFFICIENT_BALANCE)
         (asserts! (>= sbtc-received min-sbtc-out) ERR_TOO_MUCH_SLIPPAGE)
         (map-set pepe-balances signer (- user-pepe-bal pepe-amount))
         (map-set sbtc-balances signer (+ user-sbtc-bal sbtc-received))
         (print {event: "execute-limit-sell", 
                 operator: tx-sender, 
                 signer: signer, 
                 token-a: 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token,
                 amount-a: sbtc-amount,
                 token-b: 'SP1Z92MPDQEWZXW36VX71Q25HKF5K2EPCJ304F275.tokensoft-token-v4k68639zxz
                 amount-b: pepe-received})
         (ok sbtc-received))
    ERR_UUID_SUBMITTED))

;; === WITHDRAW FUNCTIONS ===
(define-public (withdraw-sbtc (amount uint) (recipient (optional principal)))
  (let ((sender tx-sender)
        (recv  (default-to sender recipient))
        (bal (sbtc-balance-of recv)))
    (asserts! (>= bal amount) ERR_INSUFFICIENT_BALANCE)
    (map-set sbtc-balances sender (- bal amount))
    (try! (as-contract (contract-call? 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token transfer 
           amount tx-sender recv none)))
    (print {event: "withdraw", sender: sender, recipient: recv, amount: amount, token: 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token, domain: .pepe-faktory-order})
    (ok true)))

(define-public (withdraw-pepe (amount uint) (recipient (optional principal)))
  (let ((sender tx-sender)
        (recv  (default-to sender recipient))
        (bal (pepe-balance-of recv)))
    (asserts! (>= bal amount) ERR_INSUFFICIENT_BALANCE)
    (map-set pepe-balances sender (- bal amount))
    (try! (as-contract (contract-call? 'SP1Z92MPDQEWZXW36VX71Q25HKF5K2EPCJ304F275.tokensoft-token-v4k68639zxz transfer 
           amount tx-sender recv none)))
    (print {event: "withdraw", sender: sender, recipient: recv, amount: amount, token: 'SP1Z92MPDQEWZXW36VX71Q25HKF5K2EPCJ304F275.tokensoft-token-v4k68639zxz, domain: .pepe-faktory-order})
    (ok true)))

;; === HELPER FUNCTIONS ===
(define-read-only (get-sbtc-balance (user principal))
  (ok (sbtc-balance-of user)))

(define-read-only (get-pepe-balance (user principal))
  (ok (pepe-balance-of user)))

(define-private (sbtc-balance-of (who principal))
  (default-to u0 (map-get? sbtc-balances who))
)

(define-private (pepe-balance-of (who principal))
  (default-to u0 (map-get? pepe-balances who))
)

(define-read-only (check (uuid (string-ascii 36)))
  (is-some (map-get? submitted-uuids uuid))
)
