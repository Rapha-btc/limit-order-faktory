(define-constant ALL_HEX 0x000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F202122232425262728292A2B2C2D2E2F303132333435363738393A3B3C3D3E3F404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F606162636465666768696A6B6C6D6E6F707172737475767778797A7B7C7D7E7F808182838485868788898A8B8C8D8E8F909192939495969798999A9B9C9D9E9FA0A1A2A3A4A5A6A7A8A9AAABACADAEAFB0B1B2B3B4B5B6B7B8B9BABBBCBDBEBFC0C1C2C3C4C5C6C7C8C9CACBCCCDCECFD0D1D2D3D4D5D6D7D8D9DADBDCDDDEDFE0E1E2E3E4E5E6E7E8E9EAEBECEDEEEFF0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF)
(define-constant BASE58_CHARS "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
(define-constant STX_VER 0x16141a15)
(define-constant BTC_VER 0x00056fc4)

(define-constant ERR_INVALID_CHAR (err {error_code: u123123, value: none}))
(define-constant ERR_TO_SHORT (err {error_code: u234234, value: none}))
(define-constant ERR_BAD_CHECKSUM (err {error_code: u345345, value: none}))
(define-constant ERR_INVALID_VERSION (err {error_code: u456456, value: none}))
(define-constant LST (list))

(define-read-only (btc-to-stx (input (string-ascii 60)))
  (let (
    ;; (map b58-to-uint input) converts input string to list of optional uint
    ;; If input character is part of BASE58 alphabet we'll get `(some X)` where `X` is position in the alphabet.
    ;; Otherwise we'll get `none`
    ;; Then we filter out all `none` values
    ;; Once we do that, we can safely unwrap-panic all `(some X)` values to get final list of uint
    (b58-numbers (map unwrap-uint (filter is-some-uint (map b58-to-uint input))))
    ;; If length of `b58-number` is smaller than length of our input, it means input contains characters not present in BASE58 alphabet.
    ;; (len b58-numbers) can't be greater than (len input), thus ">=" is used as cheaper version of "is-eq"
    (t1 (asserts! (>= (len b58-numbers) (len input)) ERR_INVALID_CHAR))
    ;; To count leading ones we can use property of `index-of?` which returns the **first** index at which element can be found.
    ;; At this stage we operate on input converted to BASE58 alphabet indexes, we're looking for 0 instead of 1.
    ;; Character at index 0 == 1.
    ;; First we convert list of uint to list of booleans, where `true` represents one and `false` anything else.
    ;; And then with `index-of?` we can find index with `false` value, which tells us how many leading `true` (or ones) are in that list.
    ;; If `index-of?` won't find any `false` value, it means that there are just `true`, hence we default to `(len input)`.
    (leading-ones-count (default-to (len input) (index-of? (map is-zero b58-numbers) false)))

    ;; we convert leading ones to leading zeros
    (leading-zeros (map force-zero (unwrap-panic (slice? b58-numbers u0 leading-ones-count))))
    ;; we "cut" everything but leading ones
    (to-decode (default-to LST (slice? b58-numbers leading-ones-count (len b58-numbers))))

    ;; covert base 58 values to base 256 ones
    (decoded (concat (fold decode-outer to-decode LST) leading-zeros))
    ;; convert list of uint to buff concatenated in reverse order
    (decoded-hex (fold to-hex-rev decoded 0x))
    (decoded-hex-len (len decoded-hex))

    ;; anything shorter than 5 characters is pointless to work any further
    (t2 (asserts! (< u4 decoded-hex-len) ERR_TO_SHORT))
    ;; extract everything but last 4 bytes, pass it through double sha265 and extract first 4 bytes
    (actual-checksum (unwrap-panic (slice? (sha256 (sha256 (unwrap-panic (slice? decoded-hex u0 (- decoded-hex-len u4))))) u0 u4)))
    ;; extract last 4 bytes
    (expected-checksum (unwrap-panic (slice? decoded-hex (- decoded-hex-len u4) decoded-hex-len)))
    ;; compare expected checksum with actual, calculated one
    (t3 (asserts! (is-eq actual-checksum expected-checksum) ERR_BAD_CHECKSUM))

    ;; extract first byte and try to match it with BTC_VER and then STX_VER
    (version (unwrap-panic (element-at? STX_VER (unwrap! (index-of? BTC_VER (unwrap-panic (element-at? decoded-hex u0))) ERR_INVALID_VERSION))))
    )
    ;; construct principal
    (principal-construct? version (unwrap-panic (as-max-len? (unwrap-panic (slice? decoded-hex u1 (- decoded-hex-len u4))) u20)))
  )
)

(define-read-only (b58-to-uint (x (string-ascii 1))) (index-of? BASE58_CHARS x))
(define-read-only (is-some-uint (x (optional uint))) (is-some x))
(define-read-only (unwrap-uint (x (optional uint))) (unwrap-panic x))
(define-read-only (is-zero (x uint)) (is-eq x u0))
(define-read-only (force-zero (x uint)) u0)

(define-read-only (to-hex-rev (x uint) (out (buff 33)))
  (unwrap-panic (as-max-len? (concat (unwrap-panic (element-at? ALL_HEX x)) out) u33))
)

(define-read-only (decode-outer (x uint) (out (list 33 uint)))
  (let (
    (new-out (fold update-out out (list x)))
    (carry-to-push (fold carry-push 0x0000 (list (unwrap-panic (element-at? new-out u0)))))
    )
    (concat
      (default-to LST (slice? new-out u1 (len new-out)))
      (default-to LST (slice? carry-to-push u1 (len carry-to-push)))
    )
  )
)

(define-read-only (update-out (x uint) (out (list 30 uint)))
  ;; carry += x * 58;
  (let ((carry (+ (unwrap-panic (element-at? out u0))  (* x u58))))
    (unwrap-panic (as-max-len? (concat
      (list (/ carry u256)) ;; new carry
      (concat
        (default-to LST (slice? out u1 (len out))) ;; existing list
        (list (mod carry u256)) ;; new element
      )
    ) u30))
  )
)


(define-read-only (carry-push (x (buff 1)) (out (list 3 uint)))
  (let ((carry (unwrap-panic (element-at? out u0))))
    (if (> carry u0)
      (unwrap-panic (as-max-len? (concat
        (list (/ carry u256)) ;; new carry
        (concat
          (default-to LST (slice? out u1 (len out))) ;; existing list
          (list (mod carry u256)) ;; new element
        )
      ) u3))
      out
    )
  )
)