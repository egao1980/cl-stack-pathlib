(in-package #:cl-stack-pathlib)

;;; Content hashes (H2a). Pure-Lisp SHA-256 (FIPS 180-4) so pathlib keeps no
;;; crypto dependency. Digest strings use the OCI form "sha256:<64 hex>",
;;; lowercase, so they can serve as content-addressed blob ids (R4a) and as
;;; EXPECTED-HASH tokens for compare-and-swap writes.
;;;
;;; The implementation needs an incremental core (init / update over an octet
;;; range / final) so FS-CONTENT-HASH on the local backend can stream a file
;;; in chunks instead of reading it whole.

(defun octets-hash (octets &key (start 0) end)
  "SHA-256 of OCTETS[START, END) as \"sha256:\" + 64 lowercase hex digits."
  (declare (ignore octets start end))
  (%h2a-pending 'octets-hash))
