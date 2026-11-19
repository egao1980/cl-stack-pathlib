(in-package #:cl-stack-pathlib/tests)

;;; OCTETS-HASH = SHA-256, "sha256:" + lowercase hex. Vectors: FIPS 180-2
;;; examples (empty, "abc", 448-bit, one million "a") plus padding boundaries
;;; (55/56/63/64/65/119 octets of "a"), checked with `shasum -a 256`.

(defun a-octets (n)
  (make-array n :element-type '(unsigned-byte 8) :initial-element (char-code #\a)))

(deftest-parametrize octets-hash-vectors
    ((input expected)
     :ids ("empty" "abc" "448-bit" "a55" "a56" "a63" "a64" "a65" "a119" "million-a" "utf8")
     ((octets) "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
     ((octets "abc") "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
     ((octets "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
     ((a-octets 55) "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318")
     ((a-octets 56) "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a")
     ((a-octets 63) "7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34")
     ((a-octets 64) "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb")
     ((a-octets 65) "635361c48bb9eab14198e76ea8ab7f1a41685d6ad62aa9146d301d4f17eb0ae0")
     ((a-octets 119) "31eba51c313a5c08226adf18d4a359cfdfd8d2e816b13f4af952f7ea6584dcfb")
     ((a-octets 1000000) "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")
     ((octets (format nil "h~Cllo ~C~%" (code-char #xE9) (code-char #x2713)))
      "9be5bd4e3f83c6050bca22ac38dd5e40df7bb23e8821e58533e298b6e2f4bbf1"))
  (ok (string= (octets-hash input) (concatenate 'string "sha256:" expected))))

(deftest octets-hash-subrange-and-types
  (ok (string= (octets-hash (octets "xxabcyy") :start 2 :end 5)
               (octets-hash (octets "abc"))))
  (ok (string= (octets-hash (octets "xxabc") :start 2)
               (octets-hash (octets "abc"))))
  ;; Any vector of octets, not only simple (unsigned-byte 8) arrays.
  (ok (string= (octets-hash (vector 97 98 99)) (octets-hash (octets "abc"))))
  (ok (string= (octets-hash (make-array 3 :element-type '(unsigned-byte 8)
                                          :initial-contents '(97 98 99)
                                          :adjustable t :fill-pointer 3))
               (octets-hash (octets "abc")))))

(deftest-parametrize content-hash-backends
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (let* ((big (let ((v (make-array 300001 :element-type '(unsigned-byte 8))))
                  (dotimes (i (length v) v) (setf (aref v i) (mod (* i 7) 251)))))
           (small (octets "hello")))
      (make-tree root (list (list "small.txt" small) (list "big.bin" big)
                            (list "empty" (octets)) "dir/"))
      (ok (string= (content-hash (join root "small.txt")) (octets-hash small)))
      ;; More than one read chunk: streaming must equal the one-shot digest.
      (ok (string= (content-hash (join root "big.bin")) (octets-hash big)))
      (ok (string= (content-hash (join root "empty"))
                   "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
      (ok (string= (fs-content-hash (path-filesystem root) (path-pathname (join root "small.txt")))
                   (octets-hash small)))
      (ok (signals (content-hash (join root "missing")) 'path-not-found))
      (ok (signals (content-hash (join root "dir")) 'path-error)))))

(deftest content-hash-zip
  (let ((fs (make-zip-fs '(("a/b.txt" "hello")))))
    (with-filesystem (fs)
      (ok (string= (content-hash "/a/b.txt") (octets-hash (octets "hello"))))
      (ok (signals (content-hash "/a/missing") 'path-not-found)))))
