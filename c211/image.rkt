#lang racket

(require
 images/flomap
 racket/flonum
 racket/gui/base
 "color.rkt"
 "matrix.rkt")

(provide
 (all-from-out "color.rkt")
 (contract-out
  [draw-image   (-> image? void)]
  [image-cols   (-> image? exact-nonnegative-integer?)]
  [image-rows   (-> image? exact-nonnegative-integer?)]
  [image-equal? (-> image? image? boolean?)]
  [image-map    (-> (-> color? color?) image? image?)]
  [image?       (-> any/c boolean?)]
  [read-image   (->* () (path-string?) image?)]
  [write-image  (->* (image?) (path-string?) image?)]
  
  [image-ref  
   (case->
    (-> image? exact-nonnegative-integer? exact-nonnegative-integer? color?)
    (-> image? exact-nonnegative-integer? exact-nonnegative-integer? band? byte?))]
  
  [image-set!
   (case->
    (-> image? exact-nonnegative-integer? exact-nonnegative-integer? color? void)
    (-> image? exact-nonnegative-integer? exact-nonnegative-integer? band? byte? void))]
  
  [make-image
   (case->
    (-> exact-nonnegative-integer? exact-nonnegative-integer? image?)
    (-> exact-nonnegative-integer? exact-nonnegative-integer? color? image?)
    (-> exact-nonnegative-integer? exact-nonnegative-integer? 
        (-> exact-nonnegative-integer? exact-nonnegative-integer? color?)
        image?))]
  ))

(define-struct image (data)
  #:constructor-name new-image
  #:methods gen:custom-write
  [(define (write-proc image port mode)
     (fprintf port
              "#image<~a ~a>" 
              (image-rows image)
              (image-cols image)))])

(define (flvector->color v)
  (color (exact-floor (* (flvector-ref v 0) 255))
         (exact-floor (* (flvector-ref v 1) 255))
         (exact-floor (* (flvector-ref v 2) 255))))

(define (color->flvector c)
  (define (fix n) (if (exact? n) (exact->inexact n) n))
  (flvector (fix (/ (color-ref c 0) 255.0))
            (fix (/ (color-ref c 1) 255.0))
            (fix (/ (color-ref c 2) 255.0))))

(define (draw-image image) 
  (flomap->bitmap 
   (build-flomap*
    3 (image-cols image) (image-rows image)
    (λ (c r)
      (color->flvector (image-ref image r c))))))

(define (image-cols image) 
  (matrix-cols (image-data image)))

(define (image-rows image) 
  (matrix-rows (image-data image)))

(define (image-equal? image1 image2)
  (equal? image1 image2))

(define (image-map f image)
  (new-image
   (matrix-generator
    (image-rows image)
    (image-cols image)
    (λ (r c)
      (f (image-ref image r c))))))
   
(define file-formats
  '(("Any image format" "*.png;*.jpg;*.jpeg;*.bmp;*.gif")
    ("Portable network graphics" "*.png")
    ("JPEG" "*.jpg;*.jpeg")
    ("Bitmap" "*.bmp")
    ("Graphics interchange format" "*.gif")
    ("Any" "*.*")))

(define read-image
  (case-lambda
    [()
     (cond
       [(get-file "read-image" #f #f #f #f null file-formats)
        => (λ (filename) (read-image filename))])]
    [(filename)
     (when filename
       (define fm (bitmap->flomap (read-bitmap filename)))
       (define-values (cols rows) (flomap-size fm))
       (new-image
        (matrix-generator
         rows cols
         (λ (r c)
           (flvector->color
            (flomap-ref* fm c r))))))]))

(define write-image
  (case-lambda
    [(image)
     (cond
       [(get-file "write-image" #f #f #f #f null file-formats)
        => (λ (filename) (write-image image filename))])]
    [(image filename)
     (define bmp (draw-image image))
     (define kind 
       (case (string->symbol (string-downcase (last (string-split (path->string filename) "."))))
         [(png)      'png]
         [(jpg jpeg) 'jpeg]
         [(xbm)      'xbm]
         [(xpm)      'xpm]
         [(bmp)      'bmp]
         [else
          (error 'write-image "Cannot write file format, support formats: png, jpeg, xbm, xpm, bmp")]))
     (send bmp save-file filename kind)]))

(define (check-bounds function-name image row col)
  (unless (and (<= 0 row (- (image-rows image) 1))
               (<= 0 col (- (image-cols image) 1)))
    (error function-name "index (~a ~a) out of bounds for ~a" row col image)))

(define image-ref
  (case-lambda
    [(image row col)
     (matrix-ref (image-data image) row col)]
    [(image row col band)
     (color-ref (matrix-ref (image-data image) row col) band)]))

(define image-set!
  (case-lambda
    [(image row col color)
     (matrix-set! (image-data image) row col color)]
    [(image row col band value)
     (color-set! (matrix-ref (image-data image) row col) band value)]))

(define make-image
  (case-lambda
    [(rows cols) 
     (make-image rows cols black)]
    [(rows cols color/f)
     (new-image 
      (matrix-generator
       rows cols
       (if (color? color/f)
           (λ (r c) 
             (color (color-ref color/f 0)
                    (color-ref color/f 1)
                    (color-ref color/f 2)))
           color/f)))]))