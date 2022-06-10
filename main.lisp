(ql:quickload '("dexador"
                "plump"
                "lquery"
                "cl-ppcre"
                "cl-date-time-parser"
                "local-time"
                "cl-csv"
                ))
(defparameter *four-plus-urls* (mapcar #'(lambda (tag) (format nil "https://www.angrymetalguy.com/tag/~A" tag)) (list 40 45 50)))
(defparameter *results-file* "results.csv")
(defparameter *all-diffs* "all-diffs.csv")

(defun get-next-url (page)
  (let ((href (lquery:$ page ".nav-links .next" (attr :href) )))
    (if (> (length href) 0) (elt href 0))))

(defun parse-review-date (review-meta) 
  (multiple-value-bind (start end) (ppcre:scan "\\son\\s.*\\sin\\s" review-meta)
    ; get rid of cruft surrounding date
    (let ((date-string (subseq review-meta (+ start 4) (- end 4))))
      (multiple-value-bind (universal-time) (cl-date-time-parser:parse-date-time date-string)
        universal-time))))

(defun parse-review-header (header)
  ; Garbage split is garbage, regex replace replaces nbsp with regular space.
  (let ((split (cl-ppcre:split " – |-" (cl-ppcre:regex-replace " " header " ") :limit 2)))
    (destructuring-bind (band album) split 
      (values band (cl-ppcre:regex-replace " Review" album "")))))

(defun format-universal-time (time-int)
  (local-time:format-timestring nil 
    (local-time:universal-to-timestamp time-int) 
    :format '(:year "-" :month "-" :day)))

(defun get-parsed-page (url)
  (lquery:$ (initialize (dex:get url))))

(defun get-reviews-info (page)
  (let ((reviews (lquery:$ page ".entry-header" (combine ".entry-title" ".entry-meta" "a"))))
    (loop for (header meta anchors) across reviews
          when (and (find "Reviews" 
                      (map 'vector (lambda (anch) (lquery-funcs:render-text anch)) anchors) 
                      :test #'string-equal)
                    (ppcre:scan "Review$" (elt (lquery-funcs:render-text header) 0)))
          collect (let ((header-text (elt header 0))
                         (meta (elt (lquery-funcs:render-text meta) 0)))
                    (multiple-value-bind (band album) (parse-review-header header-text)
                      (list band album (parse-review-date meta)) 
                      )))))

(defun recurse-get-reviews (page &key (reviews-info (list))) 
  (if page 
      (recurse-get-reviews 
        (let ((next-url (get-next-url page))) 
          (if next-url (get-parsed-page next-url))) 
        :reviews-info (concatenate 'list reviews-info (get-reviews-info page)))
      reviews-info))

(defun get-four-plus-reviews ()
  (loop for url in *four-plus-urls* 
        with result = (list)
        do (setf result (concatenate 'list result (recurse-get-reviews (get-parsed-page url))))
        finally (return result)) )

(defun more-recent-review(review-a review-b)
  (let ((a-date (car (last review-a)))
        (b-date (car (last review-b))))
    (< a-date b-date)))

(defun get-review-time (row)
  (parse-integer (car (last row))))

(defun get-days-between-reviews()
  (let ((sorted-list (cdr (cl-csv:read-csv #P"./results.csv"))))
    (sort (loop for (review . (next-review . rest)) on sorted-list
          when next-review
           collect (list review next-review (abs (- (get-review-time review) (get-review-time next-review))) )
           )
          'more-recent-review)))

(defun create-day-difference-csv ()
  (let ((sorted-data (get-days-between-reviews)))
    (with-open-file 
      (stream *all-diffs*
              :direction :output 
              :if-does-not-exist :create
              :if-exists :append)
      (loop for ((band-one album-one date-one) (band-two album-two date-two) seconds-different) in sorted-data
            do (cl-csv:write-csv-row 
                 (list band-one album-one date-one band-two album-two date-two
                   (/ seconds-different (* 60 60 24)))
                 :stream stream)))))

(defun create-four-plus-csv ()
  (let ((sorted-data (sort (get-four-plus-reviews) 'more-recent-review)))
    (with-open-file 
      (stream *results-file*
              :direction :output 
              :if-does-not-exist :create
              :if-exists :append)
      (cl-csv:write-csv-row (list "Band" "Album" "Date Published" "Nerd Date") :stream stream)
      (loop for (band album universal-date-time) in sorted-data
            do (cl-csv:write-csv-row 
                 (list 
                   band
                   album
                   (format-universal-time universal-date-time)
                   universal-date-time)
                 :stream stream)))))
