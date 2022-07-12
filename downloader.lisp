(ql:quickload :dexador)
(ql:quickload :plump)
(ql:quickload :clss)
(ql:quickload :babel)
(ql:quickload :cl-ppcre)

;; This function is put image file.
(defun writeimg (filename img)
  (with-open-file (stream (make-pathname :name filename)
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :append
                          :if-does-not-exist :create
                          )
    (format t "Now Writing ~A~%" filename)
    (dolist (target (coerce img 'list))
      (write-byte target stream))
    (format t "Done!~%~%")))

(defun connect-string-list (l)
  (reduce (lambda (x y) (format nil "~A~A" x y)) l))

;; Base URL
(defvar *baseurl* "https://gfkari.gamedbs.jp")

;; Detail Page URL
(defvar *detail* (connect-string-list (list *baseurl* "/girl/detail/")))

;; HTML Body
(defvar *body*)

;; Girls No
(defvar *numstr* "")

;; Output Dir
(defparameter *rootdir* "")

(defstruct setting-data
  (body)
  (numstr "")
  (rootdir ""))

;; Set dir and Create work dir.
(defun get-and-create-rootdir (path-string struct)
  (let ((path (connect-string-list
               (list path-string
                     (setting-data-numstr struct)))))
    (ensure-directories-exist (connect-string-list (list path "/")))
    path))

(defmacro int-to-string-10 (i)
  `(write-to-string ,i :base 10))

(defmacro get-num-str (x)
  `(int-to-string-10 ,x))

(defun get-html (s)
  (let ((binary (dex:get s :force-binary t)))
    (let* ((html (sb-ext:octets-to-string binary))
           (result (plump:parse html)))
      result)))

(defmacro msg ()
  `(format t "Please input number for wish girls~%"))

(defmacro get-href (l)
  `(plump:attribute ,l "href"))

(defmacro get-title (l)
  `(plump:attribute ,l "title"))

(defun get-body (numstr)
  (get-html
   (connect-string-list (list *detail* numstr))))

;; This funciton is get profile image and Scenario standing image.
(defun get-profile-and-scenario-images (struct)
  (let* ((path (connect-string-list (list (setting-data-rootdir struct) "/profile_and_scenario_images/")))
         (profile_fpath (connect-string-list (list path "profile_" (setting-data-numstr struct) ".png")))
         (scenario_fpath (connect-string-list (list path "scenario_" (setting-data-numstr struct) ".png")))
         (profile_url (connect-string-list (list *baseurl* "/images/profile/profile_" (setting-data-numstr struct) ".png")))
         (scenario_url (connect-string-list (list *baseurl* "/images/scenario/girl/270x570/" (setting-data-numstr struct) ".png"))))
    (when (null (probe-file (make-pathname :name profile_fpath)))
        (let ((profile_img (dex:get profile_url)))
          (ensure-directories-exist path)
          (writeimg profile_fpath profile_img)))
    (when (null (probe-file (make-pathname :name scenario_fpath)))
        (let ((scenario_img (dex:get scenario_url)))
          (writeimg scenario_fpath scenario_img)))))

(defun get-new-dl-url (&key body base-url target-class)
  (let ((table-list (coerce (clss:select target-class body)
                            'list))
        (result-list nil))
    (dolist (target table-list)
      (let ((href (plump:attribute target "href")))
        (when href
          (multiple-value-bind (a b)
              (ppcre:scan "[0-9]+$" href)
            (let* ((id-string (subseq href a b))
                   (id-integer (parse-integer id-string :junk-allowed t)))
              (when id-integer
                (push (get-html (format nil "~A~A" base-url id-integer))
                      result-list)))))))
    (reverse result-list)))

(defun save-images (&key target-list extension dir-path (check-duplicate nil) (max-count 3))
  (let ((count 1))
    (dolist (l target-list)
      (let* ((href (get-href l))
             (title (get-title l))
             (url (connect-string-list (list *baseurl* href)))
             (fpath (connect-string-list (list dir-path title extension)))
             (expect-file-path (make-pathname :name fpath)))
        (format t "~A~%" href)
        (when (and check-duplicate (probe-file expect-file-path))
          (setq fpath (connect-string-list
                       (list dir-path (format nil "~A~A" title count) extension)))
          (setq expect-file-path (make-pathname :name fpath)))
        (when (null (probe-file expect-file-path))
          (let ((img (dex:get url)))
            (writeimg fpath img)))
        (when check-duplicate
          (incf count)
          (when (> count max-count)
            (setq count 1)))))))

;; This function is get MainCards.
(defun get-main-cards (struct)
  (let ((path (connect-string-list (list (setting-data-rootdir struct) "/main/"))))
    (ensure-directories-exist path)
    (let ((body (get-new-dl-url :body (setting-data-body struct)
                                :base-url "https://gfkari.gamedbs.jp/card/group/"
                                :target-class "a.cgc")))
      (if body
          (save-images
           :target-list (coerce (clss:select "a.cl" body) 'list)
           :dir-path path
           :extension ".jpg")
          (format t "failed get-main-cards function~%")))))

;; This function is get PetiCards.
(defun get-petit-cards (struct)
  (let ((path (connect-string-list (list (setting-data-rootdir struct) "/peti/"))))
    (ensure-directories-exist path)
    (let ((target-list (coerce (clss:select "a" (clss:select "div.petitgirl-img" (setting-data-body struct))) 'list))
          (x 0))
      (dolist (l target-list)
        (let* ((href (get-href l))
               (url (connect-string-list (list *baseurl* href)))
               (fpath (connect-string-list (list path (int-to-string-10 x) ".png"))))
          (if (null (probe-file (make-pathname :name fpath)))
              (let ((img (dex:get url)))
                (writeimg fpath img)))
          (incf x))))))

;; This function is get Hitokoma.
(defun get-hitokoma (struct)
  (let ((path (connect-string-list (list (setting-data-rootdir struct) "/hitokoma/"))))
    (ensure-directories-exist path)
    (let ((target-list (coerce (clss:select "a" (clss:select "div" (setting-data-body struct))) 'list)))
      (dolist (l target-list)
        (let ((check (plump:attribute l "data-lightbox")))
          (cond ((equal check "hitokoma")
                 (let* ((href (get-href l))
                        (title (get-title l))
                        (fpath (connect-string-list (list path title ".png"))))
                   (if (null (probe-file (make-pathname :name fpath)))
                       (let* ((url (connect-string-list (list *baseurl* href)))
                              (img (dex:get url)))
                         (writeimg fpath img)))))))))))

;; This function is get ♪Cards.
(defun get-onpu-cards (struct)
  (let ((path (connect-string-list (list (setting-data-rootdir struct) "/onpu/"))))
    (ensure-directories-exist path)
    (let ((body (get-new-dl-url :body (setting-data-body struct)
                                :base-url "https://gfkari.gamedbs.jp/gfmusic/card/group/"
                                :target-class "a.cgc-m")))
      (if body
          (save-images
           :target-list (coerce (clss:select "a.cli" body) 'list)
           :dir-path path
           :check-duplicate t
           :extension ".jpg")
          (format t "failed get-onpu-cards function~%~%")))))
