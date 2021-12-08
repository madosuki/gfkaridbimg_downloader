(ql:quickload :dexador)
(ql:quickload :plump)
(ql:quickload :clss)
(ql:quickload :babel)

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
      (format t "~A~%" html)
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

;; This function is get MainCards.
(defun get-main-cards (struct)
  (let ((path (connect-string-list (list (setting-data-rootdir struct) "/main/"))))
    (ensure-directories-exist path)
    (labels ((save-top-images (n)
               (dolist (l n)
                 (let* ((result (get-href l))
                        (title (get-title l))
                        (url (connect-string-list (list *baseurl* result)))
                        (fpath (connect-string-list (list path title ".jpg")))
                        (expect-file-path (make-pathname :name fpath)))
                   (when (null (probe-file expect-file-path))
                     (let ((img (dex:get url)))
                       (writeimg fpath img)))))))
      (let ((table-list (coerce (clss:select "a.cgc" (setting-data-body struct)) 'list)))
        (dolist (target table-list)
          (let* ((href (plump:attribute target "href"))
                 (html (get-html href)))
            (save-top-images (coerce (clss:select "a.cl" html) 'list))))))))

;; This function is get PetiCards.
(defun get-petit-cards (struct)
  (let ((path (connect-string-list (list (setting-data-rootdir struct) "/peti/"))))
    (ensure-directories-exist path)
    (let ((n (coerce (clss:select "a" (clss:select "div.petitgirl-img" (setting-data-body struct))) 'list))
          (x 0))
      (dolist (l n)
        (let* ((result (get-href l))
               (url (connect-string-list (list *baseurl* result)))
               (fpath (connect-string-list (list path (int-to-string-10 x) ".png"))))
          (if (null (probe-file (make-pathname :name fpath)))
              (let ((img (dex:get url)))
                (writeimg fpath img)))
          (incf x))))))

;; This function is get Hitokoma.
(defun get-hitokoma (struct)
  (let ((path (connect-string-list (list (setting-data-rootdir struct) "/hitokoma/"))))
    (ensure-directories-exist path)
    (let ((n (coerce (clss:select "a" (clss:select "div" (setting-data-body struct))) 'list)))
      (dolist (l n)
        (let ((check (plump:attribute l "data-lightbox")))
          (cond ((equal check "hitokoma")
                 (let* ((result (get-href l))
                        (title (get-title l))
                        (fpath (connect-string-list (list path title ".png"))))
                   (if (null (probe-file (make-pathname :name fpath)))
                       (let* ((url (connect-string-list (list *baseurl* result)))
                              (img (dex:get url)))
                         (writeimg fpath img)))))))))))

;; This function is get ♪Cards.
(defun get-onpu-cards (struct)
  (let ((path (connect-string-list (list (setting-data-rootdir struct) "/onpu/")))
        (count 0)
        (onpu-list (list "")))
    (ensure-directories-exist path)
    (let ((n (coerce (clss:select "a" (clss:select "div" (setting-data-body struct))) 'list)))
      (dolist (l n)
        (let ((check (plump:attribute l "data-lightbox")))
          (cond ((equal check "gfmusic-card-set")
                 (let ((result (get-href l))
                       (title (get-title l)))
                   ;; First
                   (cond ((= count 0)
                          (push title (cdr (last onpu-list)))
                          (let ((fpath (connect-string-list (list path title ".jpg"))))
                            (when (null (probe-file (make-pathname :name fpath)))
                                (let* ((url (connect-string-list (list *baseurl* result)))
                                       (img (dex:get url)))
                                  (writeimg (connect-string-list (list path title ".jpg")) img))))
                          (setq count 2))
                         ;; Second
                         ((/= count 0)
                          ;; Check duplicate.
                          (let ((dupcount 0))
                            (dolist (dupstr onpu-list)
                              (when (equal dupstr title)
                                  (incf dupcount)))
                            ;; Get Second Card.
                            (cond ((and (= dupcount 1) (= count 2))
                                   (push title (cdr (last onpu-list)))
                                   (let ((fpath (connect-string-list (list path title  "2.jpg"))))
                                     (when (null (probe-file (make-pathname :name fpath)))
                                         (let* ((url (connect-string-list (list *baseurl* result)))
                                                (img (dex:get url)))
                                           (writeimg fpath img))))
                                   (incf count))
                                  ;; Get Third Card
                                  ((and (= dupcount 0) (= count 3)) 
                                   (push title (cdr (last onpu-list)))
                                   (let ((fpath (connect-string-list (list path title ".jpg"))))
                                     (when (null (probe-file (make-pathname :name fpath)))
                                         (let* ((url (connect-string-list (list *baseurl* result)))
                                                (img (dex:get url)))
                                           (writeimg fpath img))))
                                   (setq count 1))
                                  ;; Reset
                                  ((= dupcount 0) 
                                   (push title (cdr (last onpu-list)))
                                   (let ((fpath (connect-string-list (list path title ".jpg"))))
                                     (when (null (probe-file (make-pathname :name fpath)))
                                         (let* ((url (connect-string-list (list *baseurl* result)))
                                                (img (dex:get url)))
                                           (writeimg fpath img)))
                                     (incf count))
                                   )))))))))))))
