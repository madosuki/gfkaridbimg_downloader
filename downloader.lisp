(ql:quickload :dexador)
(ql:quickload :plump)
(ql:quickload :clss)

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
      (write-byte target stream))))

(defun connectString(l)
  (let ((x ""))
    (dolist (o l)
      (setq x (concatenate 'string x o)))
    x))

;; Base URL
(defvar *baseurl* "http://gfkari.gamedbs.jp")

;; Detail Page URL
(defvar *detail* (connectString (list *baseurl* "/girl/detail/")))

;; HTML Body
(defvar *body*)

;; Girls No
(defvar *numstr* "")

;; Output Dir
(defparameter *rootdir* "")

;; Set dir and Create work dir.
(defun creterootdir()
  (setq *rootdir* (connectString (list "./girls/" *numstr*)))
  (ensure-directories-exist (connectString (list *rootdir* "/"))))

(defmacro IntToString10(i)
  `(write-to-string ,i :base 10))

(defmacro setNum(x)
  `(setq *numstr* (IntToString10 ,x)))

(defun getHtml(s)
  (let ((html (dex:get s)))
    (let ((result (plump:parse html)))
      result)))

(defmacro msg()
  `(format t "Please input number for wish girls~%"))

(defmacro getHref(l)
  `(plump:attribute ,l "href"))

(defmacro getTitle(l)
  `(plump:attribute ,l "title"))

(defun setBody()
  (setq *body*
        (getHtml
         (connectString (list *detail* *numstr*)))))

;; This funciton is get profile image and Scenario standing image.
(defun getProfileImgAndScenarioImg()
  (let ((path (connectString (list *rootdir* "/profile_and_scenario_images/"))))
    (ensure-directories-exist path)
  (let ((profile (dex:get (connectString (list *baseurl* "/images/profile/profile_" *numstr* ".png")))))
    (writeimg (connectString (list path "profile_" *numstr* ".png")) profile))
  (let ((scenario (dex:get (connectString (list *baseurl* "/images/scenario/girl/270x570/" *numstr* ".png")))))
    (writeimg (connectString (list path "/scenario_" *numstr* ".png")) scenario))))

;; This function is get MainCards.
(defun getMaincard()
  (let ((path (connectString (list *rootdir* "/main/"))))
    (ensure-directories-exist path)
   (let ((n (coerce (clss:select "a.cl" *body*) 'list)))
     (dolist (l n)
       (let ((result (getHref l)))
         (let ((title (getTitle l)))
           (let ((url (connectString (list *baseurl* result))))
             (let ((img (dex:get url)))
               (writeimg (connectString (list path title ".jpg")) img)))))))))

;; This function is get PetiCards.
(defun getPetitcard()
  (let ((path (connectString (list *rootdir* "/peti/"))))
    (ensure-directories-exist path)
  (let ((n (coerce (clss:select "a" (clss:select "div.petitgirl-img" *body*)) 'list)))
    (let ((x 0))
    (dolist (l n)
       (let ((result (getHref l)))
           (let ((url (connectString (list *baseurl* result))))
             (let ((img (dex:get url)))
               (writeimg (connectString (list path (IntToString10 x) ".png")) img)
               (incf x)))))))))

;; This function is get Hitokoma.
(defun getHitokoma()
  (let ((path (connectString (list *rootdir* "/hitokoma/"))))
    (ensure-directories-exist path)
  (let ((n (coerce (clss:select "a" (clss:select "div" *body*)) 'list)))
    (dolist (l n)
      (let ((check (plump:attribute l "data-lightbox")))
        (cond ((equal check "hitokoma")
               (let ((result (getHref l)))
                 (let ((title (getTitle l)))
                   (let ((url (connectString (list *baseurl* result))))
                     (let ((img (dex:get url)))
                       (writeimg (connectString (list path title ".png")) img))))))))))))

(defvar *count*)
(defparameter *onpulist* (list ""))
;; This function is get ♪Cards.
(defun getOnpuCard()
  (setq *count* 0)
  (let ((path (connectString (list *rootdir* "/onpu/"))))
    (ensure-directories-exist path)
  (let ((n (coerce (clss:select "a" (clss:select "div" *body*)) 'list)))
    (dolist (l n)
      (let ((check (plump:attribute l "data-lightbox")))
        (cond ((equal check "gfmusic-card-set")
               (let ((result (getHref l)))
                 (let ((title (getTitle l)))
                   ;; First
                   (cond ((= *count* 0)
                          (push title (cdr (last *onpulist*)))
                          (let ((url (connectString (list *baseurl* result))))
                            (let ((img (dex:get url)))
                              (writeimg (connectString (list path title ".jpg")) img)
                              (setq *count* 2))))
                         ;; Second
                         ((/= *count* 0)
                          ;; Check duplicate.
                          (let ((dupcount 0))
                            (dolist (dupstr *onpulist*)
                              (if (equal dupstr title)
                                  (incf dupcount)))
                            ;; Get Second Card.
                            (cond ((and (= dupcount 1) (= *count* 2))
                                   (push title (cdr (last *onpulist*)))
                                   (let ((url (connectString (list *baseurl* result))))
                                     (let ((img (dex:get url)))
                                       (writeimg (connectString (list path title "2.jpg")) img)
                                       (incf *count*))))
                                  ;; Get Third Card
                                  ((and (= dupcount 0) (= *count* 3)) 
                                   (push title (cdr (last *onpulist*)))
                                   (let ((url (connectString (list *baseurl* result))))
                                     (let ((img (dex:get url)))
                                       (writeimg (connectString (list path title ".jpg")) img)
                                       (setq *count* 1))))
                                  ;; Reset
                                  ((= dupcount 0) 
                                   (push title (cdr (last *onpulist*)))
                                   (let ((url (connectString (list *baseurl* result))))
                                     (let ((img (dex:get url)))
                                       (writeimg (connectString (list path title ".jpg"))img)
                                       (incf *count*)))))))))))))))))
