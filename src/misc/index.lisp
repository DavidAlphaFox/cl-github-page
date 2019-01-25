(in-package #:com.liutos.cl-github-page.misc)
;; 使用local-time包获取当前时间并进行格式化
(defun make-datetime-of-now ()
  (let ((timestamp (local-time:now))
        (format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec)))
    (local-time:format-timestring nil timestamp :format format)))
