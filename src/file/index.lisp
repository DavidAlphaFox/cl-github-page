(in-package #:com.liutos.cl-github-page.file)

(defun get-basename (pathname)
  ;; 判断是否是pathname类型
  (when (pathnamep pathname)
    ;; 如果是pathname类型，先转成字符串类型
    (setf pathname (namestring pathname)))
  ;; 反向找到最后一个dot符号和最后一个路径符号
  (let ((dot-position (position #\. pathname :from-end t))
        (slash-position (position #\/ pathname :from-end t)))
    ;; 得到路径符号和dot符号之间字串，就是文件名字
    (subseq pathname (1+ slash-position) dot-position)))

(defun get-file-content (filespec)
  ;; 使用unsignd-byte 8的方式打开文件流
  (with-open-file (stream filespec
                          :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream)) ;; 获取文件长度
           (content (make-array length 
                                :element-type '(unsigned-byte 8)))) ;; 创建一个和文件长度长度的array
      (read-sequence content stream) ;; 将文件读入数组中
      (flexi-streams:octets-to-string content
                                      :external-format :utf-8)))) ;; 将该数组转化成utf8字符串
;; 判断文件是否存在
(defun is-file-exists (pathspec)
  (and (file-exists-p pathspec) ;; 文件存在
       (not (directory-exists-p pathspec)))) ;; 并且文件不是目录
;; 将内容写入文件
(defun set-file-content (content filespec)
  (with-open-file (stream filespec
                          :direction :output
                          :if-exists :supersede)
    (write-string content stream)))
