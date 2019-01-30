(in-package #:com.liutos.cl-github-page.post)
;; 得到文章的作者
(defun get-post-author (source)
  "文章的作者是取的系统用户的名称"
  (declare (optimize (speed 0)))
  (if (pathnamep source)
      (progn
        #+sbcl
        (let* ((stat (sb-posix:stat source))
               (uid (sb-posix:stat-uid stat)))
          (cdr (assoc :name (osicat:user-info uid))))
        #-sbcl
        (osicat:environment-variable "USER"))
      (osicat:environment-variable "USER")))

(defun universal-to-string (universal)
  (let ((timestamp (local-time:universal-to-timestamp universal)))
    (local-time:format-timestring
     nil timestamp
     :format '(:year "年" :month "月" :day "日"))))

;;; EXPORT
;; 添加新的文章
(defun add-post (source
                 &key
                   author
                   category-id
                   post-id
                   title
                   write-at)
  "添加新的文章，source是文章的mardown全文路径"
  (check-type category-id (or fixnum null))
  (unless author
    (setf author (get-post-author source)))
  (when (and (pathnamep source) (null title))
    (setf title (com.liutos.cl-github-page.file:get-basename source))) ;;文章的title就是文件名
  (when (pathnamep source)
    (if (com.liutos.cl-github-page.file:is-file-exists source)
        (setf source (com.liutos.cl-github-page.file:get-file-content source))
        (error "~A: 文件不存在" source)))
  (when (com.liutos.cl-github-page.storage:find-post-by-source source);; 在database中依据路径查找是否有相同文章
    (error "存在一模一样的文章"))
  (let (body)
    (setf body
          (com.liutos.cl-github-page.compile:compile-from-markdown source))
    (com.liutos.cl-github-page.storage:create-post body
                                                   1
                                                   source
                                                   title
                                                   :author author
                                                   :post-id post-id
                                                   :write-at write-at) ;; 将文章保存到数据库中
    (let ((post-id (com.liutos.cl-github-page.storage:find-max-post-id)))
      (let ((default-category (com.liutos.cl-github-page.config:get-default-category)))
        (when (and category-id default-category)
          (setf category-id
                (getf (com.liutos.cl-github-page.category:find-by-name default-category)
                      :category_id))))
      (when category-id
        (com.liutos.cl-github-page.bind:bind-category-post category-id post-id))
      post-id))) ;;增加分类的ID

(defun delete-post (post-id)
  (unless (com.liutos.cl-github-page.storage:find-post post-id)
    (error "~D: 文章不存在" post-id))
  (com.liutos.cl-github-page.storage:delete-post post-id))

(defun make-post-write-at (post)
  (let* ((write-at (getf post :write_at))
         (timestamp (local-time:universal-to-timestamp write-at)))
    (local-time:format-timestring
     nil timestamp
     :format '(:year "-" (:month 2) "-" (:day 2))
     :timezone local-time:+utc-zone+)))

(defun modify-post (post-id
                    &key
                      source
                      title
                      write-at)
  (when (pathnamep source)
    (if (com.liutos.cl-github-page.file:is-file-exists source)
        (setf source (com.liutos.cl-github-page.file:get-file-content source))
        (error "~A: 文件不存在" source)))
  (when (com.liutos.cl-github-page.storage:find-post-by-source source)
    (error "存在一模一样的文章"))
  (unless (com.liutos.cl-github-page.storage:find-post post-id)
    (error "~D: 文章不存在" post-id))
  (let (body)
    (setf body
          (com.liutos.cl-github-page.compile:compile-from-markdown source))
    (com.liutos.cl-github-page.storage:update-post post-id
                                                   :body body
                                                   :source source
                                                   :title title
                                                   :write-at write-at)))

(defun unshelve-post (post-id)
  (unless (com.liutos.cl-github-page.storage:find-post post-id)
    (error "~D: 文章不存在" post-id))
  (com.liutos.cl-github-page.storage:update-post post-id
                                                 :is-active 0))
