(in-package #:com.liutos.cl-github-page.template)

(defun fill-post-template (blog-description
                           blog-title
                           categories
                           post-body
                           post-meta
                           post-title)
  (let ((template #p"/media/world2/liutos/src/cl/cl-github-page/src/template/tmpl/post.html")
        (values (list :blog-description blog-description
                      :blog-title blog-title
                      :categories categories
                      :post-body post-body
                      :post-meta post-meta
                      :post-title post-title)))
    (with-output-to-string (string)
      (html-template:fill-and-print-template template values
                                             :stream string))))
