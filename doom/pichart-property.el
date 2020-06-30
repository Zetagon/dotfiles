;;; ~/.doom.d/pichart-property.el -*- lexical-binding: t; -*-

(defun org-dblock-write:my/pichart-property (params)
  ""
  (let ((property-table (make-hash-table
                         :test 'equal))
        (org-trust-scanner-tags t)
        clock-type
        property-list
        (property-colors (plist-get params :property-colors)))
    (org-map-entries (lambda ()
                       (setq clock-type (org-entry-properties nil "CLOCK-TYPE"))
                       (unless clock-type
                         (setq clock-type "Uncategorized"))
                       (puthash
                        clock-type
                        (+ (or (gethash clock-type property-table)
                               0)
                           (my/clock-sum-only-this-heading))
                        property-table))
                     nil
                     (plist-get params :scope))
    (maphash (lambda (k v)
               (push (list :title k
                           :value v
                           :color (or (alist-get k property-colors nil nil #'equal) "black"))
                     property-list))
             property-table)
    (my/clockreport-pichart (point) property-colors params)))

(defun my/clock-sum-only-this-heading ()
  (save-restriction
    (narrow-to-region (line-beginning-position)
                      (or (outline-next-heading)
                          (point-max)))
    (org-clock-sum-today)))

(defun my/clockreport-pichart (ipos table-list params property-colors)
  (interactive)
  (goto-char ipos)
  (let ((time-per-tag (make-hash-table
                       :test 'equal))
        (tracked-tags (-map (lambda (x) (car x)) my/clockreport-tags-colors))
        used-tag-stack)
    (pcase-dolist (`(,file-name ,file-time ,entries) table-list)
      (pcase-dolist (`(,level ,headline ,tags-list ,ts ,time ,props) entries)
        (dolist (tag tags-list)
          (if (-contains? tracked-tags tag)
              (progn
                (puthash tag (+ (or time 0)
                                (or (gethash tag time-per-tag) 0))
                         time-per-tag)
                (push tag used-tag-stack))
            (push nil used-tag-stack)))))
    (let* ((i 1)
           tracked-time-plist)
      (maphash (lambda (k v)
                 (push (list :title k :value v :color (alist-get k property-colors  nil nil #'equal)) tracked-time-plist))
               time-per-tag)
      (my/make-pichart tracked-time-plist "/tmp/chart.png")
      (insert "[[file:/tmp/chart.png][file:chart.png]]")
      (org-display-inline-images))))
