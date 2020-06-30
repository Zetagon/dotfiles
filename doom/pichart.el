;;; test.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo <http://github/leo>
;; Maintainer: Leo <leo@leo-B85-HD3>
;; Created: June 15, 2020
;; Modified: June 15, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/leo/test
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:


(defun org-dblock-write:my/pichart (params)
  ""
  (let ((tag-table (make-hash-table
                    :test 'equal))
        last-active-tags
        current-tags
        tag-list
        (my/clockreport-tags-colors (or (plist-get params :tags)
                                        my/clockreport-tags-colors)))
    (dolist (entry (my/clockreport-list-builder params))
      (setq current-tags (plist-get entry :tags))
      (when (string= current-tags "")
        (setq current-tags "Untagged"))
      (puthash
       current-tags
       (+ (or (gethash current-tags tag-table) 0)
          (plist-get entry :time))
       tag-table))
    (let ((i 0))
      (maphash (lambda (k v)
                 (push (list :title k
                             :value v
                             :color (or (alist-get k my/clockreport-tags-colors nil nil #'equal) "black"))
                       tag-list))
               tag-table)
      (setq i (+ i 1)))
    (insert-before-markers "| Tags | Time|\n")
    (dolist (entry (reverse tag-list))
      (insert-before-markers
       "| "
       (plist-get entry :title)
       (format " | %s"
               (org-minutes-to-clocksum-string (plist-get entry :value)))
       " |\n"))
    (org-table-align)
      (my/make-pichart tag-list "/tmp/chart.png")
      (insert "[[file:/tmp/chart.png][file:chart.png]]")))

(defun my/clockreport-list-builder (params)
  (org-map-entries (lambda ()
                     (list
                      :tags (mapconcat #'identity
                                       (sort (-filter (lambda (x)
                                                        (-contains? (-map #'car my/clockreport-tags-colors)
                                                                    x)) org-scanner-tags)
                                              (lambda (x y)
                                                (when (eq 1 (compare-strings x nil nil y nil nil))
                                                  t)))
                                       ", ")
                      :heading (org-get-heading t )
                      :time (my/clock-sum-only-this-heading)
                      :level (org-current-level)))
                   nil
                   (plist-get params :scope)))

(defun my/clock-sum-only-this-heading ()
  (save-restriction
    (narrow-to-region (line-beginning-position)
                      (or (outline-next-heading)
                          (point-max)))
    (org-clock-sum-today)))

(defun my/filter-inherited-tags (tags)
  (-filter (lambda (x)
             (not
              (get-text-property 0 'inherited x)))
           tags))

(defvar my/clockreport-tags-colors
  nil)
(setq my/clockreport-tags-colors
      '(("ospp" . "red")
        ("sannstat" . "blue")
        ("bervet" . "orange")
        ("ioopm" . "yellow")
        ("automata" . "green")))
(defun my/clockreport-pichart (ipos table-list params)
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
                 (push (list :title k :value v :color (alist-get k my/clockreport-tags-colors nil nil #'equal)) tracked-time-plist))
               time-per-tag)
      (my/make-pichart tracked-time-plist "/tmp/chart.png")
      (insert "[[file:/tmp/chart.png][file:chart.png]]")
      (org-display-inline-images))))
(provide 'test)
;;; test.el ends here
