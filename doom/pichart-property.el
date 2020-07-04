;;; ~/.doom.d/pichart-property.el -*- lexical-binding: t; -*-
(defun pichart/make-test-org-file ()
  (find-file (make-temp-file nil nil ".org"
                             " \"#+BEGIN: my/pichart-property :scope file :property-colors (( \"foo\" . \"red\" ) (\"wow\" . \"blue\"))
| foo | 0:42 | red |
| wow | 0:21 | blue |
[[file:/tmp/chart.png]]
#+END:

* test
:PROPERTIES:
:CLOCK-TYPE: foo
:END:
:LOGBOOK:
CLOCK: [2020-07-04 Sat 20:54]--[2020-07-04 Sat 21:15] =>  0:21
:END:
** bar
:PROPERTIES:
:CLOCK-TYPE: foo
:END:
:LOGBOOK:
CLOCK: [2020-07-04 Sat 20:54]--[2020-07-04 Sat 21:15] =>  0:21
:END:

** baz
:PROPERTIES:
:CLOCK-TYPE: wow
:END:
:LOGBOOK:
CLOCK: [2020-07-04 Sat 20:54]--[2020-07-04 Sat 21:15] =>  0:21
:END:
")))

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
    (dolist (entry (reverse property-list))
      (insert-before-markers
       "| "
       (plist-get entry :title)
       (format " | %s"
               (org-minutes-to-clocksum-string (plist-get entry :value)))
       " | "
       (plist-get entry :color)
       " | "
       "\n"))
    (insert "[[file:/tmp/chart.png]]")
    (my/make-pichart  property-list "/tmp/chart.png")))

(defun my/clock-sum-only-this-heading ()
  (save-restriction
    (narrow-to-region (line-beginning-position)
                      (or (outline-next-heading)
                          (point-max)))
    (org-clock-sum-today)))

(defun my/make-pichart (plots file)
  (with-current-buffer (find-file-noselect "/tmp/pichart.plt")
    (delete-region (point-min) (point-max))
    (insert
     "\
set term png
set output \"" file "\"
set xrange [-1:1]
set yrange [-1:1]
set style fill solid 1

unset border
unset tics
set key outside bottom
")

    (let* ((i 0)
          (prev-angle 0)
          angle
          (value-list (-map (lambda (x) (plist-get x :value)) plots))
          (sum-values (-sum value-list))
          (percent-list (-map (lambda (x) (/ (float x) sum-values))
                              value-list)))
      (loop for p in plots
            for percent in percent-list
            do
        (setq angle (+ prev-angle (* 360 percent)))
        (insert (format "$data%d << EOD
0 0 1 %d %d
e
EOD
"
                        i
                        prev-angle
                        angle))
        (setq i (+ 1 i))
        (setq prev-angle angle))

      (setq i 0)
      (insert "plot ")
      (dolist (p plots)
        (insert (format "$data%d" i) " with circles linecolor rgb \"" (or (plist-get p :color) "black") "\" title \"" (plist-get p :title) "\" ,\\\n")
        (setq i (+ 1 i)))
      (delete-char -2)
      (save-buffer)
      (call-process "gnuplot" nil nil nil "/tmp/pichart.plt"))))
