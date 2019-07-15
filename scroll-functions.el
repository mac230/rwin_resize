
;; functions for scrolling help functions in other windows

(defun mac-pdf-man-R-or-help-scroll (val)
  "Function to move to the previous page of a pdf or scroll an R or man page buffer when working in a separate, non-pdf buffer."
  (interactive)

;; set up variables
  (let* ((current-buffer (current-buffer))
         (current-point (point))
         (window-list (window-list))
         (pdf)
         (man)
         (R-buffer)
         (help-page)
         (current-buffer (current-buffer))
         (scroll-error-top-bottom t)
         (scroll-fun
          (lambda (doc-buf) (progn
                              (switch-to-buffer-other-window doc-buf)
                              (when (bobp) (scroll-up-command 1))
                              (when (eobp) (previous-line 1))
                              (scroll-up-command val)
                              (switch-to-buffer-other-window current-buffer)
                              (goto-char current-point))))
         (pdf-scroll-fun
          (lambda ()
            (switch-to-buffer-other-window pdf)
            (setq val (/ val (abs val)))
            (unless (or
                     (< (+ val (pdf-view-current-page)) 1)
                     (> (+ val (pdf-view-current-page)) (pdf-cache-number-of-pages)))
                     (pdf-view-next-page-command val))
            (switch-to-buffer-other-window current-buffer)
            (goto-char current-point)))
         )

  ;; use dolist to find which of the different buffer types you have
  (dolist (buffer window-list)
    (when (eq
           (buffer-local-value 'major-mode (window-buffer buffer))
           'Man-mode)
      (setq man (window-buffer buffer)))

    (when (eq
           (buffer-local-value 'major-mode (window-buffer buffer))
           'pdf-view-mode)
      (setq pdf (window-buffer buffer)))

    (when (eq
           (buffer-local-value 'major-mode (window-buffer buffer))
           'ess-help-mode)
      (setq R-buffer (window-buffer buffer)))

    (when (eq
           (buffer-local-value 'major-mode (window-buffer buffer))
           'help-mode)
      (setq help-page (window-buffer buffer)))
    )

  ;; now use cond to decide how to proceed
  (cond

   ;; have just a pdf (most likely use scenario)
   ((and pdf
         (not man)
         (not R-buffer)
         (not help-page))
    (funcall pdf-scroll-fun))

   ;; have just a man page
   ((and man
         (not pdf)
         (not R-buffer)
         (not help-page))
    (funcall scroll-fun man))

   ;; have just R help window
   ((and R-buffer
         (not pdf)
         (not man)
         (not help-page))
    (funcall scroll-fun R-buffer))

   ;; have just help window
   ((and help-page
         (not pdf)
         (not man)
         (not R-buffer))
    (funcall scroll-fun help-page))

   ;; have some other configuration
   ((or pdf man R-buffer help-page)
    (let* ((key (key-description
                 (read-key-sequence "scroll window:pdf=SPC, r=R, m=man, h=help")))
           (stop))
      (while (not (string= stop "g"))
        (cond
         ((string= key "SPC")
          (funcall pdf-scroll-fun))

         ((string= key "r")
          (funcall scroll-fun R-buffer))

         ((string= key "m")
          (funcall scroll-fun man))

         ((string= key "h")
          (funcall scroll-fun help-page)))

        (setq stop (key-description (read-key-sequence "SPC=pdf, r=R, m=man, h=help, g=quit: ")))
        )))

   ;; contingency function
   (t
    (if (eq pdf (window-buffer (car (window-at-side-list nil 'right))))
        (funcall pdf-scroll-fun)
      (funcall scroll-fun
               (window-buffer (car (window-at-side-list nil 'right))))))
   ))
  )


(defun mac-pdf-man-help-or-R-next-page ()
  "Function to move to the next page of a pdf or scroll a help buffer down (R, python, man, etc...) when working in a separate, non-pdf buffer."
  (interactive)
  (if (not
       (eq
        (window-buffer (car (window-at-side-list nil 'left)))
        (window-buffer (car (window-at-side-list nil 'right)))))
      (mac-pdf-man-R-or-help-scroll 10)
    (funcall (lambda nil (interactive) (scroll-up-command (/ (window-height) 2)) (forward-line (/ (window-height) 2))))))

(defun mac-pdf-man-help-or-R-prev-page ()
  "Function to move to the previous page of a pdf or scroll a help buffer up (R, python, man, etc...) when working in a separate, non-pdf buffer."
  (interactive)
  (if (not
       (eq
        (window-buffer (car (window-at-side-list nil 'left)))
        (window-buffer (car (window-at-side-list nil 'right)))))
  (mac-pdf-man-R-or-help-scroll -10)
  (funcall (lambda nil (interactive) (scroll-down-command (/ (window-height) 2)) (forward-line (* -1 (/ (window-height) 2)))))))
