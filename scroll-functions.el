
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
         (my-xwidget)
         (my-doc)
         (current-buffer (current-buffer))
         (scroll-error-top-bottom t)
         (scroll-fun
          (lambda (doc-buf)
	    (progn
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
         (xwidget-scroll-fun
          (lambda (doc-buf)
	    (progn
	      (switch-to-buffer-other-window doc-buf)
	      (when (bobp) (xwidget-webkit-scroll-up-line 1))
	      (when (eobp) (xwidget-webkit-scroll-down-line -1))
	      (xwidget-webkit-scroll-up-line val)
	      (switch-to-buffer-other-window current-buffer)
	      (goto-char current-point))))
         (doc-scroll-fun
          (lambda (my-doc)
	    (switch-to-buffer-other-window my-doc)
	    (setq val (/ val (abs val)))
	    (unless (or
		     (< (+ val (doc-view-current-page)) 1)
		     (> (+ val (doc-view-current-page)) (doc-view-last-page-number)))
	      (doc-view-goto-page (+ (doc-view-current-page) val)))
	    (switch-to-buffer-other-window current-buffer)
            (goto-char current-point)))
         )

    ;; use dolist to find which of the different buffer types you have
    (dolist (buffer window-list)
      ;; man page
      (when (eq
	     (buffer-local-value 'major-mode (window-buffer buffer))
	     'Man-mode)
	(setq man (window-buffer buffer)))
      ;; pdf doc
      (when (eq
	     (buffer-local-value 'major-mode (window-buffer buffer))
	     'pdf-view-mode)
	(setq pdf (window-buffer buffer)))
      ;; R help page
      (when (eq
	     (buffer-local-value 'major-mode (window-buffer buffer))
	     'ess-help-mode)
	(setq R-buffer (window-buffer buffer)))
      ;; emacs help page
      (when (eq
	     (buffer-local-value 'major-mode (window-buffer buffer))
	     'help-mode)
	(setq help-page (window-buffer buffer)))
      ;; xwidget browser
      (when (or
             (eq
	     (buffer-local-value 'major-mode (window-buffer buffer))
	     'xwidget-webkit-mode)
             (eq
	     (buffer-local-value 'major-mode (window-buffer buffer))
	     'nov-xwidget-webkit-mode))
	(setq my-xwidget (window-buffer buffer)))
      ;; doc-view document
      (when (eq
	     (buffer-local-value 'major-mode (window-buffer buffer))
	     'doc-view-mode)
	(setq my-doc (window-buffer buffer)))
      )

    ;; now use cond to decide how to proceed
    (cond
     ;; have just a pdf (most likely use scenario)
     ((and pdf
           (not man)
           (not R-buffer)
           (not help-page)
           (not my-xwidget)
           (not my-doc))
      (funcall pdf-scroll-fun))
     ;; have just a man page
     ((and man
           (not pdf)
           (not R-buffer)
           (not help-page)
           (not my-xwidget)
           (not my-doc))
      (funcall scroll-fun man))
     ;; have just R help window
     ((and R-buffer
           (not pdf)
           (not man)
           (not help-page)
           (not my-xwidget)
           (not my-doc))
      (funcall scroll-fun R-buffer))
     ;; have just help window
     ((and help-page
           (not pdf)
           (not man)
           (not R-buffer)
           (not my-xwidget)
           (not my-doc))
      (funcall scroll-fun help-page))
     ;; have just an xwidget view
     ((and my-xwidget
           (not pdf)
           (not man)
           (not R-buffer)
           (not my-doc))
      (funcall xwidget-scroll-fun my-xwidget))
          ;; have just an doc view
     ((and my-doc
           (not pdf)
           (not man)
           (not R-buffer)
           (not my-xwidget))
      (funcall doc-scroll-fun my-doc))

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
          (setq stop
	        (key-description
	         (read-key-sequence "SPC=pdf, r=R, m=man, h=help, g=quit: ")))
          )))
     ;; contingency function
     (t
      (if (eq pdf (window-buffer (car (window-at-side-list nil 'right))))
          (funcall pdf-scroll-fun)
        (funcall
         scroll-fun
         (window-buffer (car (window-at-side-list nil 'right))))))
     ))
  )


(defun mac-pdf-man-help-or-R-next-page ()
  "Function to move to the next page of a pdf or scroll a help buffer down 
(R, python, man, etc...) when working in a separate, non-pdf buffer."
  (interactive)
  (mac-pdf-man-R-or-help-scroll 10))
  

(defun mac-pdf-man-help-or-R-prev-page ()
  "Function to move to the previous page of a pdf or scroll a help buffer up 
(R, python, man, etc...) when working in a separate, non-pdf buffer."
  (interactive)
  (mac-pdf-man-R-or-help-scroll -10))


(defun mac-pdf-separate-window-goto-page ()
  "Function to go to a specified page in a pdf document when working 
in a separate window."
  (interactive)

;; set up variables
(let ((current-buffer (current-buffer))
      (current-point (point))
      (window-list (window-list))
      (pdf)
      (current-buffer (current-buffer))
      )

  ;; use dolist to find the pdf buffer
  (dolist (buffer window-list)
    (when (eq
           (buffer-local-value 'major-mode (window-buffer buffer))
           'pdf-view-mode)
      (setq pdf (window-buffer buffer))
      ))

  (if pdf
      (progn
        ;; now switch to the pdf, go to the desired page, and return back to the buffer you're working in
        (switch-to-buffer-other-window pdf)
        (global-data-entry-mode 1)
        (call-interactively 'pdf-view-goto-page)
        (global-data-entry-mode -1)
        (switch-to-buffer-other-window current-buffer)
        (goto-char current-point))
    (message "no pdf open!"))
))



(global-set-key (kbd "M-SPC") 'mac-pdf-man-help-or-R-next-page)
(global-set-key (kbd "M-t") 'mac-pdf-man-help-or-R-prev-page)
