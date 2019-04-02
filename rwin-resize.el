# rwin-resize     -*- mode: emacs-lisp; fill-column: 120; eval: (elisp-org-hook); eval: (auto-fill-mode t) -*-

;; my functions for setting up my emacs environment for working with R or
;; python code using emacs as my ide

(defun rwin-resize ()
  "03.25.2019 version
Re-size windows to my preferred setup with an editing buffer at top left,
RE-Builder in a very short middle window, and R in a short bottom window. 
This now uses a 'side-window' for R, effectively making it the only buffer
that can be displayed in the bottom window and sets 'window-size-fixed' for
RE-Builder.  

The code for locking the size of RE-Builder is probably redundant
since it uses both options 'window-preserve-size' and 'window-size-fixed', but
it seems to work and that's what I'm interested in. 

Note that the general goal here is to keep RE-Builder and R from being split, 
so that only the top window is split to display help/man/R help buffers.  Most
forums recommend doing this w/ 'split-window-threshold', which doesn't work here
because the width of the 3 windows is the same. 
"
  (interactive)
  ;; this let binding is needed if you call this from somewhere
  ;; other than the usual editing window
  (let ((current-window (selected-window))
        (current-buffer (current-buffer))
        (current-point (point)))

    ;; start by making sure we have the requisite buffers for R, RE-Builder, and python
    ;; 03.25.2019 - added function to change RE-Builder target buff and put point
    ;; back into the top window. 
    (when (not (bufferp (get-buffer "*R*")))
      (progn
        (R)
        (select-window (car (window-at-side-list nil 'left)))))
    (when (not (bufferp (get-buffer "*RE-Builder*")))
      (progn
        (re-builder)
        (reb-change-target-buffer current-buffer)
        (select-window (car (window-at-side-list nil 'left)))))
    (when (not (bufferp (get-buffer "*Python*")))
      (run-python))

    (select-window (car (window-at-side-list nil 'left)))
    (delete-other-windows-vertically)

    ;; a side window is supposed to be unsplittable, so use this
    (display-buffer-in-side-window (get-buffer "*R*") '(side bottom))
    ;; have to set the size though
    (set-window-text-height (get-buffer-window "*R*") 15)

    (display-buffer "*RE-Builder*"
                    '((display-buffer-reuse-window
                       display-buffer-below-selected)
                      (window-height . 5)))

    ;; make the windows for R and RE-builder dedicated to these buffers
    (set-window-dedicated-p (get-buffer-window "*R*") t)
    (set-window-dedicated-p (get-buffer-window "*RE-Builder*") t)
    (select-window (get-buffer-window "*RE-Builder*") t)
    (setq window-size-fixed 'width)

    ;; keep the height of the RE-Builder window constant
    ;; this is to prevent it from being split
    (window-preserve-size (get-buffer-window "*RE-Builder*") nil t)
    (window-preserve-size (get-buffer-window "*RE-Builder*") t t)

    ;; this sets how the window is split.  I set it to 'above', since this
    ;; shouldn't be permitted, i.e., force all new windows to split the top window
    (set-window-parameter
     (get-buffer-window "*RE-Builder*")
     'split-window '(split-window (get-buffer-window "*RE-Builder*") nil 'above))
    (select-window current-window)
    (goto-char current-point)))



(defun python-win-resize ()
  "03.25.2019 version
Re-size windows to my preferred setup with an editing buffer at top left,
RE-Builder in a very short middle window, and R in a short bottom window. 
This now uses a 'side-window' for R, effectively making it the only buffer
that can be displayed in the bottom window and sets 'window-size-fixed' for
RE-Builder.  

The code for locking the size of RE-Builder is probably redundant
since it uses both options 'window-preserve-size' and 'window-size-fixed', but
it seems to work and that's what I'm interested in. 

Note that the general goal here is to keep RE-Builder and R from being split, 
so that only the top window is split to display help/man/R help buffers.  Most
forums recommend doing this w/ 'split-window-threshold', which doesn't work here
because the width of the 3 windows is the same. 
"
  (interactive)
  ;; this let binding is needed if you call this from somewhere
  ;; other than the usual editing window
  (let ((current-window (selected-window))
        (current-buffer (current-buffer))
        (current-point (point)))

    ;; start by making sure we have the requisite buffers for R, RE-Builder, and python
    ;; 03.25.2019 - added function to change RE-Builder target buff and put point
    ;; back into the top window. 
    (when (not (bufferp (get-buffer "*R*")))
      (progn
        (R)
        (select-window (car (window-at-side-list nil 'left)))))
    (when (not (bufferp (get-buffer "*RE-Builder*")))
      (progn
        (re-builder)
        (reb-change-target-buffer current-buffer)
        (select-window (car (window-at-side-list nil 'left)))))
    (when (not (bufferp (get-buffer "*Python*")))
      (run-python))

    (select-window (car (window-at-side-list nil 'left)))
    (delete-other-windows-vertically)

    ;; a side window is supposed to be unsplittable, so use this
    (display-buffer-in-side-window (get-buffer "*Python*") '(side bottom))
    ;; have to set the size though
    (set-window-text-height (get-buffer-window "*Python*") 15)

    (display-buffer "*RE-Builder*"
                    '((display-buffer-reuse-window
                       display-buffer-below-selected)
                      (window-height . 5)))

    ;; make the windows for R and RE-builder dedicated to these buffers
    (set-window-dedicated-p (get-buffer-window "*Python*") t)
    (set-window-dedicated-p (get-buffer-window "*RE-Builder*") t)
    (select-window (get-buffer-window "*RE-Builder*") t)
    (setq window-size-fixed 'width)

    ;; keep the height of the RE-Builder window constant
    ;; this is to prevent it from being split
    (window-preserve-size (get-buffer-window "*RE-Builder*") nil t)
    (window-preserve-size (get-buffer-window "*RE-Builder*") t t)

    ;; this sets how the window is split.  I set it to 'above', since this
    ;; shouldn't be permitted, i.e., force all new windows to split the top window
    (set-window-parameter
     (get-buffer-window "*RE-Builder*")
     'split-window '(split-window (get-buffer-window "*RE-Builder*") nil 'above))
    (select-window current-window)
    (goto-char current-point)))


(defun r-or-python-win-resize (arg)
  "Re-size for R or Python processes."
  (interactive "P")
  (if (= (prefix-numeric-value arg) 1)
      ;; no prefix arg
      (rwin-resize)
    ;; prefix arg
    (python-win-resize))
  )
