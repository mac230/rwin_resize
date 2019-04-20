# rwin-resize     -*- mode: emacs-lisp; fill-column: 120; eval: (elisp-org-hook); eval: (auto-fill-mode t) -*-

;; my functions for setting up my emacs environment for working with R or
;; python code using emacs as my ide

(defun rwin-resize (arg)
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
docs/forums recommend doing this w/ 'split-window-threshold', which doesn't work
here because the width of the 3 windows is the same.
"
  (interactive "P")
  ;; undo any existing window protections
  (dolist (current-window (window-list))
      (progn
        (window-preserve-size current-window t nil)
        (select-window current-window t)
        (setq window-size-fixed nil)))

  ;; start in the usual editing window; make that the reference point
  (select-window (car (window-at-side-list nil 'left)))
  (let ((current-window (selected-window))
        (current-buffer (current-buffer))
        (current-point (point))
        (bottom-window-buffer))

    ;; some contingencies to prevent issues w/ this when called from
    ;; R or RE-Builder
    (when (not (bufferp (get-buffer "*Ibuffer*")))
      (ibuffer))
    (when (or (eq current-buffer (get-buffer "*R*"))
              (eq current-buffer (get-buffer "*RE-Builder*")))
      (setq current-buffer (get-buffer "*Ibuffer*")))

        ;; start by making sure we have the requisite buffers for R, RE-Builder, and python
    ;; 03.25.2019 - added function to change RE-Builder target buff and put point
    ;; back into the top window.
    ;; R
    (when (not (bufferp (get-buffer "*R*")))
        (R))
    ;; RE-Builder
    (when (not (bufferp (get-buffer "*RE-Builder*")))
      (progn
        (re-builder)
        (reb-change-target-buffer current-buffer)))
    ;; Python
    (when (not (bufferp (get-buffer "*Python*")))
      (run-python))

    (if (= (prefix-numeric-value arg) 1)
        (setq bottom-window-buffer (get-buffer "*R*"))
      (setq bottom-window-buffer (get-buffer "*Python*")))

    ;; now back to the editing window
    (select-window (car (window-at-side-list nil 'left)))
    (delete-other-windows-vertically)

    ;; a side window is supposed to be unsplittable, so use this
    (display-buffer-in-side-window bottom-window-buffer '(side bottom))
    ;; have to set the size though
    (set-window-text-height (get-buffer-window bottom-window-buffer) 10)

    (display-buffer "*RE-Builder*"
                    '((display-buffer-reuse-window
                       display-buffer-below-selected)
                      (window-height . 5)))

    ;; make the windows for R and RE-builder dedicated to these buffers
    (set-window-dedicated-p (get-buffer-window bottom-window-buffer) t)
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



