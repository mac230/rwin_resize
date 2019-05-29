;; rwin-resize     -*- mode: emacs-lisp; fill-column: 120; eval: (elisp-org-hook); eval: (auto-fill-mode t) -*-

;; my functions for setting up my emacs environment for working with R or
;; python code using emacs as my ide

(defun rwin-resize (arg)
"Re-size windows to my preferred setup with an editing buffer at top left,
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

As of 2019.05.25, now contains code for using R, python, and shell for the lower
editing window.
"
  (interactive "P")
  ;; undo any existing window protections
  (dolist (current-window (window-list))
      (progn
        (window-preserve-size current-window t nil)
        (set-window-dedicated-p current-window nil)
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
    (when (not (and
                (get-process "R")
                (bufferp (get-buffer "*R*"))))
      (progn
        (split-window-vertically)
        (other-window 1)
        (R)
        (sit-for 2)))

    ;; RE-Builder
    (when (not (bufferp (get-buffer "*RE-Builder*")))
      (progn
        (re-builder)
        (reb-change-target-buffer current-buffer)))

    ;; Python
    (when (not (and
                (get-process "Python")
                (bufferp (get-buffer "*Python*"))))
      (run-python))

    ;; shell
    (when (not (and
                (get-process "shell")
              (bufferp (get-buffer "*shell*"))))
      (shell))

    ;; ielm
    (when (not (and
                (get-process "ielm")
              (bufferp (get-buffer "*ielm*"))))
      (ielm))

    (cond
     ((= (prefix-numeric-value arg) 1)
      (setq bottom-window-buffer (get-buffer "*R*")))

     ((= (prefix-numeric-value arg) 2)
      (setq bottom-window-buffer (get-buffer "*shell*")))

     ((= (prefix-numeric-value arg) 3)
      (setq bottom-window-buffer (get-buffer "*ielm*")))

     (t
      (setq bottom-window-buffer (get-buffer "*Python*"))))

    ;; now back to the editing window
    (select-window (car (window-at-side-list nil 'left)))
    (delete-other-windows-vertically)

    ;; a side window is supposed to be unsplittable, so use this
    (display-buffer-in-side-window bottom-window-buffer '(side bottom))
    ;; have to set the size though
    (set-window-text-height
     (get-buffer-window bottom-window-buffer)
     ;; truncate converts a float to integer
     (truncate (* (frame-height) (float 0.2))))

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

(key-chord-define-global "jq" 'rwin-resize)


(defun comint-send-line (buf)
  "Send the current line to a buffer"
  (interactive)
  (let ((shell-input (buffer-substring
                      (line-beginning-position)
                      (line-end-position))))
    (with-current-buffer buf
      (insert shell-input)
      (comint-send-input))))


(defun ielm-send-line (buf)
  "Send the current line to a buffer"
  (interactive)
  (let ((shell-input (buffer-substring
                      (line-beginning-position)
                      (line-end-position))))
    (with-current-buffer buf
      (insert shell-input)
      (ielm-send-input))))


(defun shell-buffer-update-dir-fun ()
  "Keep the working dir of a shell script buffer in sync with the working dir of the shell process buffer."
  (let ((current-buffer (current-buffer))
        (working-dir)
        (inhibit-message t))
    (with-current-buffer "*shell*"
      (setq working-dir (substring (pwd) 10)))
    (set-buffer current-buffer)
    (cd working-dir)))


(defun send-line-R-python-shell-ielm ()
  "Send the current line to R, python, or shell based on context."
  (interactive)

  (cond
   ((or (eq major-mode 'ess-mode)
       (eq (get-buffer "*R*") (window-buffer (car (window-at-side-list nil 'bottom)))))
    (ess-eval-line))

   ((or (eq major-mode 'python-mode)
       (eq (get-buffer "*Python*") (window-buffer (car (window-at-side-list nil 'bottom)))))
    (comint-send-line (process-buffer (get-process "Python"))))

   ((or (eq major-mode 'shell-mode)
       (eq (get-buffer "*shell*") (window-buffer (car (window-at-side-list nil 'bottom)))))
    (progn
      (comint-send-line (process-buffer (get-process "shell")))
      (shell-buffer-update-dir-fun)))

   ((or (eq major-mode 'lisp-interaction-mode)
        (eq major-mode 'emacs-lisp-mode)
        (eq (get-buffer "*ielm*") (window-buffer (car (window-at-side-list nil 'bottom)))))
    (progn
      (ielm-send-line (process-buffer (get-process "ielm")))
   ))))




(defun r-python-shell-or-ielm-send-region ()
  "Send the current region to the R or python process depending on context."
  (interactive)
  (cond

   ;; ess
   ((or (eq major-mode 'ess-mode)
        (eq (get-buffer "*R*") (window-buffer (car (window-at-side-list nil 'bottom)))))
    (ess-eval-region-or-function-or-paragraph nil))

   ;; python
   ((or (eq major-mode 'python-mode)
       (eq (get-buffer "*Python*") (window-buffer (car (window-at-side-list nil 'bottom)))))
    (elpy-shell-send-statement))

   ;; shell
   ((or (eq major-mode 'sh-mode))
    (let ((shell-input (buffer-substring (point) (mark))))
      (with-current-buffer "*shell*"
        (insert shell-input)
        (comint-send-input)
        (sit-for 0.2)
        ;; deal w/ the duplicating prompt effects of sending a region
        (comint-send-input)
        )
      (shell-buffer-update-dir-fun)))

   ;; ielm
   ((or (eq major-mode 'lisp-interaction-mode)
        (eq major-mode 'emacs-lisp-mode)
        (eq (get-buffer "*ielm*") (window-buffer (car (window-at-side-list nil 'bottom)))))
    (let ((ielm-input (progn (mark-sexp -1)
                             (buffer-substring (point) (mark)))))
      (with-current-buffer "*ielm*"
        (insert ielm-input)
        (ielm-send-input))
      (deactivate-mark)))

   (t
    (message "No R, python, shell, or ielm process for this buffer."))
   ))






(defun mac-frame-width (arg)
  "Set frame width w/ custom input generated via data-entry-mode.
  [1] default - 215
  [2] full    - 364
  [3] mid     - 280
  [4] custom
"
  (interactive "P")
  (let* ((keys '("a" "f" "j" "c"))
         (keys-again keys)
         (options '("default (215)" "full (364)" "mid (280)" "custom"))
         (choice-menu '())
         (current-window (selected-window))
         (current-point (point))
         (rwin-arg nil))

    ;; 03.24.2019 - modification to account for window sizes
    ;; being preserved to prevent them from being split by 'rwin-resize'
    ;; this dolist macro undoes the protection, so now call 'rwin-resize'
    ;; at the end to re-protect them once the frame is the desired size.
    (dolist (current-window (window-list))
      (progn
        (window-preserve-size current-window t nil)
        (select-window current-window t)
        (setq window-size-fixed nil)))

    ;; decrementing loop to create the minibuffer text for the re-sizing
    (while options
    (setq choice-menu
          (cons
           (concat
            "\n"
            "                                  "
            (car keys)
            ": "
            (car options)
            "\n") choice-menu)
          options (cdr options)
          keys (cdr keys)))

    ;; set up our display and read the user choice in the minibuffer
    (let* ((choice
            (read-key-sequence (propertize
                                (mapconcat 'identity choice-menu "")
                                'face 'mac-window-select-face)))
           (new-width
            (seq-position keys-again choice)))

  ;; 'cond' function to set frame width; uses position in list of your choice
  (cond
   ((= new-width 0)
    (set-frame-width
     ;; current frame
     nil
     ;; 0.6 * max width
     ;; truncate converts to integer by rounding to 0
     (truncate (* (float 0.6) (nth 3 (assoc 'geometry (car (display-monitor-attributes-list))))))
     ;; don't 'pretend' and set pixelwise
     nil t))
   ((= new-width 1)
    (set-frame-width nil
     (truncate (nth 3 (assoc 'geometry (car (display-monitor-attributes-list)))))
     nil t))
   ((= new-width 2)
    (set-frame-width nil
     (truncate (* (float 0.75) (nth 3 (assoc 'geometry (car (display-monitor-attributes-list))))))
     nil t))
   ((= new-width 3)
    (progn
      (global-data-entry-mode)
      (set-frame-width nil
       (truncate (* (float (read-number "fractional width: ")) (nth 3 (assoc 'geometry (car (display-monitor-attributes-list))))))
       nil t)
      (global-data-entry-mode -1)))
   (t
    (set-frame-width nil
     (truncate (* (float 0.6) (nth 3 (assoc 'geometry (car (display-monitor-attributes-list))))))
     nil t))
                     ))
  (select-window current-window)
  (goto-char current-point)
  (message "")
  (rwin-resize (prefix-numeric-value arg))
  ))


