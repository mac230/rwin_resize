;; my functions for setting up my emacs environment for working with R,
;; python, elisp, shell code using emacs as my ide.  sets up a
;; REPL environment for each of the above languages


;; -----
;; miscellaneous helper functions and variable settings

;; side-window deletion
(defun mac-side-window-deleter ()
  "Delete any existing side windows."
  (dolist (w (window-list))
    (when (window--side-window-p w)
      (delete-side-window w)))
  )

;; helper function for selecting a process
;; select a process buffer and place it
(defun mac-process-placer (window &optional my-process)
  "Select a process and place its buffer in the appropriate window."
 (let* ((process-names '(R shell ielm Python))
	(process-buf))
   (if my-process
       (window--display-buffer
        my-process
        window
        'reuse nil nil)
     (progn     
       (ivy-read "process: " process-names
                 :action
                 (lambda (arg)
                   (setq process-buf
                         (process-buffer
                          (get-process (format "%s" arg))))))
       (window--display-buffer
        process-buf
        window
        'reuse nil nil)))))

;; necessary to keep R help from throwing an error
(setq ess-help-pop-to-buffer nil)


;; always display the backtrace buffer in the selected window
;; makes it easier to kill it. 
(defun mac-backtrace-buffer-display (buffer alist)
  "My function for displaying the backtrace buffer."
  (window--display-buffer
   buffer
   (selected-window)
   'reuse alist nil))


;; -----
;; mac-window-config and help buffer display functions

;; mac-window-config-1 - one window
(defun mac-window-config-1 ()
  "Basic window configuration; just one buffer."
  (select-window (car (window-at-side-list nil 'left)))
  (delete-other-windows)
  (mac-side-window-deleter)  
  ;; don't split into top bottom in this layout
  (setq display-buffer-alist 
      '(("^\\*Help\\*\\|^\\*Man.**\\|^\\*WoMan .*\\*\\|^\\*help\\[R\\].*\\*\\|.*\\.pdf\\|*info\\*\\|*eww\\*.*\\|*R dired*\\|.*jpeg\\|.*jpg\\|.*tiff\\|\\.el\\|\\.c\\|*sdcv\\*"
	 (display-buffer-reuse-window
	  mac-window-config-1-help-display
	  display-buffer-pop-up-window
	  ))
	("*Backtrace*"
	 (mac-backtrace-buffer-display)))
      ;; n. columns to split into left/right
      split-width-threshold 80
      ;; n. lines to split into top/bottom
      split-height-threshold nil))

(defun mac-window-config-1-help-display (buffer alist)
  (cond
   ((> (length (window-list)) 1)
    (window--display-buffer
     buffer
     (car (window-at-side-list nil 'right))
     'reuse alist nil))
    (t
     (display-buffer-pop-up-window buffer alist))
    ))


;; -----
;; mac-window-config-2 - pdf (right), notes/coding (left_top), process (left_bot)
(defun mac-window-config-2-help-display (buffer alist)
  (window--display-buffer
   buffer
   (car (window-at-side-list nil 'right))
   'reuse alist nil)
  )

(defun mac-window-config-2 ()
  "Window configuration with:
  [1] notes/coding buffer (top_left)
  [2] process (bottom_left)
  [3] reading buffer (right)
Requires a wide frame, so set frame width immediately."
  (let ((proc-window))
  (select-window (car (window-at-side-list nil 'left)))
  (delete-other-windows-internal)
  (mac-side-window-deleter)
  (set-frame-width
     ;; current frame
     nil
     (nth 3 (assoc 'geometry (car (display-monitor-attributes-list))))
     ;; don't 'pretend' and set pixelwise
     nil t)
  (split-window-horizontally)
;;  (display-buffer-in-side-window (current-buffer) '((side . right)))
  (split-window-vertically -12)
  (setq display-buffer-alist 
      '(("^\\*Help\\*\\|^\\*Man.**\\|^\\*WoMan .*\\*\\|^\\*help\\[R\\].*\\*\\|.*\\.pdf\\|*info\\*\\|*eww\\*.*\\|*R dired*\\|.*jpeg\\|.*jpg\\|.*tiff\\|\\.el\\|\\.c\\|*sdcv\\*"
	 (display-buffer-reuse-window
	  mac-window-config-2-help-display))
	("*Backtrace*"
	 (mac-backtrace-buffer-display)))
      ;; n. columns to split into left/right
      split-width-threshold 80
      ;; n. lines to split into top/bottom
      split-height-threshold 40
      proc-window (car (reverse (window-at-side-list nil 'left))))
  (mac-process-placer proc-window)
  ))
;; (mac-window-config-2-help-display (get-buffer "*eshell*") nil)
;; (mac-window-config-2)


;; -----
;; window config 3 - coding/notes (left); process (right)
(defun mac-window-config-3-redisplay-helper (right-win right-buffer)
  "Helper function for \'mac-window-config-3-help-display\'. "
  (when
      (= 1 (length (window-at-side-list nil 'right)))
    (progn
      (window--try-to-split-window right-win nil)
      (window-resize right-win 10)))
  (window--display-buffer
   right-buffer
   (car (reverse (window-at-side-list nil 'right)))
   'reuse nil nil))



(defun mac-window-config-3-help-display (buffer alist)
  (let* ((win-return (lambda (side) (car (window-at-side-list nil side))))
	 (right-win (funcall win-return 'right))
         (left-win (funcall win-return 'left))
	 (right-buffer (window-buffer right-win)))
    ;; split windows if you only have 1 window in frame
    (when (eq left-win right-win)
      (split-window-horizontally))
    ;; prefer process buffers for 'right-buffer'
    (dolist (w (window-list))
      (with-current-buffer (window-buffer w)
	(when
	    (or
	     (eq major-mode 'inferior-ess-mode)
	     (eq major-mode 'shell-mode)
	     (eq major-mode 'inferior-emacs-lisp-mode)
	     (eq major-mode 'inferior-python-mode)))
	(setq right-buffer (window-buffer w))))
    ;; display function
    (window--display-buffer
     buffer
     right-win
     'reuse alist nil)
    (run-with-timer 0.25 nil 'mac-window-config-3-redisplay-helper right-win right-buffer)
    ))


(defun mac-window-config-3 (&optional my-process)
  "Window configuration with: 
   [1] a coding/notes buffer (left)
   [2] a process buffer (right)."
  (let ((proc-window))
    (select-window (car (window-at-side-list nil 'left)))
    (delete-other-windows-internal)
    (set-frame-width
     ;; current frame
     nil
     (nth 3 (assoc 'geometry (car (display-monitor-attributes-list))))
     ;; don't 'pretend' and set pixelwise
     nil t)
    (split-window-horizontally)
    (setq display-buffer-alist 
	  '(("^\\*Help\\*\\|^\\*Man.**\\|^\\*WoMan .*\\*\\|^\\*help\\[R\\].*\\*\\|.*\\.pdf\\|*info\\*\\|*eww\\*.*\\|*R dired*\\|.*jpeg\\|.*jpg\\|.*tiff\\|\\.el\\|\\.c\\|*sdcv\\*"
	     (display-buffer-reuse-window
	      mac-window-config-3-help-display))
	    ("*Backtrace*"
	 (mac-backtrace-buffer-display)))
	  ;; n. columns to split into left/right
	  split-width-threshold 80
	  ;; n. lines to split into top/bottom
	  split-height-threshold 20
	  proc-window (car (window-at-side-list nil 'right)))
    (mac-process-placer proc-window my-process))
  )

;; (mac-window-config-3)
;; (mac-window-config-3-help-display (get-buffer "*eshell*") nil)


;; -----
;; window config 4 - narrow frame with process on bottom
(defun mac-window-config-4 ()
  "Window configuration for a narrow(er) frame.  Has
    [1] coding/notes (top)
    [2] process (bottom)."
  (let ((proc-window))
    (select-window (car (window-at-side-list nil 'left)))
    (delete-other-windows-internal)
    ;; for now don't want to set frame width
    ;; can revisit later
    ;;    (set-frame-width
    ;;     nil
    ;;     (round (* (nth 3 (assoc 'geometry (car (display-monitor-attributes-list)))) 0.55))
     nil t)
    (display-buffer-in-side-window
     (current-buffer) '((side . bottom)))
    (window-resize
     (car (window-at-side-list nil 'bottom))
     (- 12 (window-height (car (window-at-side-list nil 'bottom)))) nil)
    (setq display-buffer-alist 
	  '(("^\\*Help\\*\\|^\\*Man.**\\|^\\*WoMan .*\\*\\|^\\*help\\[R\\].*\\*\\|.*\\.pdf\\|*info\\*\\|*eww\\*.*\\|*R dired*\\|.*jpeg\\|.*jpg\\|.*tiff\\|\\.el\\|\\.c\\|*sdcv\\*"
	     (mac-window-config-4-help-display))
	    ("*Backtrace*"
	 (display-buffer-reuse-window
	  mac-backtrace-buffer-display)))
	  ;; n. columns to split into left/right
	  split-width-threshold 80
	  ;; n. lines to split into top/bottom
	  split-height-threshold 100
	  proc-window (car (reverse (window-at-side-list nil 'left))))
    (mac-process-placer proc-window))


;; use the top left (editing) window, since the disp. is narrow
(defun mac-window-config-4-help-display (buffer alist)
  (window--display-buffer
   buffer
   (selected-window)
   'reuse alist nil)
  )

;; (mac-window-config-4)
;; (mac-window-config-4-help-display (get-buffer "*eshell*") nil)


;; -----
;; window config 5 - side x side windows; no process
(defun mac-window-config-5 ()
  "Window configuration with:
  [1] notes/coding buffer (left)
  [2] reading buffer (right)
Requires a wide frame, so set frame width immediately."
  (select-window (car (window-at-side-list nil 'left)))
  (delete-other-windows-internal)
  (mac-side-window-deleter)
  (set-frame-width
     ;; current frame
     nil
     (nth 3 (assoc 'geometry (car (display-monitor-attributes-list))))
     ;; don't 'pretend' and set pixelwise
     nil t)
  (split-window-horizontally)
  (setq display-buffer-alist 
      '(("^\\*Help\\*\\|^\\*Man.**\\|^\\*WoMan .*\\*\\|^\\*help\\[R\\].*\\*\\|.*\\.pdf\\|*info\\*\\|*eww\\*.*\\|*R dired*\\|.*jpeg\\|.*jpg\\|.*tiff\\|\\.el\\|\\.c\\|*sdcv\\*"
	 (display-buffer-reuse-window
	  mac-window-config-5-help-display))
	("*Backtrace*"
	 (mac-backtrace-buffer-display)))
      ;; n. columns to split into left/right
      split-width-threshold 80
      ;; n. lines to split into top/bottom
      split-height-threshold 40
      proc-window (car (reverse (window-at-side-list nil 'left))))
  )


(defun mac-window-config-5-help-display (buffer alist)
  (when (eq
	 (car (window-at-side-list nil 'left))
	 (car (window-at-side-list nil 'right)))
    (split-window-horizontally))
  (window--display-buffer
   buffer
   (car (window-at-side-list nil 'right))
   'reuse alist nil)
  )

;; (mac-window-config-5)
;; (mac-window-config-5-help-display (get-buffer "*eshell*") nil)




;; -----
;; master function for calling a config
(defun mac-select-window-config-master ()
  "Master function for choosing a window configuration."
  (interactive)
  (let* ((config '("1" "2" "3" "4" "5"))
	 (choice (string-to-number
		  (ivy-read "choice: "
			    config
			    :keymap data-entry-mode-map))))
    (cond
     ((= choice 1)
      (mac-window-config-1))
     ((= choice 2)
      (mac-window-config-2))
     ((= choice 3)
      (mac-window-config-3))
     ((= choice 4)
      (mac-window-config-4))
     ((= choice 5)
      (mac-window-config-5))
     (t
      (mac-window-config-1))
    )))

(key-chord-define-global "jq" 'mac-select-window-config-master)

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

As of 2019.05.25, now contains code for using R, python, shell,
and ielm for the lower editing window.
"
  (interactive "P")
  ;; undo any existing window protections
  (dolist (current-window (window-list))
      (progn
        (window-preserve-size current-window t nil)
        (set-window-dedicated-p current-window nil)
        (select-window current-window t)
        (setq window-size-fixed nil)))

  (window-configuration-to-register ?a)
  ;; start in the usual editing window; make that the reference point
  (select-window (car (window-at-side-list nil 'left)))
  (let ((current-window (selected-window))
        (current-buffer (current-buffer))
        (current-point (point))
        (bottom-window-buffer)
	(interval 0)) 

    ;; some contingencies to prevent issues w/ this when called from
    ;; R or RE-Builder
    (when (not (bufferp (get-buffer "*Ibuffer*")))
      (ibuffer))
    (when (or (eq current-buffer (get-buffer "*R*"))
              (eq current-buffer (get-buffer "*RE-Builder*")))
      (setq current-buffer (get-buffer "*Ibuffer*")))

    (when (or
	   (not (bufferp (get-buffer "*R*")))
	   (not (bufferp (get-buffer "*shell*")))
	   (not (bufferp (get-buffer "*Python*")))
	   (not (bufferp (get-buffer "*ielm*"))))
      (setq interval 3))
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

    ;; calc
    (when (not
           (bufferp (get-buffer "*Calculator*")))
      (calc))

    ;; sit while these set up, then restore window config
    (sit-for interval)
    (when (> interval 1)
      (jump-to-register ?a))

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


(defun mac-start-all-processes ()
  "Start the various processes I interact w/ in REPL setup."
  (let ((cb (current-buffer))
	(process-names '(R shell ielm Python)))
    (window-configuration-to-register ?a)

    ;; remove processes already running from the list
    (dolist (p process-names)
      (when
	  (get-process (format "%s" p))
	(setq process-names (delq p process-names))))

    ;; start any processes not already running
    (while process-names
      (cond
       ((member 'R process-names)
	(progn
	  (R)
	  (setq process-names (delq 'R process-names))))
       ((member 'shell process-names)
	(progn
	  (shell)
	  (setq process-names (delq 'shell process-names))))
       ((member 'Python process-names)
	(progn
	  (run-python)
	  (setq process-names (delq 'Python process-names))))
       ((member 'ielm process-names)
	(progn
	  (ielm)
	  (setq process-names (delq 'ielm process-names))))
       (t
	(setq process-names nil))))
    (when (not (bufferp (get-buffer "*Calculator*")))
      (calc))
    (when (not (bufferp (get-buffer "*eshell*")))
      (eshell))
    (sit-for 4)
    (jump-to-register ?a))
  (setq comment-add 1)
  )


(defun comint-send-line (buf)
  "Send the current line to a buffer"
  (interactive)
  (let ((shell-input (buffer-substring
                      (line-beginning-position)
                      (line-end-position))))
    (with-current-buffer buf
      (when (not (eobp))
        (end-of-buffer))
      (insert shell-input)
      (comint-send-input))))


(defun ielm-send-line (buf)
  "Send the current line to a buffer"
  (interactive)
  (let ((shell-input (buffer-substring
                      (line-beginning-position)
                      (line-end-position))))
    (with-current-buffer buf
      (when (not (eobp))
        (end-of-buffer))
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

(defun send-line-helper ()
  "Context-aware function."
  (interactive)
  (if (eq major-mode 'latex-mode)
      (ebib)
    (send-line-R-python-shell-ielm)
    ))

(defun send-line-R-python-shell-ielm ()
  "Send the current line to R, shell, ielm, or python based on context."
  (interactive)
  (let* ((w (window-list))
	 (modes-fun (lambda (arg)
		      (with-current-buffer (window-buffer arg)
			major-mode)))
	 (modes (map 'list modes-fun w)))
    (cond
     ((member 'inferior-ess-r-mode modes)
      (ess-eval-line))
     ((member 'shell-mode modes)
      (progn
	(comint-send-line
	 (process-buffer (get-process "shell")))
	(shell-buffer-update-dir-fun)))
     ((member 'inferior-emacs-lisp-mode modes)
      (ielm-send-line
       (process-buffer (get-process "ielm"))))
     ((member 'inferior-python-mode modes)
      (comint-send-line (process-buffer (get-process "Python"))))
     (t
      (message "No process buffer to send input to in window list")))
     )
  )


(defun r-python-shell-or-ielm-send-region ()
  "Send the current line to R, shell, ielm, or python based on context."
  (interactive)
  (let* ((w (window-list))
	 (modes-fun (lambda (arg)
		      (with-current-buffer (window-buffer arg)
			major-mode)))
	 (modes (map 'list modes-fun w)))
    (cond
     ;; ESS 
     ((member 'inferior-ess-r-mode modes)
      (ess-eval-region-or-function-or-paragraph nil))
     ;; shell
     ((member 'shell-mode modes)
      (let ((shell-input (buffer-substring (point) (mark))))
	(with-current-buffer "*shell*"
	  (when (not (eobp))
	    (end-of-buffer))
	  (insert shell-input)
	  (comint-send-input)
	  (sit-for 0.2)
	  ;; deal w/ the duplicating prompt effects of sending a region
	  (comint-send-input)
	  )
	(shell-buffer-update-dir-fun)))
      ;; ielm
     ((member 'inferior-emacs-lisp-mode modes)
      (let ((ielm-input (progn (mark-sexp -1)
			       (buffer-substring (point) (mark)))))
	(with-current-buffer "*ielm*"
	  (when (not (eobp))
	    (end-of-buffer))
	  (insert ielm-input)
	  (ielm-send-input))
	(deactivate-mark)))
      ((member 'inferior-python-mode modes)
       (elpy-shell-send-statement))
     (t
      (message "No process buffer to send input to in window list")))
     )
  )





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
         (options '("default - 0.55" "full - 1.0" "mid - 0.5" "custom"))
         (choice-menu '())
         (current-window (selected-window))
         (current-point (point))
         (rwin-arg nil)
         (bottom-buffer
	  (buffer-name
	   (window-buffer
	    (car (window-at-side-list nil 'bottom)))))
         (pfix-arg)
         )

    ;; use this to preserve the buffer in the lower window by supplying it to rwin-resize at the end
    (cond
     ((string= bottom-buffer "*R*")
      (setq pfix-arg 1))
     ((string= bottom-buffer "*shell*")
      (setq pfix-arg 2))
     ((string= bottom-buffer "*ielm*")
      (setq pfix-arg 3))
     ((string= bottom-buffer "*Python*")
      (setq pfix-arg 4))
     (t
      (setq pfix-arg 0)))

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
       ;; 0.6 width
       ((= new-width 0)
	(set-frame-width
	 ;; current frame
	 nil
	 ;; 0.55 * max width
	 ;; truncate converts to integer by rounding to 0
	 (truncate
	  (* (float 0.55)
	     (nth 3 (assoc 'geometry (car (display-monitor-attributes-list))))))
	 ;; don't 'pretend' and set pixelwise
	 nil t))
       ;; 1.0 (full) width
       ((= new-width 1)
	(set-frame-width
	 nil
	 (truncate (nth 3 (assoc 'geometry (car (display-monitor-attributes-list)))))
	 nil t))
       ;; 0.5 width
       ((= new-width 2)
	(set-frame-width
	 nil
	 (truncate
	  (* (float 0.5)
	     (nth 3 (assoc 'geometry (car (display-monitor-attributes-list))))))
			 nil t))
       ((= new-width 3)
	(progn
	  (global-data-entry-mode)
	  (set-frame-width
	   nil (truncate
		(* (float (read-number "fractional width: "))
		   (nth 3 (assoc 'geometry (car (display-monitor-attributes-list))))))
			   nil t)
	  (global-data-entry-mode -1)))
       (t
	(set-frame-width
	 nil
	 (truncate (* (float 0.55)
		      (nth 3 (assoc 'geometry (car (display-monitor-attributes-list))))))
			 nil t))
       ))
  (select-window current-window)
  (goto-char current-point)
  (message "")
  ;; 2020.04.30 shouldn't need this any longer 
  ;; conditionally call rwin-resize if we're using a process buffer 
  ;;  (when (> pfix-arg 0)
  ;;    (rwin-resize pfix-arg))
  ))


(defun mac-ess-mark-statement ()
  "Mark a statement in R code so that the function name, not just the text bounded by parentheses, is grabbed."
  (interactive)
  (let ((count 1))

    ;; determine what type of buffer we're in
    ;; need to distinguish an ess-r-mode / org R
    ;; src blocks from everything else 
    ;; 2023.11.28 - updating because 'org-in-src-block-p'
    ;; errors in emacs 30 when called in a non-org buffer
    (when (eq major-mode 'ess-r-mode)
      (setq count 2))

    (when (eq major-mode 'org-mode)
      (when (and (org-in-src-block-p)
                 (string= "R" (car (org-babel-get-src-block-info))))
        (setq count 2)))

  (cond

   ;; at the right parenthesis (statement end)
   ((looking-back "\)" (- (point) 1) nil)
    (progn
      (set-mark (point))
      (backward-sexp count)))

   ;; at the left parenthesis (statement beginning)
   ((looking-at "\(")
    (progn
      (forward-sexp)
      (set-mark (point))
      (backward-sexp count)))

   ;; in the function name
   ((looking-at "[a-zA-Z0-9._]+(")
    (progn
      (re-search-forward "\(" nil t 1)
      (goto-char (match-beginning 0))
      (forward-sexp)
      (set-mark (point))
      (backward-sexp count)))

   ;; inside the parens
   (t
    (if (= count 2)
        (progn
          ;; 2023.11.28 - clause for
          ;; modifying the syntax
          ;; entry for '.' so that
          ;; functions like 'write.table'
          ;; are fully marked.  should
          ;; be a buffer-local change 
          (modify-syntax-entry ?. "_")
          (re-search-backward "^[^=]+\\([a-zA-Z0-9._]\\)+\\((\\)" nil nil 1)
          (goto-char (match-beginning 1))
          (forward-sexp)
          (set-mark (point))
          (backward-sexp count))
      (progn
        (re-search-backward "\(" nil nil 1)
        (mark-sexp)))))
    (exchange-point-and-mark)))


;; -----
;; to make changes and have them take effect:
;; 1. eval this w/ 'C-M-x' (eval-defun)
;; 2. re-eval the minor-mode below
;; 3. dis-able then re-enable the minor mode
(defvar mc-r-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-\\")   'mac-info-other-window)
    (define-key map (kbd "C-c k") 'r-python-shell-or-ielm-send-region)
    (define-key map (kbd "C-j")   'send-line-helper)
    (define-key map (kbd "C-c l") 'r-object-send)
    (define-key map (kbd "C-c c") 'goto-line)
    (define-key map (kbd "C-w")   'kill-ring-save)
    (define-key map (kbd "M-w")   'kill-region)
    (define-key map (kbd "M-r")   'mac-ess-mark-statement)
    (define-key map (kbd "C-c u") (lambda () (interactive) (mac-ins) (insert "()") (backward-char 1)))
    (define-key map (kbd "C-c r") 'copy-to-register)
    (define-key map (kbd "C-c b") (lambda () (interactive) (mac-ins) (insert "{}") (backward-char 1)))
    (define-key map (kbd "C-c y") 'bookmark-jump)
    (define-key map (kbd "C-c n") 'insert-register)
    (define-key map (kbd "C-c w") 'mn-weather)
    (define-key map (kbd "C-c 3") 'wc-region)
    (define-key map (kbd "M-m")   'word-b-f)
       map)
   "Keymap for r commands.")

(define-minor-mode mc-r-mode
  "Different ESS eval commands for mc."
  :lighter " mc-R-mode"
  :keymap mc-r-map
  :global t)

(mc-r-mode 1)

(defun symbol-hydra-helper (arg)
  "Helper function that allows deleting active regions."
  (interactive)
  (cond
   ((and
     (region-active-p)
     (eq delete-selection-mode t))
    (progn
      (delete-region (point) (mark))
      (insert arg)))
   (t
    (insert arg))))



(defun sep-insert ()
 "Insert \"-----\" and move to the next line.  I use this to demarcate separate ideas in my notes.
Inserts this separator as a comment in R, python, and shell modes."
 (interactive)
 ;; go to the end of the indicated line, then search backward for non-whitespace text.
 ;; this is the position the separator should be positioned relative to
 ;; This way, the function works whether you remember to hit it when
 ;; you've accidentally used 'RET' to insert a newline.
 (end-of-line)

 (let ((fwd-bound (save-excursion (forward-line 3) (line-end-position)))
       (sep))

   ;; determine which separator to use based on major mode
   (cond
    ((or
        (eq major-mode 'ess-r-mode)
        (eq major-mode 'python-mode)
        (eq major-mode 'sh-mode))
     (setq sep "## -----\n## "))

    ((or
       (eq major-mode 'emacs-lisp-mode)
       (eq major-mode 'lisp-interaction-mode))
     (setq sep ";; -----\n;; "))

    (t
     (setq sep "-----\n")))

 ;; make sure we don't get an eobp error (common w/ narrowed buffer)
 (when (eobp)
     (progn
       (insert "\n")
       (previous-line)))

     ;; always go to the end of the line so that the text on a line doesn't get broken
     (end-of-line)
     (cond

      ;; 1 - at the beginning of a buffer -> don't insert newline above
      ((or
        (bobp)
        (eq (line-number-at-pos) 1)              ;; regex for closest non-blank line
        (not (save-excursion (re-search-backward "\\(^ *?[^ \t\n\f$]\\)\\|\\(^ *?[})]\\)" nil t))))
       (progn (beginning-of-line) (insert sep) (message "1!")))

      ;; 2 - 2 blank lines present between paragraphs/blocks
      ((and
                                            ;; regex for closest non-blank line
        (save-excursion (re-search-backward "\\(^ *?[^ \t\n\f$]\\)\\|\\(^ *?[})]\\)" nil t))
        (save-excursion (re-search-forward "^\\( *\n\\|\n+ \\)\\{2\\}" fwd-bound t)))
       (progn (re-search-backward "\\(^ *?[^ \t\n\f$]\\)\\|\\(^ *?[})]\\)" nil t)
                                 ;; regex for blank lines
              (re-search-forward "^\\( *\n\\|\n+ \\)\\{2\\}" fwd-bound t)
              (beginning-of-line)
              (insert sep)
              (end-of-line)
              (message "2!")))

      ;; 3 - blank lines preceding point
      ((and
        (looking-back "^\\( *\n\\|\n+ \\)")
        (save-excursion (re-search-backward "\\(^ *?[^ \t\n\f$]\\)\\|\\(^ *?[})]\\)" nil t)))
       (progn
         (push-mark)
         (re-search-backward "\\(^ *?[^ \t\n\f$]\\)\\|\\(^ *?[})]\\)" nil t)
         (end-of-line)
         (delete-region (point) (mark))
         (insert (concat "\n\n\n" sep))
         (end-of-line)
         (message "3!")))

      ;; 4 - no blank lines
      ((save-excursion (re-search-backward "\\(^ *?[^ \t\n\f$]\\)\\|\\(^ *?[})]\\)" nil t))
       (progn
         (push-mark)
         (re-search-backward "\\(^ *?[^ \t\n\f$]\\)\\|\\(^ *?[})]\\)" nil t)
         (end-of-line)
         (delete-region (point) (mark))
         (insert (concat "\n\n\n" sep))
         (end-of-line)
         (message "4!")))
      )
     ))




(defun nssend-helper ()
  "Helper function for \'nssned\'."
  (interactive)
  (let ((expr))

  (progn
    (push-mark (point) nil nil)
    (re-search-backward "j" (line-beginning-position) t)
    (replace-match "")
    (setq expr (buffer-substring-no-properties (point) (mark)))

    ;; eval in R; this way can check against calc ouput
    (ess-send-string (ess-get-process) expr)
    (exchange-point-and-mark)

    ;; eval in calc and insert the result
    (insert (concat " = " (calc-eval expr) " "))
    (end-of-line))))


(defun nssend ()
  "Function to quickly return the value of a calculation.  The function searches
backwards for a \'j\' character to mark the beginning of an expression then makes
the region from the end of the line to the \'j\' the expression that is then
evaluated in calc and R.  Evaluation in both programs provides a means of
double-checking the results, since both R and calc have their idiosyncracies."
  (interactive)
  (let ((current-point (point)))

    ;; contingency for R evaluation
    (when (not ess-local-process-name)
      (ess-switch-process))

    ;; set the precision of the output we get from calc
    (calc-precision 4)

    ;; clean up any existing output, should it exist, and position point
    (beginning-of-line)
    (if (save-excursion (re-search-forward " *= +.*$" (line-end-position) t))

        (progn
          (re-search-forward " *= +.*$" (line-end-position) t)
          (goto-char (match-beginning 0))
          (push-mark)
          (end-of-line)
          (delete-region (point) (mark))
          (setq current-point (line-end-position))
          (goto-char current-point)
          (message "1!"))

      (progn
        (goto-char current-point)

        (cond
         ((looking-at "[^ \n]")
          (progn
            (setq current-point (line-end-position))
            (goto-char current-point)
            (fixup-whitespace)
            (message "2!")))

         ((looking-at "[[:space:]]+$")
          (progn
            (goto-char current-point)
            (end-of-line)
            (fixup-whitespace)
            (message "3!")))

         (t
          (progn
            (goto-char current-point)
            (end-of-line)
            (fixup-whitespace)
            (message "4!")))
         )))

    (when (looking-at " ")
      (delete-trailing-whitespace))

    ;; determine whether I've used 'j' to mark something for quick calculation
    ;; if I have, give me the output immediately following the calculation
    (cond

     ;; condition where I have inserted a 'j'
     ((save-excursion (re-search-backward "j" (line-beginning-position) t))
      (nssend-helper))

     ;; no "j" inserted before expression
     ((not (save-excursion (re-search-backward "j" (line-beginning-position) t)))
      (goto-char
       (car (avy--generic-jump "\(*[a-z0-9-]" nil (line-beginning-position) (line-end-position))))
      (insert "j")
      (end-of-line)
      (nssend-helper))

     (t
      (orelisp-eval)))))



(defun pmdr-timer (ptime min)
  "Function I use to track my time throughout the day.  This
creates a buffer to dump time stamp information into and messages
the timer information as well.  This function can be debugged by
commenting out the ibuffer line."
  (interactive "pProblem time\nsEnter Time (minutes): ")
  (let* ((curr-buff (current-buffer))
         ;; date-head denotes the header I create for each day
         (date-head (format-time-string "\/%m.%d.%Y\/"))
         ;; date line denotes a time string that
         ;; is inserted when this function is called
         ;; I'll search backward for the most recent
         ;; version of this to place time stamps sequentially
         (date-line (concat ", " (format-time-string "%m.%d.%Y")))
         (this-year (concat "* " (format-time-string "%Y") "\n")))
    ;; use 'cond' function to determine if I
    ;; set aside time for specific problems
    (cond
     ;; no problem time / prefix arg
     ((= (prefix-numeric-value ptime) 1)
      (setq ptime nil))
     ;; 'C-u' universal prefix arg = 10 minutes
     ((= (prefix-numeric-value ptime) 4)
      (setq ptime "10"))
     ;; some other value supplied for 'ptime'
     ((and
       ptime
       (not (= (prefix-numeric-value ptime) 4))
       (not (= (prefix-numeric-value ptime) 1)))
      (setq ptime (format "%s" ptime)))
     ;; else - no 'ptime'
     (t
      (setq ptime nil)))
    ;; use window-configuration-to-register to restore the
    ;; window setup when all the time information is taken care of
    (window-configuration-to-register #x7a)
    ;; call ibuffer to signal the timer going off
    ;; problem time
    (when ptime
      (run-at-time
       (concat ptime " min")
       nil 'ibuffer));; regular time
    (run-at-time (concat min " min") nil 'ibuffer)
    
    ;; set up a buffer to place time stamp information into;
    ;; if it doesn't exist, make it, then save it
    ;; then move into position
    (unless (bufferp (get-buffer "time_stamp.org"))
      (progn
        (find-file "~/emacs/time_stamp.org")
        (save-buffer)))
    (with-current-buffer (get-buffer "time_stamp.org")
      (goto-char (point-min))
      ;; when expression to insert the year as an
      ;; org-mode level 1 header if it's not there
      (when (not (re-search-forward (concat "* " (format-time-string "%Y") "\n") nil t))
        (re-search-backward "-\\*-" nil t)
        (end-of-line)
        (insert (concat "\n\n" this-year)))
      ;; when expression to insert the day's date HEADER if it's not there
      (when (not (re-search-forward (concat date-head "\n" "------------\n") nil t))
        (progn
          (goto-char (point-min))
          (re-search-forward this-year nil t)
          (insert (concat "\n------------\n" date-head "\n" "------------\n\n")))))
    
    ;; insert the timer information into R for easy visualization
    (when
        (not
         (bufferp (get-buffer "*R*")))
      (progn
	(R)
	(sit-for 3)))
    (with-current-buffer "*R*"
      (end-of-buffer)
      ;; the ESS equivalent of 'beginning-of-visual-line'
      ;; leaves the prompt in place
      (comint-bol)
      (kill-visual-line)
      (insert (concat "time = '" min "  min, " (format-time-string " %H:%M:%S %p,  %m.%d.%Y") "'"))
      (inferior-ess-send-input))
    ;; return to the buffer of interest,
    ;; then switch to 'time_stamp.org'
    ;; and get into position
    (switch-to-buffer "time_stamp.org")
    (end-of-buffer)(when (not (re-search-backward "\* Backups" nil t))
                     (progn
                       (end-of-buffer)
                       (beginning-of-line)
                       (insert "* Backups\n\n")))
    (re-search-backward "\* Backups" nil t)
    ;; cond function to either insert the date if it's not
    ;; there already or just insert the timer information if
    ;; the date header is there
    ;; start w/ searching for the date line,
    ;; indicating a previous call of a timer.
    ;; The new timer should go below this
    (cond
     ((re-search-backward date-line nil t)
      (progn
	(end-of-line)
	(insert
         (concat "\n" min " min. at " (format-time-string "%H:%M:%S %p, %m.%d.%Y")))));; if no date line is there (i.e., haven't called the func yet today),
     ;; we need to search backwards for the date header
     ((re-search-backward (concat (format-time-string "%m.%d.%Y") "/\n-+\n") nil t)
      (progn (re-search-forward (concat (format-time-string "%m.%d.%Y") "/\n-+\n") nil t)
             (insert (concat min " min. at " (format-time-string "%H:%M:%S %p, %m.%d.%Y"))))))
    ;; save the buffer so I don't have to save it when I call 'save-some-buffers'
    (set-buffer "time_stamp.org")
    (save-buffer);; set this variable for 'dt' function that tells me the time and most recent timer
    (setq most-recent-timer (concat (format-time-string "%H:%M:%S %p") ", " min " min."));; wrap up timer stuff by displaying timer information in the minibuffer and restoring the window configuration
    (message "%s min. at %s" min (format-time-string "%H:%M:%S %p %m.%d.%Y"))
    (jump-to-register #x7a)
    (switch-to-buffer curr-buff)
    ;; added so I don't have to think about this,
    ;; but still regularly save my scratch buffer
    (my-persistent-scratch-save)))


;; helper functions
(defun pmdr-40-10 (ptime)
  "Pomodoro timer with 40 minutes total, 10 minutes of problem solving."
  (interactive "p")
  (pmdr-timer (prefix-numeric-value ptime) "40"))

(defun pmdr-50-0 (ptime)
  "Pomodoro timer with 50 minutes total, no problem solving."
  (interactive "p")
  (pmdr-timer (prefix-numeric-value ptime) "50"))

(defun pmdr-35-0 (ptime)
  "Pomodoro timer with 50 minutes total, no problem solving."
  (interactive "p")
  (pmdr-timer (prefix-numeric-value ptime) "35"))

(defun pmdr-10-0 (ptime)
  "Pomodoro timer with 10 minutes total."
  (interactive "p")
  (run-at-time "10 minutes" nil 'pmdr-10-0-helper))

(defun pmdr-10-0-helper ()
  "Helper function for 'pmdr-10-0'."
  (interactive)
  (when (eq (current-buffer) (get-buffer "*Ibuffer*"))
    (set-buffer (previous-buffer)))
  (read-key "Time to get back to work.  Press any key to do so: "))

;; -----
;; r object sending function
(defun r-object-send (arg)
  "Send an object to the inferior R process to see what it looks like.  R functions for object 'x' information include:
   [1] x          - input just the object
   [2] head(x)    - get the first portion of the object
   [3] str(x)     - get the structure of the object
   [4] summary(x) - get summary statistic information on x
   [5] class(x)   - get class information for x
   [6] typeof(x)  - determine the type of an R object
   [7] is.y(x)    - input 'y' (e.g., 'vector') to test if x is that type of object
   [8] length(x)  - get the length of an object
   [9] names(x)   - get the names of an object

w/ prefix arg, read a string to be used for subsetting." 
  (interactive "P")
  ;; get in position to grab a variable name; use 'mark' as a boundary point
  (beginning-of-line)
  (forward-word)
  (lispy-mark-symbol)

  (let* ((object (buffer-substring-no-properties (mark) (point)))
         (current-point (point))
         (keys '("a" "h" "s" "d" "f" "j" "h" "k" "n"))
         (keys-again keys)
         (command '("" "head(" "str(" "summary(" "class(" "is." "typeof(" "length(" "names("))
         (choice '("\n"))
         (by "$")
         (subset))

    ;; use prefix argument to subset the object as desired
    ;; input what subsetting you want via 'read-string'
    (when (not (= (prefix-numeric-value arg) 1))
      (setq subset (read-string "subset (e.g., $1, [1], etc...)")))

    ;; set up choice 
    (dotimes (n (length keys) choice)
      (setq choice (cons (concat "\n                         " (nth n keys) ": " (nth n command)) choice)))

    ;; ask which choice I want, then act accordingly
    (let* ((my-choice (read-key-sequence (propertize
                                (mapconcat 'identity choice "")
                                'face 'mac-window-select-face)))
           ;; return the position in the list that matches my choice above
           ;; use this to concat from 'command'
           (number (seq-position keys-again my-choice)))


      ;; now send the desired input
      ;; do this via 'cond' to get the desired result
      (cond ((and
              (= number 0)
              (not subset))
             (ess-send-string (ess-get-process)
                           (concat (nth number command) object)
                           'nowait))

            ;; is.something() case
            ((and
              (= number 4)
              (not subset))
            (ess-send-string (ess-get-process)
                             (concat "is." (read-string "is. what? (e.g., vector): ") "(" object ")")
                             'nowait))

            ;; all others w/ no prefix arg
            ((not subset)
             (ess-send-string (ess-get-process)
                           (concat (nth number command) object ")")
                           'nowait))


            ;; is.something w/ subset
            ((and subset
                  (= number 4))
             (ess-send-string (ess-get-process)
                             (concat "is." (read-string "is. what? (e.g., vector): ") "(" object subset)
                             'nowait))

            ;; subset arg
            (subset
             (ess-send-string (ess-get-process)
                           (concat (nth number command) object subset)
                           'nowait)))
          (deactivate-mark)
	  (message ""))))


;; -----
;; function for formatting my writing 
(defun mac-writing-output ()
  "Format my writing for insertion into ms-word."
  (interactive)
(progn
  (get-buffer-create "*writing_output*")
  (kill-ring-save (point) (mark) t)
  (with-current-buffer "*writing_output*"
    (delete-region (point-min) (point-max))
    (yank)
    (goto-char (point-min))
    (while (re-search-forward "\\(.\\)\\(\n\\)" (point-max) t)
      (replace-match " " nil nil nil 2))
    (goto-char (point-min))
    (while (and
            (re-search-forward "\\. *\n" (point-max) t)
            (< (point) (point-max)))
        (insert "\n"))
    )
  (switch-to-buffer "*writing_output*")
  )
)


;; -----
;; re-builder toggle function
(defun re-builder-toggle ()
  "Display or remove the disaply of the re-builder based on context."
  (interactive)  
  ;; contingency if RE-Builder inactive
  (when (not (get-buffer "*RE-Builder*"))
    (progn
      (re-builder)
      (reb-change-target-buffer current-buffer)))

  ;; determine whether RE-Builder has a window or not
  (let ((status 0)
	(rb-window))
    (dolist (w (window-list))
      (with-current-buffer (window-buffer w)
	(when (eq major-mode 'reb-mode)
	  (setq status (1+ status)
		rb-window (get-buffer-window)))))

    (cond
     ;; if it has a window, delete the window
     ((> status 0)
      (delete-window rb-window))
     ;; if no window, give it a window
     (t
      (progn
	(select-window (car (window-at-side-list nil 'left)))
	(display-buffer
	 "*RE-Builder*"
	 '((display-buffer-reuse-window
	    display-buffer-below-selected)
	   (window-height . 5)))))))
  )


;; -----
;; 2023.12.02 - this makes the error
;; buffer for src blocks appear in 
;; the window you're working in as I prefer
(add-to-list 'display-buffer-alist
             '(".*Org-Babel Error Output.*"
               (display-buffer-reuse-window
                mac-window-config-4-help-display)))

(add-to-list 'display-buffer-alist
             '("*wclock*"
               (display-buffer-reuse-window
                mac-window-config-4-help-display)))
