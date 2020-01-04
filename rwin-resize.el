;; my functions for setting up my emacs environment for working with R,
;; python, elisp, shell code using emacs as my ide.  sets up a
;; REPL environment for each of the above languages

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

(key-chord-define-global "jq" 'rwin-resize)


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


(defun send-line-R-python-shell-ielm ()
  "Send the current line to R, python, or shell based on context."
  (interactive)
  ;; cond function determines which setup we're using
  (cond
   ((or (eq major-mode 'ess-mode)
        (eq (get-buffer "*R*") (window-buffer (car (window-at-side-list nil 'bottom))))
        (eq (get-buffer "*R*") (window-buffer (car (window-at-side-list nil 'right)))))
    (ess-eval-line))

   ((or (eq major-mode 'python-mode)
        (eq (get-buffer "*Python*") (window-buffer (car (window-at-side-list nil 'bottom))))
        (eq (get-buffer "*Python*") (window-buffer (car (window-at-side-list nil 'right)))))
    (comint-send-line (process-buffer (get-process "Python"))))

   ((or (eq major-mode 'shell-mode)
        (eq (get-buffer "*shell*") (window-buffer (car (window-at-side-list nil 'bottom))))
        (eq (get-buffer "*shell*") (window-buffer (car (window-at-side-list nil 'right)))))
    (progn
      (comint-send-line (process-buffer (get-process "shell")))
      (shell-buffer-update-dir-fun)))

   ((or (eq major-mode 'lisp-interaction-mode)
        (eq major-mode 'emacs-lisp-mode)
        (eq (get-buffer "*ielm*") (window-buffer (car (window-at-side-list nil 'bottom))))
        (eq (get-buffer "*ielm*") (window-buffer (car (window-at-side-list nil 'right)))))
    (progn
      (ielm-send-line (process-buffer (get-process "ielm")))
   ))))


(defun r-python-shell-or-ielm-send-region ()
  "Send the current region to the R or python process depending on context."
  (interactive)
  (cond

   ;; ess
   ((or (eq major-mode 'ess-mode)
        (eq (get-buffer "*R*") (window-buffer (car (window-at-side-list nil 'bottom))))
        (eq (get-buffer "*R*") (window-buffer (car (window-at-side-list nil 'right)))))
    (ess-eval-region-or-function-or-paragraph nil))

   ;; python
   ((or (eq major-mode 'python-mode)
        (eq (get-buffer "*Python*") (window-buffer (car (window-at-side-list nil 'bottom))))
        (eq (get-buffer "*Python*") (window-buffer (car (window-at-side-list nil 'right)))))
    (elpy-shell-send-statement))

   ;; shell
   ((or (eq major-mode 'shell-mode)
        (eq (get-buffer "*shell*") (window-buffer (car (window-at-side-list nil 'bottom))))
        (eq (get-buffer "*shell*") (window-buffer (car (window-at-side-list nil 'right)))))
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
   ((or (eq major-mode 'lisp-interaction-mode)
        (eq major-mode 'emacs-lisp-mode)
        (eq (get-buffer "*ielm*") (window-buffer (car (window-at-side-list nil 'bottom))))
        (eq (get-buffer "*ielm*") (window-buffer (car (window-at-side-list nil 'right)))))
    (let ((ielm-input (progn (mark-sexp -1)
                             (buffer-substring (point) (mark)))))
      (with-current-buffer "*ielm*"
        (when (not (eobp))
          (end-of-buffer))
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
         (rwin-arg nil)
         (bottom-buffer (buffer-name (window-buffer (car (window-at-side-list nil 'bottom)))))
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
  ;; conditionally call rwin-resize if we're using a process buffer 
  (when (> pfix-arg 0)
    (rwin-resize pfix-arg))
  ))




(defun mac-ess-mark-statement ()
  "Mark a statement in R code so that the function name, not just the text bounded by parentheses, is grabbed."
  (interactive)
  (cond

   ;; at the right parenthesis (statement end)
   ((looking-back "\)" (- (point) 1) nil)
    (progn
      (set-mark (point))
      (backward-sexp 2)))

   ;; at the left parenthesis (statement beginning)
   ((looking-at "\(")
    (progn
      (forward-sexp)
      (set-mark (point))
      (backward-sexp 2)))

   ;; in the function name
   ((looking-at "[a-zA-Z0-9._]+(")
    (progn
      (re-search-forward "\(" nil t 1)
      (goto-char (match-beginning 0))
      (forward-sexp)
      (set-mark (point))
      (backward-sexp 2)))

   ;; inside the parens
   (t
    (progn
      (re-search-backward "^[^=]+\\([a-zA-Z0-9._]\\)+\\((\\)" nil nil 1)
      (goto-char (match-beginning 2))
      (forward-sexp)
      (set-mark (point))
      (backward-sexp 2)))
))


;; -----
;; to make changes and have them take effect:
;; 1. eval this w/ 'C-M-x' (eval-defun)
;; 2. re-eval the minor-mode below
;; 3. dis-able then re-enable the minor mode
(defvar mc-r-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-\\")   'mac-info-other-window)
    (define-key map (kbd "C-c k") 'r-python-shell-or-ielm-send-region)
    (define-key map (kbd "C-j")   'send-line-R-python-shell-ielm)
    (define-key map (kbd "C-c l") 'r-object-send)
    (define-key map (kbd "C-c c") 'goto-line)
    (define-key map (kbd "C-w")   'kill-ring-save)
    (define-key map (kbd "M-w")   'kill-region)
    (define-key map (kbd "M-r")   'mac-ess-mark-statement)
    (define-key map (kbd "C-c u") 'parenth-insert)
    (define-key map (kbd "C-c r") 'copy-to-register)
    (define-key map (kbd "C-c b") 'curly-bracket-insert)
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
        (eq major-mode 'ess-mode)
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




(defun pmdr-timer (min)
  "Function I use to track my time throughout the day.  This creates a buffer to dump time stamp information into and messages the timer information as well. 
This function can be debugged by commenting out the ibuffer line."
  (interactive "sEnter Time (minutes):")
  (let* ((curr-buff (current-buffer))

         ;; date-head denotes the header I create for each day
         (date-head (format-time-string "\/%m.%d.%Y\/"))
         
         ;; date line denotes a time string that is inserted when this function is called
         ;; I'll search backward for the most recent version of this to place time stamps sequentially
         (date-line (concat ", " (format-time-string "%m.%d.%Y")))
         (this-year (concat "* " (format-time-string "%Y") "\n")))
         
    ;; use window-configuration-to-register to restore the window setup when all the time information is taken care of
    (window-configuration-to-register #x7a)

    ;; call ibuffer to signal the timer going off
    (run-at-time (concat min " min") nil 'ibuffer)
    
    ;; set up a buffer to place time stamp information into; if it doesn't exist, make it; save it, then move into position
    (unless (bufferp (get-buffer "time_stamp.org"))
      (progn
        (find-file "~/emacs/time_stamp.org")
        (save-buffer)))
    (switch-to-buffer "time_stamp.org")
    (beginning-of-buffer)

    ;; when expression to insert the year as an org-mode level 1 header if it's not there
    (when (not (re-search-forward (concat "* " (format-time-string "%Y") "\n") nil t))  
      (re-search-backward "-\\*-" nil t)
      (end-of-line)
      (insert (concat "\n\n" this-year)))

    ;; when expression to insert the day's date HEADER if it's not there
    (when (not (re-search-forward (concat date-head "\n" "------------\n") nil t))
      (progn
        (beginning-of-buffer)
        (re-search-forward this-year nil t)
        (insert (concat "\n------------\n" date-head "\n" "------------\n\n"))))
  
    ;; insert the timer information into R for easy visualization
    (when
        (not
         (bufferp (get-buffer "*R*")))
      (progn
	(R)
	(sit-for 3)))
    (with-current-buffer "*R*"
      (end-of-buffer)
      (comint-bol)       ;;The ESS equivalent of beginning-of-visual-line that leaves the prompt in place
      (kill-visual-line)
      (insert (concat "time = '" min "  min, " (format-time-string " %H:%M:%S %p,  %m.%d.%Y") "'"))
      (inferior-ess-send-input))

    ;; return to the buffer of interest, then switch to 'time_stamp.org' and get into position
    (switch-to-buffer "time_stamp.org")
    (end-of-buffer)
    (when (not (re-search-backward "\* Backups" nil t))
      (progn
        (end-of-buffer)
        (beginning-of-line)
        (insert "* Backups\n\n")))
    (re-search-backward "\* Backups" nil t)

    ;; cond function to either insert the date if it's not there already or just insert the timer information if the date header is there
    ;; start w/ searching for the date line, indicating a previous call of a timer.  The new timer should go below this
    (cond
     ((re-search-backward date-line nil t)
      (progn (end-of-line)
             (insert (concat "\n" min " min. at " (format-time-string "%H:%M:%S %p, %m.%d.%Y")))))

     ;; if no date line is there (i.e., haven't called the func yet today), we need to search backwards for the date header
     ((re-search-backward (concat (format-time-string "%m.%d.%Y") "/\n-+\n") nil t)
      (progn (re-search-forward (concat (format-time-string "%m.%d.%Y") "/\n-+\n") nil t)
             (insert (concat min " min. at " (format-time-string "%H:%M:%S %p, %m.%d.%Y"))))))

    ;; save the buffer so I don't have to save it when I call 'save-some-buffers'
    (set-buffer "time_stamp.org")
    (save-buffer)

    ;; set this variable for 'dt' function that tells me the time and most recent timer
    (setq most-recent-timer (concat (format-time-string "%H:%M:%S %p") ", " min " min."))

    ;; wrap up timer stuff by displaying timer information in the minibuffer and restoring the window configuration
    (message "%s min. at %s" min (format-time-string "%H:%M:%S %p %m.%d.%Y"))
    (jump-to-register #x7a)
    (switch-to-buffer curr-buff)

    ;; added so I don't have to think about this, but still regularly save my scratch buffer
    (persistent-scratch-save)))


;; -----
(defun r-object-send (arg)
  "Send an object to the inferior R process to see what it looks like.  R functions for object 'x' information include:

   [1] x          - input just the object
   [2] str(x)     - get the structure of the object
   [3] summary(x) - get summary statistic information on x
   [4] class(x)   - get class information for x
   [5] typeof(x)  - determine the type of an R object
   [6] is.y(x)    - input 'y' (e.g., 'vector') to test if x is that type of object
   [7] length(x)  - get the length of an object

w/ prefix arg, read a string to be used for subsetting." 
  (interactive "P")
  ;; get in position to grab a variable name; use 'mark' as a boundary point
  (beginning-of-line)
  (forward-word)
  (lispy-mark-symbol)
  (let* ((object (buffer-substring-no-properties (mark) (point)))
         (current-point (point))
         (keys '("a" "s" "d" "f" "j" "h" "k"))
         (keys-again keys)
         (command '("" "str(" "summary(" "class(" "is." "typeof(" "length("))
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
                             (concat "is." (read-string "is. what? (e.g., vector): ") "(" object subset ")")
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
