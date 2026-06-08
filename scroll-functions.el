
;; functions for scrolling help functions in other windows

(declare-function eat-emacs-mode "eat")
(declare-function eat-semi-char-mode "eat")
(declare-function eat-term-display-cursor "eat" (terminal))
(defvar eat-terminal)

(defun mac-claude-eat-scroll (eat-buf val)
  "Scroll the claude-code/eat buffer EAT-BUF by VAL lines from another window.

Positive VAL moves toward newer output, negative toward the conversation
history.  This mimics the R/python/man/help scrolling helpers, but works
around eat's terminal quirks:

* Browsing is done in eat's read-only (\"emacs\") mode.  In that mode
  `claude-code--eat-synchronize-scroll' leaves point alone, so Claude's
  constant terminal redraws no longer snap the view back to the prompt or
  \"fold\" away the scrollback while you read.

* When you scroll back down far enough that the live prompt is on screen,
  the buffer is returned to interactive mode and pinned to the terminal
  cursor, so you can type at Claude again and follow new output -- exactly
  like scrolling to the bottom of a shell/comint buffer."
  (let ((win (get-buffer-window eat-buf t)))
    (when (window-live-p win)
      (with-selected-window win
        ;; Freeze the view so scrolling back actually sticks.
        (unless buffer-read-only
          (eat-emacs-mode))
        (let ((scroll-error-top-bottom t)
              (scroll-preserve-screen-position t))
          (condition-case nil
              (if (< val 0)
                  (scroll-down-command (- val))
                (scroll-up-command val))
            ;; quietly clamp at the ends instead of signalling
            ((beginning-of-buffer end-of-buffer) nil)))
        ;; Caught up to the live prompt -> resume interactive following.
        (when (and (>= val 0)
                   (bound-and-true-p eat-terminal)
                   (pos-visible-in-window-p (point-max) win))
          (eat-semi-char-mode)
          (goto-char (eat-term-display-cursor eat-terminal))
          (set-window-point win (eat-term-display-cursor eat-terminal)))))))

(defun mac-pdf-man-R-or-help-scroll (val)
  "Function to move to the previous page of a pdf or scroll an R or man page buffer when working in a separate, non-pdf buffer."
  (interactive)

  ;; set up variables
  (let* ((current-buffer (current-buffer))
         (current-point (point))
         (window-list (window-list))
         (pdf)
         (man)
         (eat-buffer)
         (R-buffer)
         (help-page)
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
         (eat-scroll-fun
          (lambda (eat-buf)
	    (mac-claude-eat-scroll eat-buf val))))

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
      (when (eq (buffer-local-value 'major-mode (window-buffer buffer))
                'eat-mode)
        (setq eat-buffer (window-buffer buffer)))
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
   ;; have a claude code window via 'eat'
   ((and eat-buffer
         (not pdf)
         (not man)
         (not R-buffer)
         (not help-page))
    (funcall eat-scroll-fun eat-buffer))
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


;; -----
;; eat scroll functions written by claude code
(defun eat-scroll--lines (n)
  "Scroll the selected window by N lines (negative = down/back in history)."
  (unless (derived-mode-p 'eat-mode)
    (user-error "Not in an eat buffer"))
  (let ((eat--synchronize-scroll-function #'ignore))
    ;; Move point so we're not pinned to the terminal cursor.
    (let ((target (save-excursion
                    (forward-line n)
                    (point))))
      (goto-char target)
      (set-window-point (selected-window) target))
    ;; Now do a normal window scroll.
    (let ((scroll-preserve-screen-position t))
      (if (< n 0)
          (scroll-down (- n))
        (scroll-up n)))))

(defun eat-scroll-up (&optional arg)
  "Scroll the eat terminal window up (toward newer output).
ARG is the number of lines (default: a screenful minus `next-screen-context-lines')."
  (interactive "P")
  (eat-scroll--lines
   (cond ((null arg) (max 1 (- (window-text-height)
                               next-screen-context-lines)))
         ((eq arg '-) (- (max 1 (- (window-text-height)
                                   next-screen-context-lines))))
         (t (prefix-numeric-value arg)))))

(defun eat-scroll-down (&optional arg)
  "Scroll the eat terminal window down (toward older scrollback).
ARG is the number of lines (default: a screenful minus `next-screen-context-lines')."
  (interactive "P")
  (eat-scroll--lines
   (cond ((null arg) (- (max 1 (- (window-text-height)
                                  next-screen-context-lines))))
         ((eq arg '-) (max 1 (- (window-text-height)
                                next-screen-context-lines)))
         (t (- (prefix-numeric-value arg))))))

(defun eat-scroll-line-up (&optional n)
  "Scroll the eat window up by N lines (default 1)."
  (interactive "p")
  (eat-scroll--lines (or n 1)))

(defun eat-scroll-line-down (&optional n)
  "Scroll the eat window down by N lines (default 1)."
  (interactive "p")
  (eat-scroll--lines (- (or n 1))))

(defun eat-scroll-to-bottom ()
  "Jump back to the terminal cursor (live view) and recenter at bottom."
  (interactive)
  (unless (derived-mode-p 'eat-mode)
    (user-error "Not in an eat buffer"))
  (when eat-terminal
    (goto-char (eat-term-display-cursor eat-terminal))
    (set-window-point (selected-window)
                      (eat-term-display-cursor eat-terminal))
    (recenter -1)))

;; ---------------------------------------------------------------------------
;; Recenter-at-bottom-after-input
;;
;; The single chokepoint for "user sent something to the terminal" is
;; `eat-self-input' (used by semi-char and char modes, and the eshell
;; integration's input maps). We advise it with :after so the cursor's
;; window gets recentered to the bottom after every keystroke that
;; actually goes to the process.
;; ---------------------------------------------------------------------------

(defcustom eat-recenter-after-input-margin -1
  "Argument passed to `recenter' after sending input to an eat terminal.
-1 means \"last line of the window\".  See `recenter'."
  :type 'integer
  :group 'eat-ui)

(defun eat--recenter-after-input (&rest _)
  "Recenter point at the bottom of the window in eat buffers after input."
  (when (and eat-terminal
             (or (derived-mode-p 'eat-mode)
                 (bound-and-true-p eat--eshell-local-mode)))
    (let ((win (get-buffer-window (current-buffer))))
      (when (window-live-p win)
        (with-selected-window win
          ;; Make sure we're actually on the live cursor before recentering;
          ;; otherwise we'd "recenter" a stale scrollback position.
          (goto-char (eat-term-display-cursor eat-terminal))
          (set-window-point win (eat-term-display-cursor eat-terminal))
          (recenter eat-recenter-after-input-margin))))))

(define-minor-mode eat-recenter-on-input-mode
  "Global minor mode: recenter eat windows at bottom after every input keystroke."
  :global t
  :group 'eat-ui
  (if eat-recenter-on-input-mode
      (advice-add 'eat-self-input :after #'eat--recenter-after-input)
    (advice-remove 'eat-self-input #'eat--recenter-after-input)))


;; ---------------------------------------------------------------------------
;; Stop the claude-code / eat buffer from "folding" when scrolling back.
;;
;; (setq eat-enable-alternative-display nil) alone is not enough.  Three
;; things have to be true for scrollback to behave like a normal buffer:
;;
;;   1. No alternative-display screen wipes  -> eat-enable-alternative-display
;;   2. Unlimited scrollback (don't delete old conversation) ->
;;      eat-term-scrollback-size / claude-code-eat-never-truncate-claude-buffer
;;   3. Browse in read-only mode so claude-code--eat-synchronize-scroll does
;;      not yank you back to the prompt on every redraw.  `mac-claude-eat-scroll'
;;      handles #3 automatically; the settings below cover #1 and #2.
;; ---------------------------------------------------------------------------

(with-eval-after-load 'eat
  ;; #1: keep everything on the main display so it lands in scrollback.
  (setq eat-enable-alternative-display nil)
  ;; #2 (global default): unlimited scrollback so old turns aren't truncated.
  (setq eat-term-scrollback-size nil))

(with-eval-after-load 'claude-code
  ;; #2 (claude-code's own knob): never truncate the Claude buffer.
  (setq claude-code-eat-never-truncate-claude-buffer t))


;; ---------------------------------------------------------------------------
;; Keep M-t / M-SPC scrolling the eat buffer instead of leaking to Claude.
;;
;; `eat-semi-char-mode' (the mode claude-code keeps the buffer in so you can
;; type at Claude) binds *every* M-<ascii> chord -- including M-t and M-SPC --
;; to `eat-self-input', which forwards the raw "ESC t" / "ESC SPC" to the
;; terminal.  Claude Code's TUI reads "ESC t" as its toggle-thinking-mode
;; shortcut, so once point is in the claude buffer a repeated M-t pops that
;; menu instead of running `mac-pdf-man-help-or-R-prev-page'.
;;
;; Two things have to be true to stop this, because of how eat builds the map:
;;
;;   A. The keys must be excluded from the semi-char keymap eat *builds*.  Eat
;;      exposes `eat-semi-char-non-bound-keys' for exactly this; keys listed
;;      there are left unbound and fall through to the global map (where they
;;      are bound to the scroll commands below).  Exceptions must be written in
;;      "ESC KEY" form, not "M-KEY" form (see `eat-term-make-keymap').
;;
;;   B. The keymap object that is *already live* in every claude buffer must be
;;      patched too.  `eat--semi-char-mode' is a `define-minor-mode'; it binds
;;      the *object* that `eat-semi-char-mode-map' held at definition time and
;;      keeps using it forever -- reassigning the variable later (which is all
;;      `eat-update-semi-char-mode-map' does) never reaches an open, or even a
;;      newly opened, buffer.  So we must `define-key' the actual live object,
;;      which we fetch from `minor-mode-map-alist', binding the chords straight
;;      to the scroll commands.  (We deliberately do NOT call
;;      `eat-update-semi-char-mode-map' here -- it would only swap the variable
;;      for an orphan keymap and leave the live object untouched.)
;;
;; Either route lands on the same `mac-pdf-man-help-or-R-*' command, so it does
;; not matter which keymap object happens to be active.
;; ---------------------------------------------------------------------------
(with-eval-after-load 'eat
  ;; (A) exclude the chords from any semi-char keymap eat builds from now on,
  ;; so they fall through to the global bindings instead of the terminal.
  (dolist (key '([?\e ?t] [?\e ?\s]))
    (unless (member key eat-semi-char-non-bound-keys)
      (push key eat-semi-char-non-bound-keys)))
  ;; (B) patch the keymap object that live claude buffers actually use.
  (let ((live-map (or (cdr (assq 'eat--semi-char-mode minor-mode-map-alist))
                      (and (boundp 'eat-semi-char-mode-map)
                           eat-semi-char-mode-map))))
    (when (keymapp live-map)
      (define-key live-map [?\e ?t]  #'mac-pdf-man-help-or-R-prev-page)
      (define-key live-map [?\e ?\s] #'mac-pdf-man-help-or-R-next-page))))

