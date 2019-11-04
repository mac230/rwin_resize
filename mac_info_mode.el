;; my functions for using info-mode in one window, while taking notes in another window

(defun mac-info-other-window-helper ()
  "Helper function for 'mac-info-other-window'.  For using info mode in a separate window"
  (with-current-buffer "*info*"
    (let* ((key (read-key-sequence "input: "))
           (fun (lookup-key Info-mode-map key)))
      (unless (not (commandp fun))
        (command-execute fun))
      (set-window-point
       (get-buffer-window)
       (point-min))
       )))
  

(defun mac-prefix-arg-info-other-window-helper ()
  "Function for repeatedly entering complex commands in info-mode buffers."
  (with-current-buffer "*info*"
    (let* ((key (read-key-sequence "input: "))
           (fun (lookup-key Info-mode-map key)))
      (while (commandp fun)
        (progn 
          (command-execute fun))
        (set-window-point
         (get-buffer-window)
         (point-min))
        (setq key (read-key-sequence "input: ")
              fun (lookup-key Info-mode-map key)))
      )))
  

(defun mac-info-other-window (arg)
  "Execute a command in the info mode buffer when it exists."
  (interactive "p")
  (let ((win-list (window-list))
        (count 0))
    ;; determine if there's an info mode buffer 
    (dolist
        (win win-list)
      (when (eq 'Info-mode
                (with-current-buffer
                    (window-buffer win)
                  major-mode))
        (setq count (1+ count))))
    (cond
     ;; prefix arg and info mode buffer
     ((and
      (> count 0)
      (not (= 1 (prefix-numeric-value arg))))
      (mac-prefix-arg-info-other-window-helper))
          
     ;; no prefix arg and info mode buffer
     ((and
       (> count 0)
       (= 1 (prefix-numeric-value arg)))
      (mac-info-other-window-helper))
     
     ;; else clause; no info mode buffer - dummy
     (t 
      (message "No Info-mode buffer in window."))
     )))



(define-key mc-r-map (kbd "C-\\") 'mac-info-other-window)
