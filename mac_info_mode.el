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

(defun mac-non-info-other-window-helper ()
  "Helper function for 'mac-info-other-window'.  For using info mode in a separate window"
  (let ((current-window (selected-window))
	(right-window
	 (car (window-at-side-list nil 'right))))
  (with-current-buffer
      (window-buffer right-window)
    (let* ((key (read-key-sequence "input: "))
           (fun (key-binding key)))
      (select-window right-window)
      (unless (not (commandp fun))
	(command-execute fun))
      (select-window current-window)
      )))
  )

(defun mac-prefix-arg-info-other-window-helper ()
  "Function for repeatedly entering complex commands in info-mode buffers."
  (with-current-buffer "*info*"
    (let* ((key (read-key-sequence "input: "))
           (fun (lookup-key Info-mode-map key)))
      (while (and
              (commandp fun)
              (not (string= " " key)))
        (progn 
          (command-execute fun))
        (set-window-point
         (get-buffer-window)
         (point-min))
        (setq key (read-key-sequence "input: ")
              fun (lookup-key Info-mode-map key)))
      )))

(defun mac-prefix-arg-non-info-other-window-helper ()
  "Function for repeatedly entering complex commands in info-mode buffers."
  (let ((current-window (selected-window)))
    (select-window (car (window-at-side-list nil 'right))) 
    (let* ((current-window (selected-window))
	   (right-window
	    (car (window-at-side-list nil 'right)))
	   (key (read-key-sequence "input: "))
           (fun (key-binding key)))
      (while (and
              (commandp fun)
              (not (string= " " key)))
        (progn 
	  (select-window right-window)
	  (unless (not (commandp fun))
	    (command-execute fun))
	  (setq key (read-key-sequence "input (cancel w/ SPC): ")
		fun (key-binding key)))
	))
    (select-window current-window)
    ))
  

(defun mac-info-other-window-master (arg)
  "Execute a command in the info mode buffer when it exists."
  (interactive "p")
  (let ((win-list (window-list))
        (count 0))
    ;; determine if there's an info mode buffer 
    (dolist (win win-list)
      (with-current-buffer
	  (window-buffer win)
	(when (eq 'Info-mode major-mode)
        (setq count (1+ count)))))
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
     
     ;; no prefix arg and no info buffer
     ((and
       (= count 0)
       (= 1 (prefix-numeric-value arg)))
      (mac-non-info-other-window-helper))

     ;; prefix arg and no info mode buffer
     ((and
       (= count 0)
       (not (= 1 (prefix-numeric-value arg))))
      (mac-prefix-arg-non-info-other-window-helper))
     )
    )) 

(define-key mc-r-map (kbd "C-\\") 'mac-info-other-window-master)


