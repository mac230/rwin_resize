;; various functions I wrote

(defvar non-blank-line-regex "\\(^[^[:space:]\n]+\\)"
  "regex for a non-blank line; used to demarcate list boundaries")

(defvar list-regex "^ + - \\[[[:digit:]x]*\\] \\(.*:: \\)\\{0,1\\}"
  "regex for a list element; used to mark where sublists should go")

(defvar todo-regex "^ + - \\[[x]*\\]\\(.*:: \\)\\{0,1\\}"
  "regex for a todo list element; used to mark where sublists should go")

(defvar sublist-column-regex "^ +\\+"
  "regex used to determine the column to insert the next '+' char")

(defvar sublist-regex "\\(\n\\)\\(^ +\\+.*\\)"
  "regex for a sublist element; used to mark where sublists should go")


(defun list-line-adder ()
  "Add newlines between lines of my lists where needed."
  (interactive)
  (let* ((reverse-bound
          (save-excursion
            (re-search-backward non-blank-line-regex nil t)))
         (forward-bound
          (save-excursion
            (re-search-forward non-blank-line-regex nil t)))
         (next-elt))
 
  (when
      (not reverse-bound)
    (setq reverse-bound (point-min)))
  
  (when
      (not forward-bound)
    (setq forward-bound (point-max))) 

  (goto-char forward-bound)
  (setq next-elt (save-excursion (re-search-backward list-regex reverse-bound t 2)))

  ;; insert needed newlines
  (while (and next-elt
              (>= (point) reverse-bound))
    (progn
      (re-search-backward list-regex next-elt t)
      (when (not (re-search-backward "^\n" next-elt t))
        (open-line 1))
      (setq next-elt (save-excursion (re-search-backward list-regex reverse-bound t 2)))
      ))
  ))




(defun list-line-remover ()
  "Remove extraneous newlines from my lists." 
  (interactive)
  (let* ((list-elt-regex "\\(^    - \\[[[:digit:]x]*\\]\\).*\n\\(^ +\\+.*\n\\)*\\(^ .*\n\\)*")
         (reverse-bound
          (save-excursion
            (re-search-backward non-blank-line-regex nil t)))
         (forward-bound
          (save-excursion
            (re-search-forward non-blank-line-regex nil t)))
         (next-elt))
 
  (when
      (not reverse-bound)
    (setq reverse-bound (point-min)))
  
  (when
      (not forward-bound)
    (setq forward-bound (point-max))) 

  (goto-char forward-bound)
  (setq next-elt (save-excursion (re-search-backward list-elt-regex reverse-bound t 2)))

  ;; insert needed newlines
  (while (and next-elt
              (>= (point) reverse-bound))
    (progn
      (re-search-backward list-elt-regex next-elt t)
      (when (re-search-backward "\\(^ *\n\\)\\{2,\\}" next-elt t)
        (delete-blank-lines))
      (setq next-elt (save-excursion (re-search-backward list-elt-regex reverse-bound t 2)))
      ))
  ))




;; -----
(defun list-line-renumber ()
  "Automatically re-number lists and return point to its present location."
  (interactive)
  (let* ((current-point (point))
         (list-n 1)
         (reverse-bound
          (save-excursion
            (re-search-backward non-blank-line-regex nil t)))
         (forward-bound
          (save-excursion
            (re-search-forward non-blank-line-regex nil t))))

    (when
        (not reverse-bound)
      (setq reverse-bound (point-min)))

    (when
        (not forward-bound)
      (setq forward-bound (point-max))) 
    
    (goto-char reverse-bound)
    (unless (eobp)
      (forward-char 1))

    (while
        (<= (point) (1- forward-bound))

    (if
      (re-search-forward "^    - \\[\\([[:digit:]]+\\)\\]" forward-bound t)

        (progn
          (goto-char (match-beginning 1))
          (re-search-forward "[[:digit:]]+" nil t)
          (replace-match (format "%s" list-n))
          (end-of-line)
          (setq list-n (1+ list-n)))
      
      (if (eobp)
          (progn
            (end-of-line)
            (insert "\n"))
        (forward-char 1)))
    )

    (goto-char current-point)
  ))




;; -----
(defun list-single-spacer ()
  "Create a single-spaced list."
  (interactive)
  (let ((reverse-bound
        (save-excursion
          (re-search-backward non-blank-regex nil t)))
       (forward-bound
        (save-excursion
          (re-search-forward non-blank-regex nil t)))
       (blank-line-regex "^ *\n")
       (first-list-elt))
    
    (when (not forward-bound)
      (setq forward-bound (point-max)))

    (when (not reverse-bound)
      (setq reverse-bound (point-min)))

    (goto-char reverse-bound)
    (unless
        (not (re-search-forward list-regex))
      (while
          (and
           (or
            (save-excursion (re-search-forward list-regex forward-bound t))
            (save-excursion (re-search-forward sublist-regex forward-bound t)))
           (re-search-forward blank-line-regex forward-bound t))
      (progn
        (goto-char (match-beginning 0))
        (delete-blank-lines)))
      )))




;; -----
(defun mac-org-numbered-list ()
"Function to insert numbered lists in my preferred format of \'- [1] first point.\'"
(interactive)
(end-of-line)
(let* ((current-point (point))
       (current-number 1)
       (reverse-bound
        (save-excursion
          (re-search-backward non-blank-line-regex nil t)))
       (forward-bound
        (save-excursion
          (re-search-forward non-blank-line-regex nil t)))
       (next-list-elt
        (save-excursion
          (re-search-forward list-regex forward-bound t)))
       (prev-list-elt
        (save-excursion
          (re-search-backward list-regex reverse-bound t))))
      
  ;; contingency for when you start a list at the beginning/end of a buffer
  (when
      (not reverse-bound)
    (setq reverse-bound (point-min)))
  (when
      (not forward-bound)
    (progn
      (end-of-buffer)
      (open-line 1)
      (setq forward-bound (point-max))))

  (when prev-list-elt
    (progn
      (goto-char prev-list-elt)
      (re-search-forward "[[:digit:]]" (line-end-position) t)
      (goto-char (match-beginning 0))
      (setq current-number (1+ (number-at-point)))
      (goto-char current-point)))

  (when next-list-elt
    (progn
      (goto-char next-list-elt)
      (beginning-of-line)
      (open-line 1)))

  (when (not next-list-elt)
    (re-search-forward "\\(^ +\\+.*\n\\)+\\(^ .*\n\\)*" forward-bound t))
  
  (insert (concat "\n    - [" (format "%s" current-number) "] "))
  (list-line-renumber)
  (re-search-backward "^    - " (line-beginning-position) t)
  (re-search-forward "\\[")
  (setq current-number (format "%s" (number-at-point)))
  (list-line-adder)
  (list-line-remover)
  (setq forward-bound
        (re-search-forward non-blank-regex nil t))
  (when (not forward-bound)
    (setq forward-bound (end-of-buffer)))
  (re-search-backward (concat "^    - \\["  current-number) reverse-bound)
  (end-of-line)
  (message current-number)
  ))





;; -----
(defun mac-org-todo-list ()
"Insert to-do lists in my preferred format."
(interactive)
(end-of-line)
(let* ((current-point (point))
       (reverse-bound
        (save-excursion
          (re-search-backward non-blank-line-regex nil t)))
       (forward-bound
        (save-excursion
          (re-search-forward non-blank-line-regex nil t)))
       (next-list-elt
        (save-excursion
          (re-search-forward todo-regex forward-bound t)))
       (prev-list-elt
        (save-excursion
          (re-search-backward todo-regex reverse-bound t))))

  ;; contingency for when you start a list at the beginning/end of a buffer
  (when
      (not reverse-bound)
    (setq reverse-bound (point-min)))

  (when
      (not forward-bound)
      (setq forward-bound (point-max)))

  (when next-list-elt
    (progn
      (goto-char next-list-elt)
      (beginning-of-line)
      (open-line 1)))

  (when (not next-list-elt)
    ;; regex to get through sublists
    (re-search-forward "\\(^ +\\+.*\n\\)+\\(^ .*\n\\)*" forward-bound t))

  (insert (concat "\n    - []~~ \n"))
  (list-line-adder)
  (list-line-remover)
  (end-of-buffer)
  (re-search-backward "~~" reverse-bound t)
  (replace-match " ")
  ))





;; -----
(defun mac-org-numbered-or-todo ()
  "Insert a todo or numbered list item depending on context."
  (interactive)
  (let* ((current-point (point))
         (reverse-bound
          (save-excursion
            (re-search-backward non-blank-line-regex nil t)))
         (forward-bound
          (save-excursion
            (re-search-forward non-blank-line-regex nil t)))
         (next-list-elt
          (save-excursion
            (re-search-forward todo-regex forward-bound t)))
         (prev-list-elt
          (save-excursion
            (re-search-backward todo-regex reverse-bound t)))) 

    (when
        (not reverse-bound)
      (setq reverse-bound (point-min)))
    (when
        (not forward-bound)
      (setq forward-bound (point-max)))

    (cond
     ((or
       (save-excursion
         (re-search-forward todo-regex forward-bound t))
       (save-excursion
         (re-search-backward todo-regex reverse-bound t)))
      (mac-org-todo-list))

     (t
      (mac-org-numbered-list)))))




;; -----
(defun l1-sublist ()
  "Helper function 'mac-sublist-indent'.  
Simplest use scenario for when you call this while on a line w/ a list elt."
  (let ((cp (point))
        (curr-col))
    (when
        (save-excursion
          (beginning-of-line)
          (re-search-forward list-regex (line-end-position) t))
      (progn
        (beginning-of-line)
        (re-search-forward list-regex (line-end-position) t)
        (setq curr-col (current-column))))
    (if curr-col
      (progn
        (end-of-line)
        (insert (concat "\n" (make-string curr-col #x20) "+ ")))
      (goto-char cp))))





;; -----
(defun l2-sublist ()
  "Helper function for 'mac-sublist-indent'.  
For when calling on a sublist line w/ no wrapped text following."
  (let* ((cp (point))
         (curr-col)
         (forward-bound
          (save-excursion
            (re-search-forward non-blank-line-regex nil t)))
         (list-bound))

    (when (not forward-bound)
      (setq forward-bound (point-max)))

    (setq list-bound (save-excursion (re-search-forward list-regex forward-bound t)))
    (when (not list-bound)
      (setq list-bound forward-bound))

    (when
        (save-excursion
          (beginning-of-line)
          (re-search-forward sublist-column-regex (line-end-position) t))
      (progn
        (beginning-of-line)
        (re-search-forward sublist-column-regex (line-end-position) t)
        (setq curr-col (1- (current-column)))))
    (if (and
         curr-col
         (save-excursion (re-search-forward sublist-regex list-bound t)))
        (progn
          (re-search-forward sublist-regex)
          (goto-char (match-beginning 0))
          (insert (concat "\n" (make-string curr-col #x20) "+ ")))
      (goto-char cp))))





;; -----
(defun l3-sublist ()
  "Helper function for 'mac-sublist-indent'.  
For when calling at the end of a wrapped text sublist line w/ sublists."
  (let ((cp (point))
        (forward-bound
         (save-excursion
           (re-search-forward list-regex nil t)))
        (reverse-bound
         (save-excursion
           (re-search-backward list-regex nil t))))

    (when (not forward-bound)
      (setq forward-bound
            (save-excursion
              (re-search-forward non-blank-line-regex nil t))))
    (when (not forward-bound)
      (setq forward-bound (point-max)))

    (when (not reverse-bound)
      (setq reverse-bound
            (save-excursion
              (re-search-backward non-blank-line-regex nil t))))
    (when (not reverse-bound)
      (setq reverse-bound (point-max)))

  (let ((curr-col)
        (next-sublist)
        (next-list-elt)
        (sublist-point))

    (when
        (save-excursion
          (re-search-forward "\n +\\+" forward-bound t))
      (progn 
        (setq next-sublist (save-excursion
                             (re-search-forward "\n +\\+.*$" forward-bound t)
                             (point)))))

    (cond
     (next-sublist
      (progn
        (setq curr-col
              (save-excursion
                (re-search-forward sublist-column-regex next-sublist)
                (1- (current-column))))
        (re-search-forward "\n +\\+.*$" forward-bound t)
        (goto-char (match-beginning 0))
        (insert (concat "\n" (make-string curr-col #x20) "+ "))))
     
     ((looking-at (concat "\n" (substring list-regex 1)))
      (progn
        (setq curr-col
              (save-excursion
                (re-search-backward sublist-column-regex reverse-bound t)
                (goto-char (match-end 0))
                (1- (current-column))))
        (unless (not curr-col)
          (insert (concat "\n" (make-string curr-col #x20) "+ ")))))
     
     (t
      (goto-char cp)))
        )))





;; -----
(defun l4-sublist ()
  "Helper function for 'mac-sublist-indent'.  
Inserts sub-lists at the end of sublists."
  (let* ((reverse-bound
          (save-excursion
            (re-search-backward non-blank-line-regex nil t)))
         (forward-bound
          (save-excursion
            (re-search-forward non-blank-line-regex nil t)))
         (next-list-elt)
         (prev-list-elt)
         (line-end (line-end-position))
         (curr-col))

    (when (not reverse-bound)
      (setq reverse-bound (point-min)))
    (when (not forward-bound)
      (setq forward-bound (point-max)))

    (setq next-list-elt
          (save-excursion
           (re-search-forward list-regex forward-bound t)))
    (when (not next-list-elt)
      (setq next-list-elt forward-bound))

    (setq prev-list-elt
          (save-excursion
            (re-search-backward list-regex reverse-bound t)))
    (unless (not prev-list-elt)
      (goto-char prev-list-elt))
    (beginning-of-line)
    (re-search-forward list-regex (line-end-position) t)
    (setq curr-col (current-column))
    (while
        (save-excursion
          (re-search-forward "\\+" line-end t))
      (re-search-forward "\\+" line-end t))

    ;; look for a blank line to insert after
    (if (save-excursion
          (re-search-forward "\\(\n[[:space:]]*\\)\n" next-list-elt t))
      (progn
        (re-search-forward "\\(\n[[:space:]]*\\)\n" next-list-elt t)
        (goto-char (match-beginning 0)))
      (end-of-line))
    (insert (concat "\n" (make-string curr-col #x20) "+ "))
    (when (not (looking-at "\\(\n[[:space:]]*\\)\n"))
      (open-line 1))
    ))




;; -----
(defun mac-sublist-indent ()
  "Insert a sublist at point using my preferred format.  
Uses helper functions together w/ a cond function to decide 
how to insert the sublist."
  (interactive)
  (end-of-line)
  
  (cond
   ((looking-back (concat list-regex ".*") (line-beginning-position))
    (progn
      (l1-sublist)
      (message "l1")))

   ((and
     (looking-at (concat sublist-regex ".*"))
     (looking-back (concat sublist-regex ".*") (line-beginning-position)))
    (progn
      (l2-sublist)
      (message "l2")))

   ((looking-at "\\(\n +.*\\)+ +\\+")
    (progn
      (l3-sublist)
      (message "l3")))

   ((save-excursion
      (re-search-backward "\\(^ +\\+\\)*" nil t))
    (progn
      (l4-sublist)
      (message "l4")))
   
   (t
    (message "not in a list, dummy!"))
   )
  )

(global-set-key (kbd "C-c M-RET") 'mac-org-numbered-or-todo)
(global-set-key (kbd "C-c C-j")   'mac-sublist-indent)
