;; regexps and functions for creating lists and todo items
;; in my preferred format


;; -----
(defvar non-blank-line-regex "\\(^[^[:space:]\n]+\\)"
  "regex for a non-blank line; used to demarcate list boundaries")

(defvar blank-line-regex "^ *\n"
  "regex for a blank line; used to remove blank lines or add as needed")

(defvar list-regex "^ + - \\[[[:digit:]x]*\\] *\\(.*:: \\)\\{0,1\\}"
  "regex for a list element; used to mark where sublists should go")

(defvar todo-regex "^ + - \\[[x]*\\]\\(.*:: \\)\\{0,1\\}"
  "regex for a todo list element; used to mark where sublists should go")

(defvar sublist-column-regex "^ +[+*]"
  "regex used to determine the column to insert the next '+' char")

(defvar sublist-regex "\\(\n\\)\\(^ +[+*].*\\)"
  "regex for a sublist element; used to mark where sublists should go")





;; -----
(defun fwd-bound ()
  "Set forward-bound for list/todo list builder functions.
Setting these boundaries helps w/ positioning and renumbering."
  (let ((fwd-bound (save-excursion
                     (re-search-forward non-blank-line-regex nil t))))
    (when (not fwd-bound)
      (setq fwd-bound (point-max)))
    fwd-bound))


(defun rev-bound ()
  "Set forward-bound for list/todo list builder functions.
Setting these boundaries helps w/ positioning and renumbering."
  (let ((rev-bound (save-excursion
                     (re-search-backward non-blank-line-regex nil t))))
    (when (not rev-bound)
      (setq rev-bound (point-min)))
    rev-bound))





;; -----
(defun list-line-adder ()
  "Add newlines between lines of my lists where needed."
  (interactive)
  (let* ((rev-bound (rev-bound))
         (fwd-bound (fwd-bound))
         (next-elt))

    ;; start at list end; when no blank line after a list item, add one
    (goto-char fwd-bound)
    (beginning-of-line)
    (when
        (not
         (re-search-backward
          blank-line-regex
          (save-excursion (previous-line 1) (line-beginning-position)) t))
        (open-line 1))

    ;; search backwards for list elements
    (setq next-elt
          (save-excursion
            (re-search-backward list-regex rev-bound t)))

  ;; insert needed newlines
    (while (and
            next-elt
            (>= (point) rev-bound))
    (progn
      (re-search-backward list-regex rev-bound t)
      (when
          (not
           (re-search-backward blank-line-regex
            (save-excursion (previous-line 1) (line-beginning-position)) t))
        (open-line 1))
      (setq next-elt (save-excursion (re-search-backward list-regex rev-bound t)))
      ))
  ))





;; -----
;; (list-elt-regex "\\(^    - \\[[[:digit:]x]*\\]\\).*\n\\(^ +\\+.*\n\\)*\\(^ .*\n\\)*")
(defun list-line-remover ()
  "Remove extraneous newlines from my lists."
  (interactive)
  (let* ((rev-bound (rev-bound))
         (fwd-bound (fwd-bound))
         (next-elt))

  (goto-char fwd-bound)
  (setq next-elt
        (re-search-backward list-regex rev-bound t))

  (when (not next-elt)
    (setq next-elt rev-bound))

  ;; insert needed newlines
  (while
      (and
       (save-excursion
         (re-search-backward blank-line-regex rev-bound t))
       (>= (point) rev-bound))

    (progn
      (re-search-backward blank-line-regex rev-bound t)
      (unless
          (looking-at
           (concat "^ *\n" (substring list-regex 1)))
        (delete-blank-lines))
      (unless
          (looking-at
           (concat "^ *\n" (substring list-regex 1)))
        (delete-blank-lines)))
    ))
  )





;; -----
(defun list-line-renumber ()
  "Automatically re-number lists and return point to its present location."
  (interactive)
  (let* ((current-point (point))
         (list-n 1)
         (rev-bound (rev-bound))
         (fwd-bound (fwd-bound)))

    (goto-char rev-bound)
    (unless (eobp)
      (forward-char 1))

    (while
        (<= (point) (1- fwd-bound))

      (if ;; different regex here, b/c it has to contain a number
          (re-search-forward "^    - \\[\\([[:digit:]]+\\)\\]" fwd-bound t)

        (progn
          (goto-char (match-beginning 1))
          (re-search-forward "[[:digit:]]+" fwd-bound t)
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
  (let ((rev-bound (rev-bound))
        (fwd-bound (fwd-bound))
        (first-list-elt))

    (goto-char rev-bound)
    (unless
        (not (re-search-forward list-regex fwd-bound t))
      (while
          (and
           (or
            (save-excursion (re-search-forward list-regex fwd-bound t))
            (save-excursion (re-search-forward sublist-regex fwd-bound t)))
           (re-search-forward blank-line-regex fwd-bound t))
      (progn
        (goto-char (match-beginning 0))
        (delete-blank-lines)))
      )
    (goto-char fwd-bound)
    (beginning-of-line)
    (when
        (not
         (re-search-backward
          blank-line-regex
          (save-excursion (previous-line 1) (line-beginning-position)) t))
      (open-line 1))
))





;; -----
(defun mac-org-numbered-list ()
"Function to insert numbered lists in my preferred format of \'- [1] first point.\'"
(interactive)
(end-of-line)
(let* ((current-point (point))
       (current-number 1)
       (rev-bound (rev-bound))
       (fwd-bound (fwd-bound))
       (next-list-elt
        (save-excursion
          (re-search-forward list-regex fwd-bound t)))
       (prev-list-elt
        (save-excursion
          (re-search-backward list-regex rev-bound t))))

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
    (re-search-forward "\\(^ +\\+.*\n\\)+\\(^ .*\n\\)*" fwd-bound t))

  (insert (concat "\n    - [" (format "%s" current-number) "] "))
  (list-line-renumber)
  (re-search-backward "^    - " (line-beginning-position) t)
  (re-search-forward "\\[" (line-end-position) t)
  (setq current-number (format "%s" (number-at-point))
        fwd-bound (fwd-bound))
  (goto-char fwd-bound)
  (beginning-of-line)
  (list-line-remover)
  (setq fwd-bound (fwd-bound))
  (goto-char fwd-bound)
  (beginning-of-line)
  (list-line-adder)
  (setq fwd-bound (fwd-bound))
  (goto-char fwd-bound)
  (re-search-backward (concat "^    - \\["  current-number) rev-bound t)
  (end-of-line)
  (message current-number)
  ))





;; -----
(defun mac-org-todo-list ()
"Insert to-do lists in my preferred format."
(interactive)
(end-of-line)
(let* ((current-point (point))
       (rev-bound (rev-bound))
       (fwd-bound (fwd-bound))
       (next-list-elt
        (save-excursion
          (re-search-forward todo-regex fwd-bound t)))
       (prev-list-elt
        (save-excursion
          (re-search-backward todo-regex rev-bound t))))

  (when next-list-elt
    (progn
      (goto-char next-list-elt)
      (beginning-of-line)
      (open-line 1)))

  (when (not next-list-elt)
    ;; regex to get through sublists
    (re-search-forward "\\(^ +\\+.*\n\\)+\\(^ .*\n\\)*" fwd-bound t))

  (insert (concat "\n    - [] ~~\n"))
  (setq fwd-bound (fwd-bound))
  (goto-char fwd-bound)
  (beginning-of-line)
  (list-line-remover)
  (setq fwd-bound (fwd-bound))
  (goto-char fwd-bound)
  (beginning-of-line)
  (list-line-adder)
  (end-of-buffer)
  (re-search-backward "~~" rev-bound t)
  (replace-match "")
  ))





;; -----
(defun mac-org-numbered-or-todo ()
  "Insert a todo or numbered list item depending on context."
  (interactive)
  (let* ((current-point (point))
         (rev-bound (rev-bound))
         (fwd-bound (fwd-bound))
         (next-list-elt
          (save-excursion
            (re-search-forward todo-regex fwd-bound t)))
         (prev-list-elt
          (save-excursion
            (re-search-backward todo-regex rev-bound t))))

    (cond
     ((or
       (save-excursion
         (re-search-forward todo-regex fwd-bound t))
       (save-excursion
         (re-search-backward todo-regex rev-bound t)))
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
         (fwd-bound (fwd-bound))
         (list-bound))

    (setq list-bound (save-excursion (re-search-forward list-regex fwd-bound t)))
    (when (not list-bound)
      (setq list-bound fwd-bound))

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
        (fwd-bound
         (save-excursion
           (re-search-forward list-regex nil t)))
        (rev-bound
         (save-excursion
           (re-search-backward list-regex nil t))))

    (when (not fwd-bound)
      (setq fwd-bound
            (save-excursion
              (re-search-forward non-blank-line-regex nil t))))
    (when (not fwd-bound)
      (setq fwd-bound (point-max)))

    (when (not rev-bound)
      (setq rev-bound
            (save-excursion
              (re-search-backward non-blank-line-regex nil t))))
    (when (not rev-bound)
      (setq rev-bound (point-max)))

  (let ((curr-col)
        (next-sublist)
        (next-list-elt)
        (sublist-point))

    (when
        (save-excursion
          (re-search-forward "\n +[+*]" fwd-bound t))
      (progn
        (setq next-sublist (save-excursion
                             (re-search-forward "\n +[+*].*$" fwd-bound t)
                             (point)))))

    (cond
     (next-sublist
      (progn
        (setq curr-col
              (save-excursion
                (re-search-forward sublist-column-regex next-sublist)
                (1- (current-column))))
        (re-search-forward "\n +[+*].*$" fwd-bound t)
        (goto-char (match-beginning 0))
        (insert (concat "\n" (make-string curr-col #x20) "+ "))))

     ((looking-at (concat "\n" (substring list-regex 1)))
      (progn
        (setq curr-col
              (save-excursion
                (re-search-backward sublist-column-regex rev-bound t)
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
  (let* ((rev-bound (rev-bound))
         (fwd-bound (fwd-bound))
         (next-list-elt)
         (prev-list-elt)
         (line-end (line-end-position))
         (curr-col))

    (setq next-list-elt
          (save-excursion
           (re-search-forward list-regex fwd-bound t)))
    (when (not next-list-elt)
      (setq next-list-elt fwd-bound))

    (setq prev-list-elt
          (save-excursion
            (re-search-backward list-regex rev-bound t)))
    (unless (not prev-list-elt)
      (goto-char prev-list-elt))
    (beginning-of-line)
    (re-search-forward list-regex (line-end-position) t)
    (setq curr-col (current-column))
    (while
        (save-excursion
          (re-search-forward "[+*]" line-end t))
      (re-search-forward "[+*]" line-end t))

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
  (insert "~~")
  (list-line-remover)
  (list-line-adder)
  (re-search-forward "~~" nil t)
  (replace-match "")
  )




;; -----
(defun mac-alt-sublist-indent ()
  "Insert an alternative char '*' to denote separate sublist points."
  (interactive)
  (mac-sublist-indent)
  (re-search-backward "\\+" (line-beginning-position) t)
  (replace-match "* ")
  (fixup-whitespace)
  (insert " ")) 



  
;; -----
(defun mac-sub-sub-list ()
"Insert a 'level 2' item beneath a sub-list point." 
(interactive)
(let ((fwd-bound (fwd-bound))
      (rev-bound (rev-bound))
      (column))
  (end-of-line)

  ;; contingency for no sublist item 
  (unless
      (not
       (save-excursion 
         (re-search-backward " +\\([+*]\\) " rev-bound t)))
    
    (progn 
      (save-excursion 
        (re-search-backward "^ +\\([+*]\\) " rev-bound t)
        (goto-char (match-end 0))
        (setq column (current-column)))
      (end-of-line)
      (insert (concat "\n" (make-string column #x20) "- "))))))




;; -----
(defun mac-sub-sub-sub-list ()
  "Insert a 'level 2' item beneath a sub-list point." 
  (interactive)
  (let ((fwd-bound (fwd-bound))
        (rev-bound (rev-bound))
        (column))
    (end-of-line)

    (setq rev-bound (save-excursion (re-search-backward list-regex rev-bound t) (end-of-line) (point)))

    ;; contingency for no sublist item 
    (unless
        (not
         (save-excursion 
           (re-search-backward "^ +- " rev-bound t)))

      (progn 
        (save-excursion 
          (re-search-backward "^ +- " rev-bound t)
          (goto-char (match-end 0))
          (setq column (current-column)))
        (end-of-line)
        (insert (concat "\n" (make-string column #x20) "> "))))))






;; -----
;; open up org-mode bindings for my use
(define-key org-mode-map (kbd "C-c C-y") nil)
(define-key org-mode-map (kbd "C-c C-k") nil)

(global-set-key (kbd "C-c M-RET") 'mac-org-numbered-or-todo)
(global-set-key (kbd "C-c C-j")   'mac-sublist-indent)
(global-set-key (kbd "C-c C-k")   'mac-alt-sublist-indent)
(global-set-key (kbd "C-c C-y")   'mac-sub-sub-list)
(global-set-key (kbd "C-c C-7")   'mac-sub-sub-sub-list)


