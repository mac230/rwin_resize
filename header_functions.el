(defvar do-not-capitalize '("is"
                            "a"
                            "an"
                            "of"
                            "the"
                            "with"
                            "in"
                            "is"
                            "for"
                            "to"
                            "on"
                            "DNA"
                            "RNA"
                            "siRNA"
                            "shRNA"
                            "PCR"
                            "CRISPR"
                            "CRISPRa"
                            "CRISPRi"
                            "QTL"
                            "TFT"
                            "which"
                            "and"
                            "this" 
                            )
  "List of words to not capitalize in headings.")

(defvar never-capitalize '("DNA"
                           "RNA"
                           "mRNA"
                           "QTL"
                           "UTR"
                           "CRISPR"
                           "shRNA"
                           "siRNA"
                           "X-pQTL"
                           "GFP"
                           "RFP"
                           "TFT"
                           "PCR"
                           "sfGFP"
                           "mCherry")
  "List of words that I never want modified by 'capitalize-word' - mostly abbreviations.")


(defun mac-header-capitalization ()
  "Helper function that capitalizes words in a heading."
  (interactive)

  ;; start by cleaning any extraneous whitespace
  (beginning-of-line)
  (fixup-whitespace)
  (end-of-line)
  (delete-trailing-whitespace (line-beginning-position) (line-end-position))

  (when (eobp)
   (save-excursion (insert "\n")))
  
  (let ((p (line-beginning-position))
        (e (line-end-position))
        (current-word)
        (count 0)
        (case-fold-search nil))

    ;; first word is handled slightly differently than the rest
    ;; since we capitalize words like "the" if they're the first one.
    (goto-char p)
    (while (not (word-at-point))
      (forward-char 1))
    (setq current-word (word-at-point))
    (unless (or
         (string-match "[[:lower:]]+[[:upper:]]+" current-word)
         (string-match "[[:upper:]]\\{2\\}+" current-word))
      (progn (capitalize-word 1) (forward-char 1) (setq p (point))))
    
    (while
        ;; do-again test
        (< p e)

      ;; contingency in case point moves to a non-word position
      (while (not (word-at-point)) (forward-char 1))
      (setq current-word (word-at-point))

      ;; first test - see if current-word is one that's never capitalized
      (dolist (check do-not-capitalize count)
        (when (string= check current-word)
          (setq count (1+ count))))

      ;; second test - see if current word has double caps or something like eGFP
      (when (or
             (string-match-p "[[:lower:]]+[[:upper:]]+" current-word)
             (string-match-p "[[:upper:]]\\{2\\}+" current-word))
        (setq count (1+ count)))

      ;; decrementing function
      (if (> 1 count)
          (progn (capitalize-word 1) (unless (eobp)) (forward-char 1) (setq p (point))) 
        (progn (forward-word 1) (unless (eobp)) (forward-char 1) (setq p (point))))
      (setq count 0))
    (goto-char e))
  )



;; 2019.06.26 - fix so that no longer capitalizes words like 'eQTL', 'PCR', etc...
(defun italic-heading-new ()
  "Function to insert a non-foldable org-mode heading that surrounds the heading with '-' and italicizes the text."
  (interactive)
  (mac-header-capitalization)
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        (current-word)
        (count 0)
        (case-fold-search nil)
        (n-dashes  (1+ (save-excursion (end-of-line) (current-column))))
        (dashes-inserted 0)
        (top-dash-line)
        (bottom-dash-line))

  ;; control for situations where you create a heading at the end of the document
  (goto-char end)
  (end-of-line)
 
  (when (eobp)
      (progn
        (insert "\n")
        (goto-char beg)
        (setq beg (line-beginning-position)
              end (line-end-position))))

  ;; control for situations where you create a heading at the beginning of the document
  (goto-char beg)
  (beginning-of-line)
  (when (bobp)
    (progn
      (insert "\n")
      (goto-char (1+ first-word)) ;; 1+ needed because first word becomes the first line of the buffer
        (setq beg (line-beginning-position)
              end (line-end-position))))

  ;; control for situation where there is not a blank line above or below your header
  ;; above
  (goto-char beg)
  (previous-line)
  (beginning-of-line)
  (when (re-search-forward "[^ \t\n\f]+" (line-end-position) t)
      (progn
        (end-of-line)
        (insert "\n")
        (goto-char (1+ beg))
        (setq beg (line-beginning-position)
              end (line-end-position))))

  ;; below
  (goto-char end)
  (next-line)
  (beginning-of-line)
  (when (re-search-forward "[^ \t\n\f]+" (line-end-position) t)
      (progn
        (beginning-of-line)
        (insert "\n")
        (goto-char beg)
        (setq beg (line-beginning-position)
              end (line-end-position))))
;; now capitalize
;;  (goto-char end)
;;  (mac-header-capitalization)
;;  (goto-char beg)

  ;; while loop to insert dashes for the lines above and below the header
  ;; can't do the bottom line at the same time because the relative position changes w/ each "-" insertion
  (goto-char beg)
  (end-of-line)
  (insert "/")
  (beginning-of-line)
  (insert "/")

  (setq beg (line-beginning-position)
        end (line-end-position)
        top-dash-line (save-excursion (previous-line) (line-beginning-position)))

  (while (< dashes-inserted (1+ n-dashes))
    (progn
      (goto-char top-dash-line)
      (insert "-")
      (setq dashes-inserted (1+ dashes-inserted))))

  ;; reset for the next round of this
  (setq dashes-inserted 0)

  ;; repeat the while loop to insert dashes below
  (goto-char end)
  (setq bottom-dash-line (save-excursion (next-line) (line-beginning-position)))
  (while (< dashes-inserted (1+ n-dashes))
    (progn
      (goto-char bottom-dash-line)
      (insert "-")
      (setq dashes-inserted (1+ dashes-inserted))))

  ;; End by putting point in position to continue typing
  (end-of-line)
  (newline)))


(defun date-header ()
  "Create dates with italic headings in documents that are not foldable but useful for separating sections of notes."
  (interactive)
  (insert (format-time-string "%Y.%m.%d"))
  (italic-heading-new))


(defun hcapitalize (head)
  "Create org-mode headings and capitizalize the relevant text in automatically.  
Use w/ a hydra to rapidly and efficiently generate org-headings."
  (interactive "kLevel: ")
  (beginning-of-line)
  (cond
   ((equal head (kbd "j"))
    (insert "* "))
   ((equal head (kbd "k"))
    (insert "** "))
   ((equal head (kbd "l"))
    (insert "*** "))
   ((equal head (kbd ";"))
    (insert "**** " ))
   (t
    (message "Invalid header specifications")))
  (mac-header-capitalization))

