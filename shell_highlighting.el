(defface mac-dirs-face
  '((t (:foreground "#5555EE" :weight semi-bold)))
  "Face for my window selection function.")

(defface mac-file-face
  '((t (:foreground "#447744" :weight semi-bold)))
  "Face for my window selection function.")

(defface mac-bash-prompt-face
  '((t (:foreground "#771111" :weight semi-bold)))
  "Face for my window selection function.")

(set-face-attribute 'comint-highlight-prompt nil
                    :foreground "#771111"
                    :weight 'semi-bold
                    :inherit nil)

;; 2019.05.13 try to get some highlighting in shell like in eshell
(defun font-lock-executable-annotation ()
   (font-lock-add-keywords
   'shell-mode
   '(("\\(d[drwx-]\\{9\\}.*[0-9:]\\{4,5\\} \\)\\(.*[a-z0-9]*$\\)" 2 'mac-dirs-face)
     ("\\(-[drwx-]\\{9\\}.*[0-9:]\\{4,5\\} \\)\\(.*\\.[a-z]\\{2,4\\}$\\)" 2 'mac-file-face)
     ("bash.*\\$" . 'mac-bash-prompt-face)
     ("\\.\\(zip\\|gz\\)" . 'eshell-ls-archive-face)
     ))
   )


;; 2019.05.13 try to get some highlighting in shell like in eshell
(defun font-lock-executable-annotation ()
   (font-lock-add-keywords
   'shell-mode
   '(("\\(d[drwx-]\\{9\\}.*[0-9:]\\{4,5\\} \\)\\(.*[a-z0-9]*$\\)" 2 'mac-dirs-face)
     ("\\(-[drwx-]\\{9\\}.*[0-9:]\\{4,5\\} \\)\\(.*\\.[a-z]\\{2,4\\}$\\)" 2 'mac-file-face)
     ("bash.*\\$" . 'mac-bash-prompt-face)
     ("\\.\\(zip\\|gz\\)" . 'eshell-ls-archive-face)
     ))
   )


(defun shell-hook ()
  "Hook to use company-pcomplete, company-dabbrev, and company-files in shell and file completion w/ space"
  (interactive)
  (set (make-local-variable 'completion-at-point-functions) '(pcomplete-completions-at-point t))
  (set (make-local-variable 'company-backends) '((company-files company-shell company-pcomplete company-dabbrev company-capf company-yasnippet :separate)))
  (set (make-local-variable 'company-frontends) '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)) 
  (setq pcomplete-file-ignore nil)   ;; don't ignore any file names
  (setq pcomplete-dir-ignore nil)    ;; don't ignore any dir names
  (font-lock-executable-annotation)
  (font-lock-mode)
  (setq company-shell-clean-manpage t)
  (company-mode))

(add-hook 'shell-mode-hook 'shell-hook)
