
(defun my-custom-function ()
  "My custom function that prints a message."
  (interactive)
  (message "Hello, Emacs!"))

;; =========
(setq org-format-latex-options
      '(:foreground "white" :background "#2e2e2e" :scale 1.2 :html-foreground "navy" :html-background "lemonchiffon"))
(global-linum-mode t)
;;(setq org-adapt-indentation nil)
;;(setq org-indent-mode -1)

;;==============================================
(setq org-startup-folded 'showeverything)
(setq org-html-postamble nil)

(defun my-org-html-head (exporter)
  (when (eq exporter 'html)
    (setq-local org-html-head-extra
                (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\" />\n"
                        org-html-head-extra))))

(add-hook 'org-export-before-processing-hook 'my-org-html-head)



;; ==============================================

(defun org-latex-preview-all-sections ()
  "Run `org-latex-preview' for all sections in the buffer."
  (interactive)
  (let ((org-heading-regexp "^\\*+ "))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (org-latex-preview)))))

(add-hook 'org-mode-hook 'org-latex-preview-all-sections)

(global-set-key (kbd "C-l") 'org-latex-preview)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-l C-a") 'org-latex-preview-all-sections)
