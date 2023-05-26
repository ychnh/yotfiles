(defun my-custom-function ()
  "My custom function that prints a message."
  (interactive)
  (message "Hello, Emacs!"))
(global-set-key (kbd "C-c C-f") 'my-custom-function)

;; =========
(setq org-format-latex-options
      '(:foreground "white" :background "#2e2e2e" :scale 1.5 :html-foreground "navy" :html-background "lemonchiffon"))
(global-linum-mode t)
