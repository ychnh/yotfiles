;;; init.el --- Spacemacs Initialization File -*- no-byte-compile: t -*-
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Avoid garbage collection during startup.
;; see `SPC h . dotspacemacs-gc-cons' for more info

(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(load (concat (file-name-directory load-file-name) "core/core-load-paths")
      nil (not init-file-debug))
(load (concat spacemacs-core-directory "core-versions")
      nil (not init-file-debug))
(load (concat spacemacs-core-directory "core-dumper")
      nil (not init-file-debug))


;; Remove compiled core files if they become stale or Emacs version has changed.
(load (concat spacemacs-core-directory "core-compilation")
      nil (not init-file-debug))
(load spacemacs--last-emacs-version-file t (not init-file-debug))
(when (or (not (string= spacemacs--last-emacs-version emacs-version))
          (> 0 (spacemacs//dir-byte-compile-state
                (concat spacemacs-core-directory "libs/"))))
  (spacemacs//remove-byte-compiled-files-in-dir spacemacs-core-directory))
;; Update saved Emacs version.
(unless (string= spacemacs--last-emacs-version emacs-version)
  (spacemacs//update-last-emacs-version))

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  ;; Disabling file-name-handlers for a speed boost during init might seem like
  ;; a good idea but it causes issues like
  ;; https://github.com/syl20bnr/spacemacs/issues/11585 "Symbol's value as
  ;; variable is void: \213" when emacs is not built having:
  ;; `--without-compress-install`
  (let ((please-do-not-disable-file-name-handler-alist nil))
    (require 'core-spacemacs)
    (spacemacs/dump-restore-load-path)
    (configuration-layer/load-lock-file)
    (spacemacs/init)
    (configuration-layer/stable-elpa-init)
    (configuration-layer/load)
    (spacemacs-buffer/display-startup-note)
    (spacemacs/setup-startup-hook)
    (spacemacs/dump-eval-delayed-functions)
    (when (and dotspacemacs-enable-server (not (spacemacs-is-dumping-p)))
      (require 'server)
      (when dotspacemacs-server-socket-dir
        (setq server-socket-dir dotspacemacs-server-socket-dir))
      (unless (server-running-p)
        (message "Starting a server...")
        (server-start)))))


(defun my-custom-function ()
  "My custom function that prints a message."
  (interactive)
  (message "Hello, Emacs!"))

;; =========
(setq org-format-latex-options
      '(:foreground "white" :background "#2e2e2e" :scale 0.85 :html-foreground "navy" :html-background "lemonchiffon"))
(global-linum-mode t)


;(setq org-adapt-indentation nil)
;(setq org-indent-mode -1)
;; Keep the same indent level for all lines
;(setq electric-indent-mode t)
;(setq-default evil-shift-width 2)
;(setq-default tab-width 2)
;(setq-default tab-stop-list (number-sequence 2 120 2))




;;==============================================
;;(setq org-startup-folded 'showeverything)
(setq org-html-postamble nil)
(setq org-html-toc nil)

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
(global-set-key (kbd "C-k") 'org-latex-preview-all-sections)

;;(setq org-superstar-headline-bullets-list '("#"))
(setq org-superstar-prettify-item-bullets nil)



(setq-default dotspacemacs-show-trailing-whitespace nil)
(setq org-ellipsis " ++")
(setq org-M-RET-may-split-line nil)

(defun my-indent-settings()
  (interactive)
  (setq indent-line-function 'indent-relative))
(add-hook 'org-mode-hook 'my-indent-settings)
(setq indent-line-function 'indent-relative)

(defun my-indent-and-newline ()
  "Insert a newline and indent using `indent-relative'."
  (interactive)
  (newline)
  (indent-relative-maybe))

(eval-after-load 'evil
  '(progn
     (define-key evil-insert-state-map (kbd "RET") 'my-indent-and-newline)))

(setq org-display-outline-path nil)
;(enable-theme 'grayscale-theme)
; =====================================================

(defun my-custom-function ()
  "My custom function that prints a message."
  (interactive)
  (message "Hello, Emacs!"))

(defun orgfold-get-fold-info-file-name ()
  (interactive)
  (concat (file-name-directory buffer-file-name) "." (file-name-nondirectory buffer-file-name) ".fold"))

(defun orgfold-save ()
  (interactive)
  (save-excursion
    (goto-char (point-min))

    (let (foldstates)
      (unless (looking-at outline-regexp)
        (outline-next-visible-heading 1))

      (while (not (eobp))
        (push (when (seq-some (lambda (o) (overlay-get o 'invisible))
                         (overlays-at (line-end-position)))
                t)
              foldstates)
        (outline-next-visible-heading 1))

      (with-temp-file (orgfold-get-fold-info-file-name)
        (prin1 (nreverse foldstates) (current-buffer))))))

(defun orgfold-restore ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((foldfile (orgfold-get-fold-info-file-name))
           (foldstates
            (when (file-readable-p foldfile)
              (with-temp-buffer
                (insert-file-contents foldfile)
                (when (> (buffer-size) 0)
                  (read (current-buffer)))))))

      (when foldstates
        (show-all)
        (goto-char (point-min))

        (unless (looking-at outline-regexp)
          (outline-next-visible-heading 1))

        (while (and foldstates
                    (not (eobp)))
          (when (pop foldstates)
            (hide-subtree))

          (outline-next-visible-heading 1))

        (message "restored saved folding")))))

(defun orgfold-activate ()
  (interactive)
  (orgfold-restore)
  (add-hook 'kill-buffer-hook 'orgfold-kill-buffer nil t))

(defun orgfold-kill-buffer ()
  ;; don't save folding info for unsaved buffers
  (interactive)
  (unless (buffer-modified-p)
    (orgfold-save)))

(add-hook 'org-mode-hook 'orgfold-activate)
(add-hook 'after-save-hook 'orgfold-save)

(defun my-save-and-savefold ()
  (interactive)
  (orgfold-save)
  (save-buffer)
  (message "saving orgfold")
)

;;===============
(global-set-key (kbd "C-s") 'my-save-and-savefold)

(defun save-and-quit-emacs ()
  "Save buffers and quit Emacs."
  (interactive)
  (save-some-buffers t)
  (kill-emacs))

(global-set-key (kbd "C-q") 'save-and-quit-emacs)
