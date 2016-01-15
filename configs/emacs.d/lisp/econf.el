;;; econf.el --- general emacs configuration
;;;
;;; Commentary:
;;;
;;; Code:

;; Dont show the GNU splash screen, etc.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; highlight during query
(setq query-replace-highlight t)
;; highlight incremental search
(setq search-highlight t)
;; highlight matching paren
(show-paren-mode t)
;; show column number and line number
(dolist (mode '(column-number-mode line-number-mode))
  (when (fboundp mode) (funcall mode t)))
;; Toggle line highlighting in all buffers
(when (display-graphic-p)
  (setq show-paren-style 'expression)
  (global-hl-line-mode t))

;; show current function in modeline
;;(which-function-mode t)
(put 'downcase-region 'disabled nil)

;; Backup files
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;; Tab width
(setq tab-width 4)
;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Indent with RET instead of requiring TAB on newline
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Allow Narrowing
(put 'narrow-to-region 'disabled nil)

;; save desktop sessions automatically
;;(desktop-save-mode 1)

(provide 'config.el)
;;; econf.el ends here
