;;; modes.el --- Mode configurations

;;; Commentary:
;;;
;;; Code:


;; Default mode (open unidentified files in org mode)
(setq major-mode 'org-mode)

;; Text files in org-mode
(add-to-list 'auto-mode-alist '("\\.txt?\\'" . org-mode))

;; cperl mode
(defalias 'perl-mode 'cperl-mode)
(defvaralias 'c-basic-offset 'default-tab-width)
(defvaralias 'cperl-indent-level 'default-tab-width)
;; fix emacs wonky paren indenting issues
(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)
(add-to-list 'auto-mode-alist '("\\.pm\\.t?\\'" . cperl-mode))

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tt\\'"    . web-mode))
;; Web Mode customizations
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Evil
(require 'evil)
(evil-mode 1)
;; exiting insert-mode, don't move cursor back
(setq evil-move-cursor-back nil)
;; page up / page down with C-k/j
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
                                                (interactive)
                                                (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
                                                (interactive)
                                                (evil-scroll-down nil)))
;; Evil Numbers (vim-like increment/decrement)
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-visual-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Dired Mode
(put 'dired-find-alternate-file 'disabled nil)

;; Helm mode (require all the time)
(require 'helm)
(helm-mode 1)
(setq helm-buffers-fuzzy-matching t)

;;; guide-key mode
(guide-key-mode 1)
(setq-default guide-key/guide-key-sequence t
              guide-key/idle-delay 0.5)

;; linum mode
(dolist (mode-hook '(text-mode-hook prog-mode-hook))
  (add-hook mode-hook
            (lambda ()
              (linum-mode 1))))

;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)

;; Smooth scroll
(require 'smooth-scroll)
(smooth-scroll-mode t)

(provide 'modes)
;;; modes.el ends here
