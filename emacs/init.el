;;; init.el --- Main init file

;;; Commentary:
;;;
;;; Code:

(add-to-list 'load-path "~/.emacs.d/use-package")
(require 'use-package)
(require 'bind-key)


;;; GENERAL SETTINGS

;; Dont show the GNU splash screen, etc.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)

(setq query-replace-highlight t)      ; highlight during query
(setq search-highlight t)             ; highlight incremental search
(global-hl-line-mode t)               ; highlight current line
(show-paren-mode t)                   ; highlight matching paren
(setq show-paren-style 'expression)   ; highlight content between parens
;;(which-function-mode t)               ; show current function in modeline
(setq tab-width 4)                    ; tab-width
(setq-default indent-tabs-mode nil)   ; use spaces instead of tabs
(setq make-backup-files nil)          ; plz don't make backup files
(put 'narrow-to-region 'disabled nil) ; allow Narrowing
(put 'downcase-region 'disabled nil)  ; allow down-casing
(put 'dired-find-alternate-file 'disabled nil)          ; allow dired 'a' functionality
(define-key global-map (kbd "RET") 'newline-and-indent) ; "auto-indent"
(setq require-final-newline t)                          ; require newline at EOF

;; Session Management
(filesets-init)


;;; MODES & EXTRA PACKAGES

;; cperl mode setup
(defalias    'perl-mode          'cperl-mode)
(defvaralias 'c-basic-offset     'default-tab-width)
(defvaralias 'cperl-indent-level 'default-tab-width)
(setq cperl-indent-level 4 ; fix emacs wonky paren indenting issues
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)
(add-to-list 'auto-mode-alist '("\\.pm\\.t?\\'" . cperl-mode))

;; Lazy load our goodies and extras
(use-package linum)
(use-package web-mode
   :load-path "~/.emacs.d/web-mode"
   :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tt\\'"    . web-mode))
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4))
  (add-hook 'web-mode-hook  'my-web-mode-hook))
(use-package evil
  :demand
  :load-path "~/.emacs.d/evil"
  :config
  (evil-mode 1)
  (setq evil-move-cursor-back nil)
  ;; page up/down with C-j/k
  (define-key evil-normal-state-map (kbd "C-k") (lambda ()
                                                  (interactive)
                                                  (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-j") (lambda ()
                                                  (interactive)
                                                  (evil-scroll-down nil))))
(add-to-list 'load-path "~/.emacs.d/emacs-async")
(use-package helm-config
  :load-path "~/.emacs.d/helm"
  :config
  (setq helm-buffers-fuzzy-matching t)
  (helm-mode 1)
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-buffers-list)
         ("C-x C-f" . helm-find-files)))
(use-package fish-mode
  :load-path "~/.emacs.d/emacs-fish")


;;; COLOR THEMES

;; Terminal
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized t)
(customize-set-variable 'solarized-termcolors 256)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)
(column-number-mode 1)


;;; KEY BINDINGS

;; C-c [a-zA-Z] Bingings
(global-set-key (kbd "C-c c") 'calendar)
(global-set-key (kbd "C-c e") 'evil-mode)
(global-set-key (kbd "C-c k") 'next-buffer)
(global-set-key (kbd "C-c j") 'previous-buffer)
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c p") 'previous-buffer)
(global-set-key (kbd "C-c l") 'linum-mode)

;; C-c C-[a-zA-Z] Bingings
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region) 

;; Function Key Bindings
(global-set-key [f1] 'goto-line)
(global-set-key [f2] 'comment-or-uncomment-region) ; toggle comment
(global-set-key [f3] 'linum-mode)                  ; toggle line numbers
(global-set-key [f5] 'evil-mode)                   ; toggle evil-mode
(global-set-key [f8] 'delete-trailing-whitespace)
(global-set-key [f9] 'filesets-open)


(provide 'init)
;;; init.el ends here
