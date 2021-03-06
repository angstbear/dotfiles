;;; init.el --- emacs configuration
;;;
;;; Commentary:
;;;
;;; Code:

;; Package repositories (require 'package)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(require 'use-package)
(require 'diminish)
(require 'bind-key)


;;; GENERAL SETTINGS

;; Dont show the GNU splash screen, menu-bar, etc.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(when (boundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (display-graphic-p) ; GUI
  ;; Set titles for frame and icon (%f == file name, %b == buffer name)
  (setq-default frame-title-format (list "%f"))
  (setq-default icon-title-format "Emacs - %b")

  (unless (string-equal system-type "darwin") ; Linux & Windows
    ;; Cut/Copy/Paste to & from clipboard
    (global-set-key "\C-w" 'clipboard-kill-region)
    (global-set-key "\M-w" 'clipboard-kill-ring-save)
    (global-set-key "\C-y" 'clipboard-yank)))

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
(when (display-graphic-p)
  (desktop-save-mode 1))

;; Add homebrew bin directory to exec-path on Mac
(when (string-equal system-type "darwin")
  (add-to-list 'exec-path "/usr/local/bin"))


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
(use-package epa-file)
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tt\\'"    . web-mode))
  :config
  ;; Web Mode customizations
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (linum-mode 1)
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4))
  (add-hook 'web-mode-hook  'my-web-mode-hook))
(use-package evil
  :demand
  :config
  (evil-mode 1)
  (setq evil-move-cursor-back nil)
  ;; page up/down with C-k/j
  (define-key evil-normal-state-map (kbd "C-k") (lambda ()
                                                  (interactive)
                                                  (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-j") (lambda ()
                                                  (interactive)
                                                  (evil-scroll-down nil))))
(use-package evil-numbers
  :init
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))
(use-package helm-config
  :bind (("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t))
(use-package guide-key
  :config
  (guide-key-mode 1)
  (setq-default guide-key/guide-key-sequence t
                guide-key/idle-delay 0.5))
(use-package auto-complete-config
  :config
  (ac-config-default))
(use-package smooth-scroll
  :if (display-graphic-p)
  :config
  (smooth-scroll-mode t))

(use-package helm-projectile)
(use-package fish-mode)
(use-package gist)

(use-package markdown-mode
  :config
  (setq markdown-command "/usr/local/bin/markdown")
  (define-key markdown-mode-map     (kbd "TAB") 'markdown-cycle)
  (define-key evil-normal-state-map (kbd "TAB") 'markdown-cycle))
(use-package achievements
  :diminish "Achv"
  :config
  (defun achievements-progress ()
    "Show our current achievements progress."
    (interactive)
    (achievements-list-achievements)
    (message (concat
              "Your current achievments score: "
              (int-to-string (round achievements-score)))))
  (global-set-key (kbd "C-c a") 'achievements-progress))
(use-package twittering-mode
  :load-path "~/.emacs.d/twittering-mode-3.0.0"
  :config
  (setq twittering-use-master-password t)
  (setq twittering-cert-file "/usr/local/etc/openssl/cert.pem"))
(use-package emojify
  :diminish "e"
  :config
  (add-hook 'org-mode-hook      'emojify-mode)
  (add-hook 'markdown-mode-hook 'emojify-mode)
  (add-hook 'eww-mode-hook      'emojify-mode))
(use-package emoji-cheat-sheet-plus
  :diminish "ecs"
  :config
  (add-hook 'org-mode-hook      'emoji-cheat-sheet-plus-display-mode)
  (add-hook 'markdown-mode-hook 'emoji-cheat-sheet-plus-display-mode))
(use-package jabber
  :init
  ;; Define location of gnutls-cli on OS X, installed via brew
  (when (string-equal system-type "darwin")
    (setq starttls-use-gnutls t
          starttls-gnutls-program "/usr/local/bin/gnutls-cli"
          starttls-extra-arguments nil))
  :config
  (let ((emacs-jabber-dir "~/.emacs.d/jconf/"))
    (when (file-exists-p emacs-jabber-dir)
      (add-to-list 'load-path emacs-jabber-dir)
      (require 'account-list)))
  (defvar jabber-alert-message-hooks '(jabber-message-echo jabber-message-scroll jabber-message_beep))
  (setq jabber-alert-presence-hooks nil
        jabber-autoaway-timeout 5
        jabber-autoaway-xa-timeout 0
        jabber-chat-buffer-show-avatar nil
        jabber-keepalive-interval 120
        jabber-message-alert-same-buffer nil
        ;;jabber-chatstates-confirm nil        ; Don't send typing events
        ;;jabber-roster-show-bindings nil      ; Don't show the help text at the top by default
        jabber-auto-reconnect t              ; Automatically try to reconnect when disconnect detected
        jabber-roster-line-format " %-50n %u %-8s  %S" ; Roster contact format
        jabber-show-offline-contacts nil     ; Don't show offline contacts by default
        jabber-show-resources nil            ; Don't show resources
        jabber-history-enabled t             ; Keep history
        ;;jabber-use-global-history nil        ; Store chat history in per-contact files
        ;;jabber-history-dir "~/.emacs.d/jabber-history"  ; Set history directory
        jabber-activity-count-in-title t
        fsm-debug nil                        ; No debug messages
        undo-outer-limit 99900000)           ; bh jabber tends bombard undo capacity in the roster buffer
  ;; Don't allow anonymous authentication
  (defadvice jabber-xml-get-children (after eaw-remove-anonymous)
    (setq ad-return-value (remove '(mechanism nil "ANONYMOUS") ad-return-value)))
  (ad-activate 'jabber-xml-get-children)
  ;; Allow multi-line with Shift+Enter
  (add-hook 'jabber-chat-mode-hook
            (lambda ()
              (define-key jabber-chat-mode-map [(shift return)] 'newline))))

(use-package circe ; irc server and channel config in jconf/account-list.el
  :config
  (setq circe-default-quit-message "quit: Leaving")
  (setq circe-default-part-message "quit: Leaving"))


;;; COLOR THEMES

(unless (display-graphic-p) ; Terminal
  (add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
  (load-theme 'solarized t)
  (customize-set-variable 'solarized-termcolors 256)
  (set-frame-parameter nil 'background-mode 'dark)
  (set-terminal-parameter nil 'background-mode 'dark)
  (enable-theme 'solarized)
  (column-number-mode 1)
  ;; color-theme-approximate for terminal color degrade
  (autoload 'color-theme-approximate-on "color-theme-approximate")
  (color-theme-approximate-on))

(when (display-graphic-p) ; GUI
  (use-package powerline
    :init
    (when (string-equal system-type "darwin") ; Mac OS X
      ;; sRGB doesn't blend with Powerline's pixmap colors, but is only
      ;; used in OS X. Disable sRGB before setting up Powerline.
      (setq ns-use-srgb-colorspace nil))
    :config
    (setq powerline-height 20)
    (setq powerline-default-separator 'zigzag)
    (use-package airline-themes
      :config
      (load-theme 'solarized-dark t)
      (load-theme 'airline-solarized-alternate-gui t)))

  ;; cycle-powerline-separators
  ;; (from: https://github.com/aaronbieber/)
  (defun cycle-powerline-separators (&optional reverse)
    "Set Powerline separators in turn. If REVERSE is not nil, go backwards."
    (interactive)
    (let* ((fn (if reverse 'reverse 'identity))
           (separators (funcall fn '("arrow" "arrow-fade" "slant"
                                     "chamfer" "wave" "brace" "roundstub" "zigzag"
                                     "butt" "rounded" "contour" "curve")))
           (found nil))
      (while (not found)
        (progn (setq separators (append (cdr separators) (list (car separators))))
               (when (string= (car separators) powerline-default-separator)
                 (progn (setq powerline-default-separator (cadr separators))
                        (setq ns-use-srgb-colorspace nil)
                        (setq found t)
                        (redraw-display)))))))
  ;; clean-mode-line
  ;; from: http://jr0cket.co.uk/2013/01/tweeking-emacs-modeline-for-clojure.html.html
  (defvar mode-line-cleaner-alist
    `((auto-complete-mode . " α")
      (yas-minor-mode . " γ")
      (paredit-mode . " Φ")
      (eldoc-mode . "")
      (abbrev-mode . "")
      (auto-revert-mode . "")
      (undo-tree-mode . " u")
      (helm-mode . " θ")
      (guide-key-mode . " κ")
      (volatile-highlights-mode . " υ")
      (smooth-scroll-mode . " δ")
      (nrepl-mode . " ηζ")
      (nrepl-interaction-mode . " ηζ")
      ;; Major modes
      (flycheck-mode . " ζ")
      (jabber-chat-mode . " Jbr")
      (visial-line-mode . " υ")
      (hi-lock-mode . "")
      (python-mode . " Py")
      (cperl-mode . "ςP")
      (emacs-lisp-mode . "ξL")
      (markdown-mode . "md"))
    "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")
  ;; Greek letters - C-u C-\ greek ;; C-\ to revert to default
  ;; ς ε ρ τ υ θ ι ο π α σ δ φ γ η ξ κ λ ζ χ ψ ω β ")

  (defun clean-mode-line ()
    (interactive)
    (cl-loop for cleaner in mode-line-cleaner-alist
             do (let* ((mode (car cleaner))
                       (mode-str (cdr cleaner))
                       (old-mode-str (cdr (assq mode minor-mode-alist))))
                  (when old-mode-str
                    (setcar old-mode-str mode-str))
                  ;; major mode
                  (when (eq mode major-mode)
                    (setq mode-name mode-str)))))
  (add-hook 'after-change-major-mode-hook 'clean-mode-line))


;;; GLOBAL KEY BINDINGS

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
(global-set-key (kbd "C-c C-e") 'emoji-cheat-sheet-plus-insert)
(global-set-key (kbd "C-c C-j") 'jabber-connect)

;; Function Key Bindings
(global-set-key [f1] 'goto-line)
(global-set-key [f2] 'comment-or-uncomment-region)
(global-set-key [f3] 'linum-mode)                 ; toggle line numbers
(global-set-key [f4] 'flycheck-mode)              ; toggle flycheck
(global-set-key [f5] 'evil-mode)                  ; toggle evil-mode
(global-set-key [f6] 'org-toggle-inline-images)   ; not globally applicable; should be moved to modes.el
(global-set-key [f7] 'cycle-powerline-separators)
(global-set-key [f8] 'delete-trailing-whitespace)
(global-set-key [f9] 'filesets-open)
(global-set-key [mouse-3] 'imenu)

(provide 'init)
;;; init.el ends here
