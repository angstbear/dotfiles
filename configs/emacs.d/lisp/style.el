;;; style.el --- Style configuration
;;;
;;; Commentary:
;;;
;;; Code:


(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Set titles for frame and icon (%f == file name, %b == buffer name)
(setq-default frame-title-format (list "%f"))
(setq-default icon-title-format "Emacs - %b")

;; Cut / Copy / Paste
;; see http://www.emacswiki.org/emacs/CopyAndPaste#toc6
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; OS-specific preferences (themes, fonts, etc.)
(cond
 ((string-equal system-type "gnu/linux") ; Linux
  (load-theme 'solarized-dark t))

 ((string-equal system-type "windows-nt") ; Windows
  (require 'powerline)
  (setq powerline-height 20)
  (setq powerline-default-separator 'arrow)
  (require 'airline-themes)
  (load-theme 'solarized-dark t)
  (load-theme 'airline-solarized-alternate-gui t))

 ((string-equal system-type "darwin") ; Mac OS X
  ;; sRGB doesn't blend with Powerline's pixmap colors, but is only
  ;; used in OS X. Disable sRGB before setting up Powerline.
  (setq ns-use-srgb-colorspace nil)

  (require 'powerline)
  (setq powerline-height 20)
  (setq powerline-default-separator 'arrow)

  ;;(require 'color-theme)
  ;;(color-theme-initialize)
  ;;(color-theme-robin-hood)
    
  (require 'airline-themes)
  (when (display-graphic-p)
    (load-theme 'solarized-dark t)
    (load-theme 'airline-solarized-alternate-gui t))))

;; color-theme-approximate for terminal color degrade
;;(when (eq (symbol-value 'display-graphic-p) nil)
(autoload 'color-theme-approximate-on "color-theme-approximate")
(color-theme-approximate-on);)

;; emojify - display emojis on emacs init
(add-hook 'after-init-hook #'global-emojify-mode)

;; emoji-cheat-sheet-plus
;; enabled emoji in buffer
(add-hook 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)

;; cycle-powerline-separators function
;; Originally from https://github.com/aaronbieber/dotfiles
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
;; From http://jr0cket.co.uk/2013/01/tweeking-emacs-modeline-for-clojure.html
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas-minor-mode . " γ")
    (paredit-mode . " Φ")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . " u")
    (helm-mode . " θ")
    (guide-key-mode . " κ")
    (volatile-highlights-mode . " υ")
    (smooth-scroll-mode . " δ")
    (nrepl-mode . " ηζ")
    (nrepl-interaction-mode . " ηζ")
    ;; Major modes
    (flycheck-mode . " ζ")
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

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; Greek letters - C-u C-\ greek ;; C-\ to revert to default
;;; ς ε ρ τ υ θ ι ο π α σ δ φ γ η ξ κ λ ζ χ ψ ω β ")

;;; Note:
;;; Another way to do this in the future via diminish.el
;; (eval-after-load "cperl-mode" '(diminish 'cperl-mode '(" cP")))

(provide 'style.el)
;;; style.el ends here
