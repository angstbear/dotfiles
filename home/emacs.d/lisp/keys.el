;;; keys.el --- Global key bindings

;;; Commentary:
;;;
;;; Currently mostly just using Function keys
;;;
;;; Code:

;; C-c [a-zA-Z] Bingings
(global-set-key (kbd "C-c c") 'calendar)
(global-set-key (kbd "C-c k") 'next-buffer)
(global-set-key (kbd "C-c j") 'previous-buffer)
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c p") 'previous-buffer)

;; C-c C-[a-zA-Z] Bingings
(global-set-key (kbd "C-c C-e") 'emoji-cheat-sheet-plus-insert) ;; insert emoji w/Helm

;; Function Key Bindings
(global-set-key [f1] 'goto-line)
(global-set-key [f2] 'comment-region)
(global-set-key [(shift f2)] 'universal-argument) ; uncomment is Shift-F2 F2
(global-set-key [f3] 'linum-mode) ; toggle line numbers
(global-set-key [f4] 'flycheck-mode)
(global-set-key [f5] 'evil-mode) ; toggle evil-mode
(global-set-key [f6] 'org-toggle-inline-images) ; not globally applicable; should be moved to modes.el
(global-set-key [f7] 'cycle-powerline-separators)
(global-set-key [f8] 'delete-trailing-whitespace)
(global-set-key [f9] 'filesets-open)
(global-set-key [mouse-3] 'imenu)

(provide 'keys)
;;; keys.el ends here
