;;; init.el --- Main configuration file for Emacs
;-*-Emacs-Lisp-*-

;;; Commentary:
;;;
;;; To load this init in .emacs file
;;; (load-file "~/dotfiles/configs/emacs.d/init.el")
;;;
;;; Code:

;; Package repositories
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Brew-installed Emacs Lisp files
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Filesets
(filesets-init)

(require 'epa-file)
(require 'linum)

;; Load libraries
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(load-library "econf")
(load-library "functions")
(load-library "modes")
(load-library "style")
(load-library "keys")

;; Run shell
;;(shell)

(provide 'init)
;;; init.el ends here
