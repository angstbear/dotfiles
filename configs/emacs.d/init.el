;;; init.el --- Main configuration file for Emacs
;;;
;;; Commentary:
;;;
;;; Code:

;; Package repositories
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(require 'epa-file)
(require 'linum)

;; Load libraries
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(load-library "econf")
(load-library "functions")
(load-library "modes")
(load-library "style")
(load-library "keys")

;; Filesets
(filesets-init)

(provide 'init)
;;; init.el ends here
