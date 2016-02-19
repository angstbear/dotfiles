
;; JSON - Use python to format readible
;;
;; Currently useing json-reformat elisp package
;; Highlight region, then M-x json-reformat-region
;;
;(defun json-format ()
;  (interactive)
;  (save-excursion
;    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))
