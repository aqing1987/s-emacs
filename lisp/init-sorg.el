(require 'org)
;; === load org latex config
;; refs https://github.com/tumashu/ox-latex-chinese
(require 'ox-latex-chinese)
(oxlc/toggle-ox-latex-chinese t)

;; to show .png normally
(setq org-latex-create-formula-image-program 'imagemagick)

(setq org-src-fontify-natively t)


;; === org agenda config
;; C-c C-s - select when to start
;; C-c C-d - select when to finish
;; C-c a - open agenda mode and select view mode(r)

;; set agenda directory
(setq org-agenda-files '("~/org"))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-x g") 'magit-status)

;; disable `validate'
(setq org-html-validation-link nil)

;; disable creator info
(setq org-export-with-creator nil)

;; disable author info
(setq org-export-with-author nil)

;; disable time stamp
(setq org-export-time-stamp-file nil)

;; preserve line breaks when export
(setq org-export-preserve-breaks t)

;; set the table of contents level limit
(setq org-export-with-toc 2)

;; === screenshot in org-mode
;; require: scrot installed
(defun s-screenshot()
  "take a screenshot into a unique-named file in the current buffer file
   directory and insert a link to this file."
  (interactive)
  (setq filename
		(concat(make-temp-name
				(concat(getenv "HOME") "/Pictures/"))".png"))
  (suspend-frame)
  (call-process-shell-command "scrot" nil nil nil nil "-s" (concat
															"\""filename"\""))
  (insert (concat "[[" filename "]]"))
  ;(org-display-inline-images)
  )

(global-set-key (kbd "M-p") 's-screenshot)

(provide 'init-sorg)