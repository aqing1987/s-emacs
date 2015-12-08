;; screenshot in org-mode
(defun s-org-capture()
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
  (org-display-inline-images)
  )

;(global-set-key (kbd "C-o") 'my-screenshot)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; disable `validate'
(setq org-html-validation-link nil)

;; disable creator info
(setq org-export-with-creator nil)

;; disable author info
(setq org-export-with-author nil)

;; disable time stamp
(setq org-export-time-stamp-file nil)
