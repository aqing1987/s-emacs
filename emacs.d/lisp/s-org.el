(require 'org)
(setq org-src-fontify-natively t)


;; === org agenda config
;; C-c C-s - select when to start
;; C-c C-d - select when to finish
;; C-c a - open agenda mode and select view mode(r)

;; set agenda directory
(setq org-agenda-files '("~/org"))

;; set org-agenda 
(global-set-key (kbd "C-c a") 'org-agenda)
