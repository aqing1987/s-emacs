(require 'org)
;; === load org latex config
;; refs https://github.com/tumashu/ox-latex-chinese
(require 'ox-latex-chinese)
(oxlc/toggle-ox-latex-chinese t)

;; to show .png normally
(setq org-latex-create-formula-image-program 'imagemagick) ;支持中文

(setq org-src-fontify-natively t)


;; === org agenda config
;; C-c C-s - select when to start
;; C-c C-d - select when to finish
;; C-c a - open agenda mode and select view mode(r)

;; set agenda directory
(setq org-agenda-files '("~/org"))

;; set org-agenda 
(global-set-key (kbd "C-c a") 'org-agenda)

