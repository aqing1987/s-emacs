;; set title
(setq frame-title-format "E = MC^2")

;; close toolbar，tool-bar-mode is a Minor Mode
(tool-bar-mode -1)

;; close scroll bar
(scroll-bar-mode -1)

;; show line num
(global-linum-mode 1)

;; show column number
(setq column-number-mode t)

;; change cursor type
(setq cursor-type 'bar)

;; use y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; disable *~
(setq make-backup-files nil)

;; disable start-up screen
(setq inhibit-splash-screen 1)

;; change font size as 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 160)

;; add recent open records
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

;; enable del selectd zone
(delete-selection-mode 1)

;; highlight current line
(global-hl-line-mode 1)

;; highlight match parenthesis
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
