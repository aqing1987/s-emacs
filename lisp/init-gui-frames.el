;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;;----------------------------------------------------------------------------
;; self defined
;;----------------------------------------------------------------------------

;; set title
(setq frame-title-format "E = MC^2")

;; use y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; disable *~
(setq make-backup-files nil)

;; change font size as 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 160)

;; add recent open records
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

;; enable del selected zone
(delete-selection-mode 1)

;; highlight current line
(global-hl-line-mode 1)

;; highlight match parenthesis
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)


;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)

;; NO tool bar or scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

(provide 'init-gui-frames)
