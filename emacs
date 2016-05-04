
;; =============================================================
;; === org mode
;; =============================================================
(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/contrib/lisp")
(require 'org-install)
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook
		  (lambda () (setq truncate-lines nil)))
 
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

;; pdf output
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))

;; font config like this in case run with "emacs -nw" will produce
;; fontset 'tty' does not exist bug.
(defun s-font()
  (interactive)
  ;; font config for org table showing.
  (set-default-font "monospace-11")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font (frame-parameter nil 'font)
					  charset
					  (font-spec :family "WenQuanYi Micro Hei")))
  ;; tune rescale so that Chinese character width = 2 * English character width
  (setq face-font-rescale-alist '(("monospace" . 1.0) ("WenQuanYi" . 1.23))))

(add-to-list 'after-make-frame-functions
			 (lambda (new-frame)
			   (select-frame new-frame)
			   (if window-system
				   (s-font))))
(if window-system
	(s-font))

;; =============================================================
;; === php mode
;; =============================================================
(add-to-list 'load-path "~/.emacs.d/lisp/php-mode")
(require' php-mode)

;; =============================================================
;; === cscope
;; =============================================================
(require 'xcscope)

;; 打开cscope时不更新，提高索引速度
;(setq cscope-do-not-update-database t)


;; =============================================================
;; === cedet
;; =============================================================
(require 'cedet)
(require 'semantic)

;; Enable EDE (Project Management) features
(global-ede-mode t)

;; config semantic searching zone
(setq semanticdb-project-roots
      (list
       (expand-file-name "/home/re-x/workspace")))


;; =============================================================
;; === ecb
;; =============================================================
(add-to-list 'load-path "~/.emacs.d/lisp/ecb")
(require 'ecb)

;(require 'ecb-autoloads)
(setq stack-trace-on-error nil)
(setq ecb-tip-of-the-day nil)

;; switch between windows
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; open and close
(global-set-key [f12] 'ecb-activate)
(global-set-key [M-f12] 'ecb-deactivate)

;; =============================================================
;; === yasnippet (put before auto-complete)
;; =============================================================
(add-to-list 'load-path "~/.emacs.d/lisp/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)
(yas/minor-mode-on) 

;; =============================================================
;; === auto-complete
;; =============================================================
(add-to-list 'load-path "~/.emacs.d/lisp/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
			 "~/.emacs.d/lisp/auto-complete//ac-dict")
(ac-config-default)

(setq ac-use-quick-help nil)
(setq ac-auto-start 4)
(global-set-key "\M-/" 'auto-complete)
;; show menu 0.8 seconds later
(setq ac-auto-show-menu 0.8)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(setq ac-menu-height 12)

;; ;; =============================================================
;; ;; === common lisp process
;; ;; =============================================================
;; ;; load slime for common lisp life
;; (require 'slime-autoloads)
;; (setq inferior-lisp-program "clisp")

;; ;; Automatically use slime mode on .cl and .lisp files.
;; (add-to-list 'auto-mode-alist '("\\.cl$" . common-lisp-mode))
;; (add-to-list 'auto-mode-alist '("\\.lisp$" . common-lisp-mode))

;; ;; key binding for slime
;; (global-set-key "\C-c \C-c" 'slime-compile-defun)


;; (global-set-key [(meta ?/)] 'hippie-expand)
;; (setq hippie-expand-try-functions-list
;;       '(try-expand-dabbrev
;;         try-expand-dabbrev-visible
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         try-complete-file-name-partially
;;         try-complete-file-name
;;         try-expand-all-abbrevs
;;         try-expand-list
;;         try-expand-line
;;         try-complete-lisp-symbol-partially
;;         try-complete-lisp-symbol))

;; =============================================================
;; === Input method config (Use C-\ to toggle input method)
;; =============================================================

;; EIM Input method. 
;; Chinese input method config
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-eim/")
(require 'eim-extra)
(autoload 'eim-use-package "eim" "Another emacs input method")
(setq eim-use-tooltip nil)

(global-set-key "`" 'eim-insert-ascii)

(add-hook 'eim-wb-load-hook
          (lambda ()
            (let ((map (eim-mode-map)))
              (define-key map "-" 'eim-previous-page)
              (define-key map "=" 'eim-next-page))))

(register-input-method "eim-wb" "euc-cn" 'eim-use-package
		       "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method "eim-py" "euc-cn" 'eim-use-package
		       "拼音" "汉字拼音输入法" "py.txt")

(setq activate-input-method nil)
(setq default-input-method "eim-wb")


;; =============================================================
;; === UI config
;; =============================================================

;; buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


(setq default-line-spaceing 4)
(setq default-fill-column 80)


(set-mouse-color "white")
(setq-default cursor-type 'bar)


(show-paren-mode t)
(setq show-paren-style 'parentheses)

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(set-foreground-color "grey")
(set-background-color "black")
(set-cursor-color "gold1")

(set-face-foreground 'highlight "white")
(set-face-background 'highlight "blue")
(set-face-foreground 'region "cyan")
(set-face-background 'region "blue")
(set-face-foreground 'secondary-selection "skyblue")
(set-face-background 'secondary-selection "darkblue")

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(menu-bar-mode -1)

(setq column-number-mode t)

(setq visible-bell t)
(setq inhibit-startup-message t)

;; backup files control
(setq make-backup-files nil)
(setq-default make-backup-files nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(ecb-options-version "2.40")
 '(show-paren-mode t)
 '(tool-bar-mode nil))
