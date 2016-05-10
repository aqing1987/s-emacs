;;
;; restart emacs.
;;
;; Now, call list-packages.
;; Find the package you want, move cursor to the line press Enter ↵.
;; press Tab ↹ to the “Install” button and press Enter ↵.
;; Then, you can start to use the package.
;;
;; Update Packages
;; To update packages, just press 【U x】.
;; package-menu-mark-upgrades 【U】 → For any package that has a
;; new version, the installed one will be marked “D” (for delete),
;; and new one will be marked “I” (to install).
;; package-menu-execute 【x】 → run the marked commands on packages.
;;
;; Installed Packages Location
;; New packages are installed at ~/.emacs.d/elpa/.
;;

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; cl - Common Lisp Extension
(require 'cl)

;; add whatever packages you want here
(defvar my/packages
  '(
	company
	xcscope
	ecb
	yasnippet
	auto-complete
	tabbar
	;; theme
	monokai-theme
	)  "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
		when (not (package-installed-p pkg)) do (return nil)
		finally (return t)))


(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))


;; enable company mode
(global-company-mode t)

;; === cscope
(require 'xcscope)

;; === cedet
(global-ede-mode t)

;; === ecb
(require 'ecb)
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


;; === yasnippet
(require 'yasnippet)
(yas/global-mode 1)
(yas/minor-mode-on)

;; === auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
			 "~/.emacs.d//ac-dict")
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

;; === tabbar config
(require 'tabbar)
(tabbar-mode)

(defun my-tabbar-buffer-groups ()  
  "Return the list of group names the current buffer belongs to.  
   Return a list of one element based on major mode."  
  (list  
   (cond  
    ((string-equal "*" (substring (buffer-name) 0 1))  
     "Emacs Buffer"  
     )  
    ((eq major-mode 'dired-mode)  
     "Dired"  
     )  
    (t  
     "User Buffer"  
     ))))  

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)  

;; === theme
(load-theme 'monokai t)
