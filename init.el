(defvar best-gc-cons-threshold 4000000 "Best default gc threshold value. Should't be too big.")
;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))) )
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                   (*linux* nil)
                   (t nil)))

;; load els
(let ((file-name-handler-alist nil))
  (require 'init-modeline)
  (require 'cl-lib)
  (require 'init-utils)
  ;; Must come before elpa, as it may provide package.el
  (require 'init-site-lisp) 
  (require 'init-elpa)
  (require 'init-exec-path) ;; Set up $PATH
  (require 'init-frame-hooks)
  ;; any file use flyspell should be initialized after init-spelling
  (require 'init-spelling)
  (require 'init-xterm)
  (require 'init-gui-frames)
  (require 'init-ido)
  (require 'init-dired)
  (require 'init-uniquify)
  (require 'init-ibuffer)
  (require 'init-flymake)
  (require 'init-smex)
  (require 'init-ivy)
  (require 'init-hippie-expand)
  (require 'init-windows)
  (require 'init-sessions)
  (require 'init-markdown)
  (require 'init-javascript)
  (require 'init-org)
  (require 'init-org-mime)
  (require 'init-css)
  (require 'init-python-mode)
  (require 'init-lisp)
  (require 'init-elisp)
  (require 'init-yasnippet)
  (require 'init-zencoding-mode)
  (require 'init-cc-mode)
  (require 'init-gud)
  (require 'init-gtags)
  (require 'init-sh)
  (require 'init-ctags)
  (require 'init-bbdb)
  (require 'init-gnus)
  (require 'init-lua-mode)
  (require 'init-workgroups2)
  (require 'init-term-mode)
  (require 'init-web-mode)
  ;(load "s-org.el")
  ;(load "s-code.el")
  ;; (load "s-packages.el")
  (load "s-keybindings.el")

  ;; setup color theme
  (load-theme 'monokai t)

  (when (require 'time-date nil t)
	(message "Emacs startup time: %d seconds."
			 (time-to-seconds (time-since emacs-load-start-time))))
  )
