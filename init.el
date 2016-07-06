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
  (require 'init-sorg)
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
  (require 'init-cscope)
  (require 'init-ctags)
  (require 'init-bbdb)
  (require 'init-gnus)
  (require 'init-lua-mode)
  (require 'init-workgroups2)
  (require 'init-term-mode)
  (require 'init-web-mode)
  (require 'init-slime)
  (require 'init-clipboard)
  (require 'init-company)
  (require 'init-keyfreq)
  (require 'init-httpd)

  ;; misc has some crucial tools I need immediately
  (require 'init-misc)

  ;; setup color theme
  ;; comment below line if you want to setup color theme in your own way
  (if (or (display-graphic-p)
          (string-match-p "256color"(getenv "TERM")))
      (load-theme 'monokai t))

  (require 'init-emacs-w3m)
  (require 'init-hydra)

  (require 'init-misc-lazy)
  (require 'init-which-func)
  (require 'init-fonts)
  (require 'init-hs-minor-mode)
  (require 'init-textile)
  (require 'init-csv)
  (require 'init-writting)
  (require 'init-doxygen)
  (require 'init-pomodoro)
  (require 'init-emacspeak)
  (require 'init-artbollocks-mode)
  
  ;; (load "s-code.el")
  ;; (load "s-packages.el")
  (load "s-keybindings.el")

  (when (require 'time-date nil t)
	(message "Emacs startup time: %d seconds."
			 (time-to-seconds (time-since emacs-load-start-time))))

  ;;----------------------------------------------------------------------------
  ;; Locales (setting them earlier in this file doesn't work in X)
  ;;----------------------------------------------------------------------------
  (require 'init-locales)

  ;; my personal setup, other major-mode specific setup need it.
  ;; It's dependent on init-site-lisp.el
  (if (file-exists-p "~/.custom.el") (load-file "~/.custom.el"))
  )

(setq gc-cons-threshold best-gc-cons-threshold)
;;; Local Variables:
;;; no-byte-compile: t
;;; End:
(put 'erase-buffer 'disabled nil)
