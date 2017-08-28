
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar best-gc-cons-threshold 4000000
  "Best default gc threshold value. Should't be too big.")

;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; ----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)

(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(setq *unix*
      (or *linux*
          (eq system-type 'usg-unix-v)
          (eq system-type 'berkeley-unix)))
(setq *emacs24* (and (not (featurep 'xemacs))
                     (or (>= emacs-major-version 24))))
(setq *no-memory*
      (cond
       (*is-a-mac*
        (< (string-to-number
            (nth 1
                 (split-string (shell-command-to-string "sysctl hw.physmem"))))
           4000000000))
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
  ;; any file use flyspell should be initialized after init-spelling
  (require 'init-spelling)
  (require 'init-gui-frames)
  (require 'init-ido)
  (require 'init-dired)
  (require 'init-uniquify)
  (require 'init-ibuffer)
  (require 'init-smex)
  (require 'init-ivy)
  (require 'init-hippie-expand)
  (require 'init-windows)

  (require 'init-whitespace)
  (require 'init-sessions)
  (require 'init-markdown)
  (require 'init-javascript)
  (require 'init-sorg)
  (require 'init-python-mode)
  (require 'init-lisp)
  (require 'init-elisp)
  (require 'init-yasnippet)
  (require 'init-zencoding-mode)
  (require 'google-c-style)
  (require 'init-ecb)
  (require 'init-gdb)
  (require 'init-gtags)
  (require 'init-sh)
  (require 'init-cscope)
  (require 'init-ctags)
  (require 'init-bbdb)
  (require 'init-gnus)
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
  (require 'init-artbollocks-mode)

  (when (require 'time-date nil t)
	(message "Emacs startup time: %d seconds."
			 (time-to-seconds (time-since emacs-load-start-time))))

  )

;; disable emacs making changes, and keep the settings in a separate file
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

(setq gc-cons-threshold best-gc-cons-threshold)
;;; Local Variables:
;;; no-byte-compile: t
;;; End:
(put 'erase-buffer 'disabled nil)
