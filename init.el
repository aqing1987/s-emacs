(defvar best-gc-cons-threshold 4000000 "Best default gc threshold value. Should't be too big.")
;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; load els
(let ((file-name-handler-alist nil))
  (require 'init-modeline)
  (require 'cl-lib)
  (require 'init-utils)
  (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
  (load "s-ui.el")
  (load "s-org.el")
  (load "s-code.el")
  (load "s-packages.el")
  (load "s-keybindings.el")

  (when (require 'time-date nil t)
	(message "Emacs startup time: %d seconds."
			 (time-to-seconds (time-since emacs-load-start-time))))
  )
