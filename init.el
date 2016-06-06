(setq emacs-load-start-time (current-time))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; load els
(load "s-ui.el")
(load "s-org.el")
(load "s-code.el")
(load "s-packages.el")
(load "s-keybindings.el")

(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds."
		   (time-to-seconds (time-since emacs-load-start-time))))
