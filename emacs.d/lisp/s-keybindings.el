;; goto line
(global-set-key [(control t)] 'goto-line)

;; open init file quickly
(defun open-init-file()
  (interactive)
  (find-file "~/workspace/open-src/s-emacs/emacs.d/init.el"))

;; map function open-init-file() to key <f2>
(global-set-key (kbd "<f2>") 'open-init-file)
