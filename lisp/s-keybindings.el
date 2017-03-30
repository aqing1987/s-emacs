;; goto line
(global-set-key [(control t)] 'goto-line)

;; open init file quickly
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; map function open-init-file() to key <f2>
(global-set-key (kbd "<f2>") 'open-init-file)

(define-key global-map [(control f4)]  'hexl-mode)

;; === buffer switch
(define-key global-map [(f3)] 'switch-to-prev-buffer)
(define-key global-map [(f4)] 'switch-to-next-buffer)

;; === ecb
;; switch between windows
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; open and close
(global-set-key [f12] 'ecb-activate)
(global-set-key [M-f12] 'ecb-deactivate)


;; === magit keys
(global-set-key (kbd "C-x g") 'magit-status)
