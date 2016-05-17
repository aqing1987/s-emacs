;; goto line
(global-set-key [(control t)] 'goto-line)

;; open init file quickly
(defun open-init-file()
  (interactive)
  (find-file "~/workspace/open-src/s-emacs/emacs.d/init.el"))

;; map function open-init-file() to key <f2>
(global-set-key (kbd "<f2>") 'open-init-file)

;; === cscope
(define-key global-map [(control f5)]  'cscope-set-initial-directory)
(define-key global-map [(control f6)]  'cscope-unset-initial-directory)

(define-key global-map [(control f7)]  'cscope-find-this-symbol)
(define-key global-map [(control f8)]  'cscope-find-global-definition)

(define-key global-map [(control f9)]  'cscope-history-forward-line)
(define-key global-map [(control f10)] 'cscope-history-backward-line)

(define-key global-map [(control f11)] 'cscope-history-forward-file)
(define-key global-map [(control f12)] 'cscope-history-backward-file)

(define-key global-map [(meta f9)]  'cscope-display-buffer)

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
