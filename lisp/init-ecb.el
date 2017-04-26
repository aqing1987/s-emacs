(require-package 'ecb)

(require 'ecb)
(setq ecb-tip-of-the-day nil)

;; switch between windows
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; open and close
(global-set-key [f12] 'ecb-activate)
(global-set-key [M-f12] 'ecb-deactivate)

(provide 'init-ecb)