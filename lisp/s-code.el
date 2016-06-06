(require 'cc-mode)
(require 'semantic)

;;=== indent config
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)

(setq indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)


(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 2)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 0)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    )
  "rex Programming Style")

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c-style t)
  ;; other customizations
  (setq tab-width 4 indent-tabs-mode nil)
  (define-key c-mode-base-map [(f7)] 'compile)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

