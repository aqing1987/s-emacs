;; tabbar 

(add-to-list 'load-path "~/.emacs.d/elpa/tabbar-20141109.143/")
(require 'tabbar)
(tabbar-mode)

(defun my-tabbar-buffer-groups ()  
  "Return the list of group names the current buffer belongs to.  
   Return a list of one element based on major mode."  
  (list  
   (cond  
    ((string-equal "*" (substring (buffer-name) 0 1))  
     "Emacs Buffer"  
     )  
    ((eq major-mode 'dired-mode)  
     "Dired"  
     )  
    (t  
     "User Buffer"  
     ))))  
  
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)  
