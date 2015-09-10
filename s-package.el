;;
;; restart emacs.
;;
;; Now, call list-packages.
;; Find the package you want, move cursor to the line press Enter ↵.
;; press Tab ↹ to the “Install” button and press Enter ↵.
;; Then, you can start to use the package.
;;
;; Update Packages
;; To update packages, just press 【U x】.
;; package-menu-mark-upgrades 【U】 → For any package that has a
;; new version, the installed one will be marked “D” (for delete),
;; and new one will be marked “I” (to install).
;; package-menu-execute 【x】 → run the marked commands on packages.
;;
;; Installed Packages Location
;; New packages are installed at ~/.emacs.d/elpa/.
;; 
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))
