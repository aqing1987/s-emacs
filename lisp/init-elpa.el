(require 'package)

;; List of VISIBLE packages from melpa-unstable (http://melpa.org)
;; Feel free to add more packages!
(defvar melpa-include-packages
  '(bbdb
    color-theme
    ivy
    counsel
    swiper
    wgrep
    robe
    groovy-mode
    inf-ruby
    simple-httpd
    dsvn
    move-text
    string-edit ; looks magnars don't update stable tag frequently
    findr
    mwe-log-commands
    yaml-mode
    noflet
    db
    creole
    web
    idomenu
    pointback
    buffer-move
    regex-tool
    quack
    legalese
    htmlize
    scratch
    session
    crontab-mode
    bookmark+
    flymake-lua
    multi-term
    dired+
    inflections
    dropdown-list
    lua-mode
    tidy
    pomodoro
    auto-compile
    packed
    gitconfig-mode
    textile-mode
    w3m
    company
    company-statistics
    xcscope
    ecb
    yasnippet
    auto-complete
    tabbar
    async
    dash
    with-editor
    git-commit
    magit-popup
    magit
    php-mode
    company-c-headers
    smex
    keyfreq
    browse-kill-ring
    ;; make all the color theme packages available
    afternoon-theme
    monokai-theme
    heroku-theme)
  "Don't install any Melpa packages except these packages")

(setq package-archives
      '(
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ))

;;------------------------------------------------------------------------------
;; Internal implementation, newbies should NOT touch code below this line!
;;------------------------------------------------------------------------------

;; Patch up annoying package.el quirks
(defadvice package-generate-autoloads
    (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat
                                 ;; name is string when emacs <= 24.3.1,
                                 (if (symbolp name) (symbol-name name) name)
                                 "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))

;; Add support to package.el for pre-filtering available packages
(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used
by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
	(around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function',
if non-nil."
  (when (or (null package-filter-function)
			(funcall package-filter-function
					 (car package)
					 (funcall (if (fboundp 'package-desc-version)
								  'package--ac-desc-version
								'package-desc-vers)
							  (cdr package))
					 archive))
    ad-do-it))

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If No-REFRESH is no-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))

;; Don't take Melpa versions of certain packages
(setq package-filter-function
      (lambda (package version archive)
        (and
         (not (memq package '(eieio)))
         (or (and (string-equal archive "melpa")
                  (memq package melpa-include-packages))
             (not (string-equal archive "melpa")))
         )))

;;------------------------------------------------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;------------------------------------------------------------------------------

(package-initialize)

(require-package 'dash) ; required by string-edit
(require-package 'color-theme) ; color-theme 6.6.1 in elpa is buggy
(require-package 'monokai-theme)
(require-package 'auto-compile)
(require-package 'avy)
(require-package 'expand-region) ;; I prefer stable version
(require-package 'fringe-helper)
(require-package 'haskell-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'yagist)
(require-package 'wgrep)
(require-package 'request) ; http post/get tool
(require-package 'lua-mode)
(require-package 'robe)
(require-package 'yaml-mode)
(require-package 'paredit)
(require-package 'erlang)
(require-package 'findr)
(require-package 'jump)
(require-package 'nvm)
(require-package 'writeroom-mode)
(require-package 'haml-mode)
(require-package 'scss-mode)
(require-package 'markdown-mode)
(require-package 'dired+)
(require-package 'link)
(require-package 'connection)
(require-package 'dictionary) ; dictionary requires 'link and 'connection
(require-package 'htmlize)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'rainbow-delimiters)
(require-package 'textile-mode)
(require-package 'coffee-mode)
(require-package 'flymake-coffee)
(require-package 'crontab-mode)
(require-package 'dsvn)
(require-package 'git-timemachine)
(require-package 'exec-path-from-shell)
(require-package 'flymake-css)
(require-package 'flymake-jslint)
(require-package 'flymake-ruby)
(require-package 'swiper)
(require-package 'find-file-in-project)
(require-package 'elpy)
(require-package 'hl-sexp)
(require-package 'ibuffer-vc)
(require-package 'less-css-mode)
(require-package 'move-text)
(require-package 'mwe-log-commands)
(require-package 'page-break-lines)
(require-package 'pointback)
(require-package 'regex-tool)
(require-package 'rinari)
(require-package 'groovy-mode)
(require-package 'ruby-compilation)
(require-package 'emmet-mode)
(require-package 'session)
(require-package 'tidy)
(require-package 'unfill)
(require-package 'w3m)
(require-package 'idomenu)
(require-package 'ggtags)
(require-package 'buffer-move)
(require-package 'ace-window)
(require-package 'cmake-mode)
(require-package 'cpputils-cmake)
(require-package 'flyspell-lazy)
(require-package 'bbdb)
(require-package 'pomodoro)
(require-package 'flymake-lua)
(require-package 'dropdown-list)
;; rvm-open-gem to get gem's code
(require-package 'rvm)
;; C-x r l to list bookmarks
(require-package 'bookmark+)
(require-package 'multi-term)
(require-package 'js2-mode)
(require-package 's)
;; js2-refactor requires js2, dash, s, multiple-cursors, yasnippet
;; I don't use multiple-cursors, but js2-refactor requires it
(require-package 'multiple-cursors)
(require-package 'tagedit)
(require-package 'git-link)
(require-package 'cliphist)
(require-package 'yasnippet)
(require-package 'company-statistics)
(require-package 'company)
(require-package 'company-c-headers)
(require-package 'legalese)
(require-package 'simple-httpd)
(require-package 'git-messenger)
(require-package 'git-gutter)
(require-package 'async)
(require-package 'with-editor)
(require-package 'git-commit)
(require-package 'magit-popup)
(require-package 'magit)
(require-package 'flx-ido)
(require-package 'neotree)
(require-package 'define-word)
(require-package 'quack) ;; for scheme
(require-package 'hydra)
(require-package 'php-mode)
(require-package 'xcscope)
(require-package 'tabbar)
(require-package 'smex)
(require-package 'keyfreq)
(require-package 'browse-kill-ring)

(provide 'init-elpa)
