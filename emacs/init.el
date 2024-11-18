;; Set warning level
;;(setq warning-minimum-level :emergency)
(setq debug-on-error t)

;; Straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
	  (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Tell straight.el to use use-package expressions
(setq straight-use-package-by-default t)

;;; Load Modules
(require 'find-lisp)

;; First load modules that are required as dependencies, evil mode in this case for keybinds
(mapcar (lambda (fn)
		  (load (file-name-sans-extension fn)))
		(find-lisp-find-files "~/.config/emacs/early-modules" "\\.el\\'"))

(mapcar (lambda (fn)
		  (load (file-name-sans-extension fn)))
		(find-lisp-find-files "~/.config/emacs/org-mode" "\\.el\\'"))

;;Then load the rest
(mapcar (lambda (fn)
		  (load (file-name-sans-extension fn)))
		(find-lisp-find-files "~/.config/emacs/modules" "\\.el\\'"))

(require 'cl-lib)

;;disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
