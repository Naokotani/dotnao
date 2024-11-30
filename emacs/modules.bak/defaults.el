(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)          ; Disable the menu bar

;; Puts Emacs customize code in seperate file. 
(setq custom-file (locate-user-emacs-file "custom-vars.el")) 
(load custom-file 'noerro 'nomessage)

(setq global-auto-revert-mode 1)  ; Revert buffers when the file changes on disc
(setq global-auto-revert-non-file-buffers 1) ; Reverts dired buffers when directories change on disc

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
		prog-mode-hook
		conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))
