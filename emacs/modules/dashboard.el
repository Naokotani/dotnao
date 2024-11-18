(use-package page-break-lines)
(setq inhibit-splash-screen t)

;; (setq dashboard-banner-ascii "
;; ███████╗███╗░░░███╗░█████╗░░█████╗░░██████╗
;; ██╔════╝████╗░████║██╔══██╗██╔══██╗██╔════╝
;; █████╗░░██╔████╔██║███████║██║░░╚═╝╚█████╗░
;; ██╔══╝░░██║╚██╔╝██║██╔══██║██║░░██╗░╚═══██╗
;; ███████╗██║░╚═╝░██║██║░░██║╚█████╔╝██████╔╝
;; ╚══════╝╚═╝░░░░░╚═╝╚═╝░░╚═╝░╚════╝░╚═════╝░
;; ")

;;   ;; Simple dashboard that displays recent files and upcoming agenda items
;; (use-package dashboard
;;   :straight t
;;   :config
;;   :custom
;;   (dashboard-set-heading-icons t)
;;   (dashboard-set-file-icons t)
;;   (dashboard-startup-banner 'ascii)
;;   (dashboard-set-footer nil)
;;   (dashboard-banner-logo-title nil))
;; (dashboard-open)

(setq initial-buffer-choice "/home/naokotani/Documents/denote/20230911T100446--todos__personal.org")

(use-package auth-source-pass)
(setq auth-source-pass-file "~/.password-store")
(setq auth-source-creation-default-password-scheme 'default)

(defun nao/irc-connect ()
	(interactive)
	(erc-tls :server "irc.libera.chat"
					 :port 6697
					 :nick   "Naokotani"
					 :full-name "Chris Hughes"
           :password (auth-source-pass-get 'secret "irc/Naokotani")))

(setq erc-fill-column 120
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20)

(setq erc-track-exclude '("#emacs")
      erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY")
      erc-hide-list '("JOIN" "NICK" "QUIT" "MODE" "AWAY")
      erc-track-exclude-server-buffer t)

(setq erc-autojoin-channels-alist '(("Libera.Chat" "#systemcrafters" "#emacs" "#naocoding")))

(add-hook 'erc-mode-hook 
          (lambda ()
            (setq-local corfu-auto nil)))


(use-package elfeed-tube)

(add-hook 'erc-insert-post-hook (lambda ()
                                  (let ((line-spacing 0.5))
                                  (insert ""))))
(setq erc-prompt "> ")
(setq erc-insert-timestamp-left-and-right "%H:%M")
(setq erc-format-message "[%t] %n: %s")
