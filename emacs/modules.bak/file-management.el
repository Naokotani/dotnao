(use-package dirvish
 :config
 (setq dirvish-header-line-format
      '(:left (path) :right (free-space))
      dirvish-mode-line-format
      '(:left ("    " file-time " " file-size symlink) :right (omit yank index)))
 (setq dirvish-attributes
		'(vc-state subtree-state all-the-icons collapse git-msg file-size))
 (dirvish-peek-mode)
 (setq dirvish-default-layout '(0 0.4 0.6))
 (setq dirvish-header-line-height '(25 . 25))
 (setq dirvish-mode-line-height '(20 . 20))
 (setq dirvish-reuse-session 'resume)
 (setq dired-hide-details-mode t)
 (setq dired-listing-switches
		"-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
 :init
 (dirvish-override-dired-mode))

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)
(use-package 0x0)
(setq dired-auto-revert-buffer t)
