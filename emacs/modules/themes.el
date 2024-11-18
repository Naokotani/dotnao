(use-package modus-themes)

;; Add all your customizations prior to loading the themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t)

(modus-themes-select 'modus-vivendi-tinted)

(define-key global-map (kbd "<f11>") #'modus-themes-toggle)

;;Install all the icons for integration into doom modeline and Dirvish
(use-package all-the-icons
  :straight t)

;; Transparency
(set-frame-parameter nil 'alpha-background 100)
(defvar nao/alpha-background 100)

(defun nao/toggle-alpha-background ()
  "Toggle alpha-background between 90 and 100."
  (interactive)
	(cond
   ((= nao/alpha-background 70) (setq nao/alpha-background 80))
				((= nao/alpha-background 80) (setq nao/alpha-background 90))
				((= nao/alpha-background 90) (setq nao/alpha-background 100))
				((= nao/alpha-background 100) (setq nao/alpha-background 50))
				((= nao/alpha-background 50) (setq nao/alpha-background 70)))
  (set-frame-parameter nil 'alpha-background nao/alpha-background))

;; Modeline setup
(setq mode-line-format nil)

(kill-local-variable 'mode-line-format)

(force-mode-line-update)

(setq-default mode-line-format
              '("%e"
                my-modeline-buffer-name
                "  "
                my-modeline-major-mode))

(defface my-modeline-background
  '((t :foreground "#7fafff" :inherit bold))
  "Face with a red background for use on the mode line.")

(defface my-modeline-background-modified
  '((t :foreground "#c48702" :inherit bold))
  "Face with a red background for use on the mode line.")

(defun my-modeline--buffer-name ()
  "Return `buffer-name' with spaces around it."
  (format " %s " (buffer-name)))

(defvar-local my-modeline-buffer-name
    '(:eval
        (if (buffer-modified-p) 
            (propertize (my-modeline--buffer-name) 'face 'my-modeline-background-modified)
          (propertize (my-modeline--buffer-name) 'face 'my-modeline-background)))
  "Mode line construct to display the buffer name.")

(put 'my-modeline-buffer-name 'risky-local-variable t)

(defun my-modeline--major-mode-name ()
  "Return capitalized `major-mode' as a string."
  (capitalize (symbol-name major-mode)))

(defvar-local my-modeline-major-mode
    '(:eval
      (when (mode-line-window-selected-p)
       (if (eglot-managed-p)
           (propertize (my-modeline--major-mode-name) 'face 'all-the-icons-green)
           (propertize (my-modeline--major-mode-name) 'face 'all-the-icons-cyan))))
  "Mode line construct to display the major mode.")

(put 'my-modeline-major-mode 'risky-local-variable t)

(mode-line-window-selected-p)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [0] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [0] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
(setq git-gutter-fr:side 'right-fringe)
(fringe-mode '(15 . 3))

;;Use this to change the fringe for and background color
(set-face-attribute 'fringe nil :background "#0d0e1c" :foreground "ffffff")

(winner-mode)

(use-package spacious-padding)

;;These is the default value, but I keep it here for visiibility.
(setq spacious-padding-widths
      '( :internal-border-width 10
         :header-line-width 0
         :tab-width 0
         :left-fringe-width 30
         :mode-line-width 10
         :right-divider-width 10
         :scroll-bar-width 0
         :fringe-width 20))

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
(setq spacious-padding-subtle-mode-line
      `( :mode-line-active 'default
         :mode-line-inactive vertical-border))

(spacious-padding-mode 1)
