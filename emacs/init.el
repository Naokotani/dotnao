;; Set warning level
;;(setq warning-minimum-level :emergency)
(setq debug-on-error t)

(require 'find-lisp)
(require 'cl-lib)

;;disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Exec path from shell
(use-package exec-path-from-shell
  :vc (:url "https://github.com/purcell/exec-path-from-shell"))
(exec-path-from-shell-initialize)


(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)          ; Disable the menu bar
(setq tab-bar-show 3)

;; File management defaults
(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)

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

;; Org mode

;; Ensure Org Mode is loaded
(use-package org
  :bind
  (("C-c a" . org-agenda)))

(setq org-agenda-span 14)

(setq org-hide-emphasis-markers t)
(setq org-startup-with-inline-images t)

(setq org-structure-template-alist
      '(("el" . "src emacs-lisp")
		("js" . "src javascript")
		("css" . "src css")
		("sql" . "src sql")
		("pu" . "src plantuml :file")
		("rs" . "src rust")
		("c" . "src c")
		("c++" . "src c++")
		("java" . "src java")
		("html" . "src html")))

(setq org-src-block-faces 
	  '(("emacs-lisp" modus-themes-nuanced-magenta)
		("elisp" modus-themes-nuanced-magenta)
		("clojure" modus-themes-nuanced-magenta)
		("clojurescript" modus-themes-nuanced-magenta)
		("c" modus-themes-nuanced-blue)
		("c++" modus-themes-nuanced-blue)
		("sh" modus-themes-nuanced-green)
		("sql" modus-themes-nuanced-green)
		("html" modus-themes-nuanced-yellow)
		("xml" modus-themes-nuanced-yellow)
		("css" modus-themes-nuanced-red)
		("scss" modus-themes-nuanced-red)
		("python" modus-themes-nuanced-green)
		("ipython" modus-themes-nuanced-magenta)
		("javascript" modus-themes-nuanced-cyan)
		("yaml" modus-themes-nuanced-cyan)
		("conf" modus-themes-nuanced-cyan)
		("docker" modus-themes-nuanced-cyan)))

(defun nao/org-mode-setup ()
        (org-indent-mode)
        ;; Allow variable pitch faces
        (variable-pitch-mode 1)
        ;; Enables line wrapping
        (visual-line-mode 1))

(setq org-agenda-files "~/Documents/denote/agenda")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/denote/20230911T100446--todos__personal.org" "Unsorted")
         "* TODO %?\n  %i\n  %a")))

 (define-key global-map (kbd "C-c n t")
  (lambda () (interactive) (org-capture nil "t")))


(setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
				 (todo . " %i")
				 (tags . " %i")
				 (search . " %i")))

(setq org-todo-repeat-to-state "ONGOING")
(setq org-log-repeat nil)
;; Gives a list of keywords for org agenda. The bottom list is used for a custom org agenda workflow buffer
(setq org-todo-keywords
      '((sequence "ACTIVE(a)" "NEXT(n)" "PLAN(p)" "TODO(t)"
		  "ONGOING(o)" "WAIT(w@/!)" "|" "READY(r)" "DONE(d!)" "CANC(k@)")))

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(
	;; View only taks marked ACTIVE and NEXT
	("n" "Priority Tasks"
	 ((todo "ACTIVE"
		((org-agenda-overriding-header "Active Project")
		 (org-agenda-files org-agenda-files)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")
		 (org-agenda-todo-list-sublevels nil)
		 (org-agenda-files org-agenda-files)))))
	;; Setup buffer to display workflow
	("w" "Workflow Status"
	 ((todo "ACTIVE"
		((org-agenda-overriding-header "Active Project")
		 (org-agenda-files org-agenda-files)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Prority Tasks")
		 (org-agenda-todo-list-sublevels nil)
		 (org-agenda-files org-agenda-files)))
	  (todo "READY"
		((org-agenda-overriding-header "Ready for final submission")
		 (org-agenda-files org-agenda-files)))
	  (todo "PLAN"
		((org-agenda-overriding-header "In Planning")
		 (org-agenda-todo-list-sublevels nil)
		 (org-agenda-files org-agenda-files)))
	  (todo "WAIT"
		((org-agenda-overriding-header "Waiting on External")
		 (org-agenda-files org-agenda-files)))))))

;; Set Faces
(defun nao/org-font-setup ()
  (require 'org-faces)
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;;Org Bullets provides nicer bullets for Org Mode headers and lists
(use-package org-bullets
  :vc (:url https://github.com/sabof/org-bullets)
  :custom
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;; Visual Fill Column provides margins for buffers
(use-package visual-fill-column
  :vc (:url "https://codeberg.org/joostkremers/visual-fill-column"))

(defun nao/org-mode-visual-fill ()
        (setq visual-fill-column-width 120
                                visual-fill-column-center-text t)
        (visual-fill-column-mode 1))

;; Org-mode hooks
(add-hook 'org-mode-hook #'nao/org-mode-setup)
(add-hook 'org-mode-hook #'nao/org-font-setup)
(add-hook 'org-mode-hook #'org-bullets-mode)
(add-hook 'org-mode-hook #'nao/org-mode-visual-fill) ; Display margins in Org buffers

(use-package denote
  :vc (:url "https://github.com/protesilaos/denote")
  :config
  (setq denote-known-keywords '("school" "work" "personal"))
  (setq denote-infer-keywords t)
  (setq denote-directory (expand-file-name "~/Documents/denote/"))
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)
  :bind
  (("C-c n c" . denote)
   ("C-c n o" . denote-open-or-create)
   ("C-c n l" . denote-link)))

(add-to-list 'load-path "~/.config/emacs/lisp")
(eval-after-load 'ox '(require 'ox-koma-letter))
(eval-after-load 'ox-koma-letter
  '(progn
     (add-to-list 'org-latex-classes
                  '("my-letter"
                    "\\documentclass\{scrlttr2\}
     \\usepackage[english]{babel}
     \\setkomavar{frombank}{(1234)\\,567\\,890}
     \[DEFAULT-PACKAGES]
     \[PACKAGES]
     \[EXTRA]"))
     (setq org-koma-letter-default-class "my-letter")))
(eval-after-load 'ox-latex
  '(add-to-list 'org-latex-packages-alist '("AUTO" "babel" t) t))

;; (use-package plantuml-mode
;;   :vc (:url "https://github.com/skuro/plantuml-mode"))

;; (setq org-plantuml-jar-path "/home/naokotani/src/plantuml.jar")
;; (setq plantuml-jar-path "/home/naokotani/src/plantuml.jar")
;; ;; TODO something wrong with this, says void. Also move into the use-package
;; ;;(add-to-list 'org-src-lang-mode '("plantuml" . plantuml))
;; (setq plantuml-default-exec-mode 'jar)
;; (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
;; (setq org-src-fontify-natively t
;;       org-src-window-setup 'current-window ;; edit in current window
;;       org-src-strip-leading-and-trailing-blank-lines t
;;       org-src-preserve-indentation t ;; do not put two spaces on the left
;;       org-src-tab-acts-natively t)

;; Which Key
(which-key-mode)

;; Vertico
(use-package vertico
  :vc (:url "https://github.com/minad/vertico")
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
	(cons (format "[CRM%s] %s"
				  (replace-regexp-in-string
				   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
				   crm-separator)
				  (car args))
		  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
		'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq read-extended-command-predicate
	#'command-completion-default-include-p)
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :vc (:url "https://github.com/oantolin/orderless")
  :init
  (setq completion-styles '(orderless basic)
		completion-category-defaults nil
		completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :vc (:url "https://github.com/minad/corfu")
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_CA")
(setq ispell-alternate-dictionary "/usr/share/hunspell/en_CA-large.dic")

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package orderless
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package emacs
  :init
  (setq completion-cycle-threshold 2)

  (setq tab-always-indent 'complete))

(use-package marginalia
  :vc (:url "https://github.com/minad/marginalia")
  :bind (("M-A" . marginalia-cycle)
		 :map minibuffer-local-map
		 ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :vc (:url "https://github.com/minad/consult")
  :bind (;; C-c bindings (mode-specific-map)
		 ("C-c h" . consult-history)
		 ("C-c m" . consult-mode-command)
		 ("C-c k" . consult-kmacro)
		 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
		 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
		 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
		 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
		 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
		 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
		 ("M-#" . consult-register-load)
		 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
		 ("C-M-#" . consult-register)
		 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
		 ("<help> a" . consult-apropos)            ;; orig. apropos-command
		 ("M-g e" . consult-compile-error)
		 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
		 ("M-g g" . consult-goto-line)             ;; orig. goto-line
		 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
		 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
		 ("M-g m" . consult-mark)
		 ("M-g k" . consult-global-mark)
		 ("M-g i" . consult-imenu)
		 ("M-g I" . consult-imenu-multi)
		 ("M-s d" . consult-find)
		 ("M-s D" . consult-locate)
		 ("M-s g" . consult-grep)
		 ("M-s G" . consult-git-grep)
		 ("M-s r" . consult-ripgrep)
		 ("M-s l" . consult-line)
		 ("M-s L" . consult-line-multi)
		 ("M-s m" . consult-multi-occur)
		 ("M-s k" . consult-keep-lines)
		 ("M-s u" . consult-focus-lines)
		 ("M-s e" . consult-isearch-history)
		 :map isearch-mode-map
		 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
		 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
		 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
		 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
		 :map minibuffer-local-map
		 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
		 ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
		register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package modus-themes
  :vc (:url "https://github.com/protesilaos/modus-themes"))

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t)

(modus-themes-select 'modus-vivendi-tinted)

(define-key global-map (kbd "<f11>") #'modus-themes-toggle)
(use-package all-the-icons
  :vc (:url "https://github.com/domtronn/all-the-icons.el"))

;; Transparency
(set-frame-parameter nil 'alpha-background 100)
(defvar nao/alpha-background 100)

(defun nao/toggle-alpha-background ()
  "Toggle alpha-background between 90 and 100."
  (interactive)
	(cond
   ((= nao/alpha-background 70) (setq nao/alpha-background 80))
				((= nao/alpha-background 80) (setq nao/alpha-backnground 90))
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
  :vc (:url https://github.com/emacsorphanage/git-gutter)
  :config
  (setq git-gutter:update-interval 2))

(set-face-background 'git-gutter:modified "#000000")
(set-face-foreground 'git-gutter:added "#00c06f")
(set-face-background 'git-gutter:added "#00c06f")
(set-face-foreground 'git-gutter:deleted "#ff5f5")
(fringe-mode '(12 . 3))

;;Use this to change the fringe for and background color
(set-face-attribute 'fringe nil :background "#0d0e1c" :foreground "ffffff")

(use-package spacious-padding
  :vc (:url "https://github.com/protesilaos/spacious-padding"))

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

(use-package projectile
  :vc (:url "https://github.com/bbatsov/projectile")
  :init
  (projectile-mode +1)
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  :bind (:map projectile-mode-map
              ("C-," . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package fontaine
  :vc (:url "https://github.com/protesilaos/fontaine"))

(setq fontaine-presets
      '((regular
         :default-family "JetBrains Mono"
         :default-weight regular
         :default-slant normal
         :default-height 120

         :fixed-pitch-family "JetBrains mono"
         :fixed-pitch-weight regular
         :fixed-pitch-slant normal
         :fixed-pitch-height 120

         :fixed-pitch-serif-family nil
         :fixed-pitch-serif-weight nil
         :fixed-pitch-serif-slant nil
         :fixed-pitch-serif-height 1.0

         :variable-pitch-family "Libre Baskerville"
         :variable-pitch-weight nil
         :variable-pitch-slant nil
         :variable-pitch-height 1.3

         :mode-line-active-family nil
         :mode-line-active-weight nil
         :mode-line-active-slant nil
         :mode-line-active-height 1.0

         :mode-line-inactive-family nil
         :mode-line-inactive-weight nil
         :mode-line-inactive-slant nil
         :mode-line-inactive-height 1.0

         :header-line-family nil
         :header-line-weight nil
         :header-line-slant nil
         :header-line-height 1.0

         :line-number-family nil
         :line-number-weight nil
         :line-number-slant nil
         :line-number-height 1.0

         :tab-bar-family nil
         :tab-bar-weight nil
         :tab-bar-slant nil
         :tab-bar-height 1.0

         :tab-line-family nil
         :tab-line-weight nil
         :tab-line-slant nil
         :tab-line-height 1.0

         :bold-family nil
         :bold-slant nil
         :bold-weight bold
         :bold-height 1.0

         :italic-family nil
         :italic-weight nil
         :italic-slant italic
         :italic-height 1.0

         :line-spacing nil)
        (tutor
         :default-height 320)))

(fontaine-set-preset 'regular)
(fontaine-mode 1)
(define-key global-map (kbd "C-c f") #'fontaine-set-preset)
;;TODO
;; (set-face-attribute 'org-level-1 nil :height 240)
;; (set-face-attribute 'org-level-2 nil :height 200)
;; (set-face-attribute 'org-level-3 nil :height 180)
;; (set-face-attribute 'org-level-4 nil :height 200)
;; (set-face-attribute 'org-level-5 nil :height 200)
;; (set-face-attribute 'org-level-6 nil :height 200)
;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-block nil :inherit 'fixed-pitch)

(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat"))

(setq eat-enable-directory-tracking t)

(defun nao/kitty ()
  "Open a Kitty terminal in the current working directory."
  (interactive)
  (let ((cwd default-directory))  ; Get the current working directory
    (start-process "kitty" nil "kitty" "sh" "-c" (concat "cd " cwd " && $SHELL"))))

(defun nao/foot-open-in-directory ()
  (interactive)
  "Open foot terminal in the current directory and give it a title of Foot or the current projectile project. Thanks Ashraz for the ignore part"
    (start-process "foot" nil "foot" "-D" (expand-file-name default-directory) "--title"
                   (or (ignore-errors (project-name (project-current))) "Foot")))

(global-set-key (kbd "<escape>") 'tab-next)
(global-set-key (kbd "<f5>") 'async-shell-command)
(global-set-key (kbd "<f6>") 'eshell)
(global-set-key (kbd "<f7>") 'hydra-window/body)
(global-set-key (kbd "<f8>") 'display-line-numbers-mode)
(global-set-key (kbd "<f10>") 'tab-switch)
(global-set-key (kbd "<f10>") 'persp-switch)
(global-set-key (kbd "C-c D") 'dirvish-dispatch)
(global-set-key (kbd "C-c d") 'dirvish)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "<f12>") 'nao/toggle-alpha-background)
(global-set-key (kbd "C-x l") 'previous-buffer)
(global-set-key (kbd "C-x h") 'next-buffer)
(global-set-key (kbd "C-x j") 'goto-line)
(global-set-key (kbd "C-c j") 'webjump)
(global-set-key (kbd "C-c C-j") 'webjump)
(global-set-key (kbd "C-x C-t") 'transpose-chars)
(global-set-key (kbd "M-j") 'avy-goto-char-2)
(global-set-key (kbd "M-k") 'nao/copy-line)
(global-set-key (kbd "M-K") 'duplicate-dwim)
(global-set-key (kbd "C-x o") 'nao/other-window)
(global-set-key (kbd "<mouse-8>") 'scroll-up-command)
(global-set-key (kbd "<mouse-9>") 'scroll-down-command)

(define-key prog-mode-map (kbd "C-c C-o") 'browse-url-at-point)
"C-x o"
(defun nao/move-divider-right ()
		(if (windows-sharing-edge (selected-window) 'right)
				(enlarge-window-horizontally 1)
			(shrink-window-horizontally 1)))

(defun nao/move-divider-left ()
		(if (windows-sharing-edge (selected-window) 'right)
				(shrink-window-horizontally 1)
			(enlarge-window-horizontally 1)))

(defun nao/move-divider-down ()
		(if (windows-sharing-edge (selected-window) 'below)
				(enlarge-window 1)
			(shrink-window 1)))

(defun nao/move-divider-up ()
		(if (windows-sharing-edge (selected-window) 'below)
				(shrink-window 1)
			(enlarge-window 1)))

(use-package hydra
  :vc (:url "https://github.com/abo-abo/hydra"))

(defhydra hydra-buffer (:color red :hint nil)
	"
_j_ previous buffer _k_ last buffer _i_buffer _b_ switch buffer
_o_ buffer other window _x_ kill buffer _s_cratch _m_essages"

	("j" previous-buffer)
	("k" next-buffer)
	("i" ibuffer)
	("b" consult-buffer)
	("o" consult-buffer-other-window)
	("x" kill-buffer)
	("s" (switch-to-buffer "*scratch*"))
	("m" (switch-to-buffer "*Messages*"))
  ("H" (nao/move-divider-left))
  ("J" (nao/move-divider-down))
  ("K" (nao/move-divider-up))
  ("L" (nao/move-divider-right))
  ("q" nil))

(defhydra hydra-window (:color red
                        :hint nil)
  "
  Split: _v_ert _x_:horz
   Move: _s_wap
   Size: _H_ shrink horz _J_ shrink vert _K_ enlarge vert _L_ enlarge vert
 Frames: _df_ delete
Leaders: 'f' file 'b' buffer 'd' delete 't' Eat"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" (nao/move-divider-left))
  ("J" (nao/move-divider-down))
  ("K" (nao/move-divider-up))
  ("L" (nao/move-divider-right))
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("tt" eat)
  ("tp" eat-project)
  ("to" eat-other-window)
  ("v" split-window-right)
  ("x" split-window-below)
  ("s" window-swap-states)
  ("tt" eat)
  ("tp" eat-project)
  ("o" delete-other-windows :exit t)
  ("ff" find-file)
  ("fo" find-file-other-window)
  ("bb" consult-buffer)
  ("bo" consult-buffer-other-window)
  ("dw" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t)
  ("q" nil))

(use-package 0x0
  :vc (:url "https://github.com/emacsmirror/0x0"))


(defun nao/replace-and-keep-kill()
  (interactive)
  (mark-sexp)
  (kill-region (region-beginning) (region-end))
  (insert (nth 1 kill-ring)))

(defun nao/replace-and-discard()
  (interactive)
  (mark-sexp)
  (kill-region (region-beginning) (region-end))
  (setq kill-ring (cdr kill-ring))  
  (insert (nth 0 kill-ring)))

(defun nao/copy-line ()
  (interactive)
  (save-excursion
    (if (equal current-prefix-arg nil)
        (progn
          (beginning-of-line)
          (set-mark (point))
          (end-of-line)
          (copy-region-as-kill (region-beginning) (region-end)))
      (progn
        (beginning-of-line)
        (set-mark (point))
        (next-line current-prefix-arg)
        (copy-region-as-kill (region-beginning) (region-end))))))

(use-package repeat
  :config
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)
  :bind
  ("C-t" . nao/transpose-line-down)
  ("C-S-t" . nao/traspose-line-up)
  (:repeat-map transpose-repeat-map
	       ("t" . nao/transpose-line-down)
	       ("T" . nao/transpose-line-up))
  :config
  (defun nao/transpose-line-up ()
    "Transpose the current line up and move cusror to line where it moved"
    (interactive)
    (transpose-lines 1)
    (previous-line 2))
  (defun nao/transpose-line-down ()
	"Transpose the current line down and move cusror to line where it moved"
	(interactive)
	(next-line 1)
	(transpose-lines 1)
	(previous-line 1)))

(repeat-mode)
(use-package window
  :ensure nil
  :bind
  ("C-x o"   . +other-window)
  ("C-x O"   . +other-other-window)
  (:repeat-map other-window-repeat-map
               ("o" . +other-window)
               ("O" . +other-other-window))
  :config
  (defun +other-other-window ()
    "Go to previous window."
    (interactive)
    (other-window -1))
  (defun +other-window ()
    "Open cousult-buffer-other-window if only window or swich windows"
    (interactive)
    (if (one-window-p)
        (consult-buffer-other-window)
      (other-window 1))))

(setq dashboard-banner-ascii "
███████╗███╗░░░███╗░█████╗░░█████╗░░██████╗
██╔════╝████╗░████║██╔══██╗██╔══██╗██╔════╝
█████╗░░██╔████╔██║███████║██║░░╚═╝╚█████╗░
██╔══╝░░██║╚██╔╝██║██╔══██║██║░░██╗░╚═══██╗
███████╗██║░╚═╝░██║██║░░██║╚█████╔╝██████╔╝
╚══════╝╚═╝░░░░░╚═╝╚═╝░░╚═╝░╚════╝░╚═════╝░
")

;; Simple dashboard that displays recent files and upcoming agenda items
(use-package dashboard
  :vc (:url "https://github.com/emacs-dashboard/emacs-dashboard")
  :config
  :bind
  (:map dashboard-mode-map
  ("A" . org-agenda))
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)

  (dashboard-items '((agenda . 5)
                     (recents . 5)))

  (dashboard-center-content t)
  (dashboard-startup-banner 'ascii)
  (dashboard-set-footer nil)
  (dashboard-banner-logo-title nil))
(dashboard-open)

;;(setq initial-buffer-choice "/home/naokotani/Documents/denote/20230911T100446--todos__personal.org")

(use-package auth-source-pass)
(setq auth-source-pass-file "~/.password-store")
(setq auth-source-creation-default-password-scheme 'default)

(setopt rcirc-authinfo
        '(("Libera.Chat" nickserv "Naokotani" (auth-source-pass-get 'secret "irc/Naokotani"))))

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

(add-hook 'erc-insert-post-hook (lambda ()
                                  (let ((line-spacing 0.5))
                                  (insert ""))))
(setq erc-prompt "> ")
(setq erc-insert-timestamp-left-and-right "%H:%M")
(setq erc-format-message "[%t] %n: %s")

(defun nao/magit-status-select-project ()
  "Prompt the user to select a project from the list and call nao/magit-status-for-project."
  (interactive)
  (let ((project (completing-read "Select a project: " nao/git-projects)))
    (nao/magit-status-for-project project)))

(defun nao/magit-status-for-project (project)
  "Switch to PROJECT directory and display its Git status."
  (cd project)
  (magit-status))

(use-package all-the-icons-dired
  :vc (:url "https://github.com/jtbm37/all-the-icons-dired"))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso"))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-dwim-target t))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
        '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
          ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
          (".*" "xdg-open"))))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)

  ;; In Emacs 29 there is a binding for `repeat-mode' which lets you
  ;; repeat C-x C-j just by following it up with j.  For me, this is a
  ;; problem as j calls `dired-goto-file', which I often use.
  (define-key dired-jump-map (kbd "j") nil))

(use-package dired-aux
  :ensure nil
  :after dired
  :bind
  ( :map dired-mode-map
    ("C-+" . dired-create-empty-file)
    ("M-s f" . nil)
    ("C-<return>" . dired-do-open) ; Emacs 30
    ("C-x v v" . dired-vc-next-action)) ; Emacs 28
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask) ; Emacs 27
  (setq dired-vc-rename-file t)             ; Emacs 27
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
  (setq dired-create-destination-dirs-on-trailing-dirsep t)) ; Emacs 29

(use-package dired-x
  :ensure nil
  :after dired
  :bind
  ( :map dired-mode-map
    ("I" . dired-info))
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil))

(add-to-list 'load-path "/home/naokotani/.config/emacs/extra/dired-hacks")
(require 'dired-subtree)

;; TODO didnt load
(use-package dired-subtree
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package image-dired
  :ensure nil
  :commands (image-dired)
  :bind
  ( :map image-dired-thumbnail-mode-map
    ("<return>" . image-dired-thumbnail-display-external))
  :config
  (setq image-dired-thumbnail-storage 'standard)
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4))

;;; Automatically preview Dired file at point (dired-preview.el)
;; One of my packages: <https://protesilaos.com/emacs>
(use-package dired-preview
  :ensure t
  ;; :hook (dired-mode . (lambda ()
  ;;                       (when (string-match-p "Pictures" default-directory)
  ;;                         (dired-preview-mode 1))))
  :defer 1
  :hook (after-init . dired-preview-global-mode)
  :config
  (setq dired-preview-max-size (* (expt 2 20) 10))
  (setq dired-preview-delay 0.5)
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(gz\\|"
                "zst\\|"
                "tar\\|"
                "xz\\|"
                "rar\\|"
                "zip\\|"
                "iso\\|"
                "epub"
                "\\)"))

  (setq dired-preview-display-action-alist
        '((display-buffer-in-side-window)
          (side . bottom)
          (window-height . 0.2)
          (preserve-size . (t . t))
          (window-parameters . ((mode-line-format . none)
                                (header-line-format . none)))))
  )

(use-package pdf-tools)

;;; dired-like mode for the trash (trashed.el)
;; (use-package trashed
;;   :ensure t
;;   :commands (trashed)
;;   :config
;;   (setq trashed-action-confirmer 'y-or-n-p)
;;   (setq trashed-use-header-line t)
;;   (setq trashed-sort-key '("Date deleted" . t))
;;   (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; TODO <glenneth> typst-ts-mode, yeah
;;      <glenneth> and I hook .typ documents in to eglot and tinymist. [23:10]>  

(use-package auctex
	:ensure t)

(require 'tex)

(setq org-export-date-timestamp-format "%B %d, %y")

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((exec-path-from-shell :url
			   "https://github.com/purcell/exec-path-from-shell"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
