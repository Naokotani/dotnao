(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))
(setq indent-line-function 'insert-tab)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(use-package prettier-js)
(setq js-jsx-indent-level 2)
(setq js-indent-level 2)
(straight-use-package 'project)
(add-hook 'js-mode-hook 'eglot-ensure)

(add-hook 'rust-mode-hook
          (lambda () (set (make-local-variable 'compile-command) "npx tsc")))

;; Typescript
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts?\\'" . js-mode))

;; C setup
(defun nao/c-mode-keybinds ()
  (define-key c-mode-map (kbd "C-c m") 'compile))
(add-to-list 'auto-mode-alist '("\\.c\\.h\\'" . c-ts-mode))
(add-hook 'c-ts-mode 'eglot-ensure)
(add-hook 'c-ts-mode-hook #'nao/c-mode-keybinds)
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "gnu")))
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "gnu")))

(use-package emmet-mode
  :config
  (setq emmet-move-cursor-between-quotes t)
  :hook ((sgml-mode . emmet-mode)
        (css-mode . emmet-mode)
        (web-mode . emmet-mode)
        (js-mode . emmet-mode)))

(use-package web-mode
  :config
  (setq web-mode-code-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(electric-pair-mode)

;;Rust
(use-package yasnippet)
(use-package yasnippet-snippets)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Common lisp setup
;;(use-package slime
;;	:config
;;	(setq inferior-lisp-program "sbcl"))

;; (use-package svelte-mode)
;; (add-to-list 'eglot-server-programs '((svelte-mode) "svelteserver" "--stdio"))
;; (add-hook 'svelte-mode-hook 'eglot-ensure)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package yaml-pro)
;;(use-package tree-sitter)
;;(use-package typescript-mode)
;;(use-package tree-sitter-langs)
;;(add-hook 'typescript-ts-mode-hook #'tree-sitter-mode)
;;(add-to-list 'treesit-extra-load-path "/home/naokotani/src/tree-sitter-modules/dist")
;;(setq rust-mode-treesitter-derive t)


;;(setq treesit-language-source-alist
;;   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;     (css "https://github.com/tree-sitter/tree-sitter-css")
;;     (cmake "https://github.com/uyha/tree-sitter-cmake")
;;     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;     (c "https://github.com/tree-sitter/tree-sitter-c")
;;     (go "https://github.com/tree-sitter/tree-sitter-go")
;;     (html "https://github.com/tree-sitter/tree-sitter-html")
;;     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;     (json "https://github.com/tree-sitter/tree-sitter-json")
;;     (make "https://github.com/alemuller/tree-sitter-make")
;;     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;     (python "https://github.com/tree-sitter/tree-sitter-python")
;;     (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package clojure-mode)
(use-package cider)
(use-package clj-deps-new)
(setq cider-shadow-default-options "app")

(use-package elpy)
(use-package geiser-mit)

(require 'org)
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
(require 'cider)
(setq org-confirm-babel-evaluate nil)

(use-package request)

;;use-package docker
;;  :ensure t
;;  :bind ("C-c d" . docker))

(use-package cmake-mode)

(setq gdb-many-windows t)

(use-package hl-todo)
(use-package consult-todo)
