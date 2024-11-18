(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;; Emacs follows the working directory in Eat
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
