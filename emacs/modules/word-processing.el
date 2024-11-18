;; Enables spell checking for text mode and disable for some other modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Set spell checking to use the command line program hunspell
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_CA")
(setq ispell-alternate-dictionary "/usr/share/hunspell/en_CA-large.dic")

(defun nao/org-ispell-ignore ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re)))

(add-hook 'org-mode-hook #'nao/org-ispell-ignore)

(use-package csv-mode)

(defun nao/replace-verbatim-with-tcblisting ()
  "Replace verbatim with tcblisting"
  (interactive)
  (let ((end-tcblisting "\\end{tcblisting}")
        (begin-tcblisting "
\\begin{tcblisting}{
    listing engine=minted,
    minted style=nord,
    listing only,
    boxrule=1pt,
    colback=nordbg,
    colframe=nordfg,
}")
        (end-verbatim "\\end{verbatim}")
        (begin-verbatim "\\begin{verbatim}"))
    (goto-char -1)
    (replace-string begin-verbatim begin-tcblisting)
    (goto-char -1)
    (replace-string end-verbatim end-tcblisting)))

(defun nao/replace-minted-with-tcblisting ()
  "Replace verbatim with tcblisting"
  (interactive)
  (let ((end-tcblisting "\\end{tcblisting}")
        (begin-tcblisting "
\\begin{tcblisting}{
    listing engine=minted,
    minted style=nord,
    listing only,
    boxrule=1pt,
    colback=nordbg,
    colframe=nordfg,
}")
        (end-verbatim "\\end{minted}")
        (begin-verbatim "\\begin{minted}[]{java}"))
    (goto-char -1)
    (replace-string begin-verbatim begin-tcblisting)
    (goto-char -1)
    (replace-string end-verbatim end-tcblisting)))
