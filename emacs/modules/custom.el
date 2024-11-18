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


(defun nao/transpose-line-down ()
	"Transpose the current line down and move cusror to line where it moved"
	(interactive)
	(next-line 1)
	(transpose-lines 1)
	(previous-line 1))

(defun nao/transpose-line-up ()
	"Transpose the current line up and move cusror to line where it moved"
	(interactive)
	(transpose-lines 1)
	(previous-line 2))








