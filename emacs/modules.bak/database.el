 (defun nao/select-sql ()
   (interactive)
   (sql-send-string (format "SELECT * FROM %s;" (read-string "table: ")))
   (end-of-buffer))
