;; eval the first two stanzas to create a proc, create a buffer
;; '*mysql output scratch*' in org-mode so the formatted results have
;; a place to go, call foo-send-query to play with it

;; TODO handle errors
;; TODO properly handle results (names, etc.)
;; TODO accept input from a separate buffer in a mode to be created
;; TODO make it possible to instantiate/select sessions
;; TODO abstract for handling multiple RDBMSes

;; create a process
(let (proc)
  (setq proc
        (start-process "mysql" " *mysql*"
                       "/usr/bin/mysql"
                       "--protocol" "tcp"
                       "--host" "192.168.56.210"
                       "--port" "3306"
                       "--user" "sqlmod"
                       "--password=password"
                       "--prompt" ""
                       "--batch" "--force" "--raw" "--reconnect"
                       "sqlmod"))
  (process-put proc :outbuf
               (get-buffer-create "*mysql output scratch*"))
  (with-current-buffer " *mysql*"
    (goto-char (point-max))))

;; add a process filter
(when (get-process "mysql")
  (let ((proc (get-process "mysql")))
    (set-process-filter proc
                        #'(lambda (proc string)
                            (with-current-buffer (process-buffer proc)
                              (goto-char (point-max))
                              (insert string)
                              (goto-char (point-max)))
                            (with-current-buffer (process-get proc :outbuf)
                              (let ((inhibit-read-only t)
                                    (result-table (foo-reformat-mysql-org string))
                                    insert-posn)
                                (goto-char (point-max))
                                ;; convert this to a case for error handling
                                (if (not (string= "" result-table))
                                    (progn
                                      (setq insert-posn (point))
                                      (insert result-table)
                                      (goto-char (1+ insert-posn))
                                      (org-table-insert-hline)
                                      (org-table-align)
                                      (foo-goto-point-max (process-get proc :outbuf)))
                                    (progn (insert "No result\n")
                                           (foo-goto-point-max (process-get proc :outbuf))))))))))

;; kill the process
(kill-process "mysql")

(defun foo-send-query (str)
  (if (not (null (get-process "mysql")))
      (let* ((proc (get-process "mysql"))
             (procbuf (process-buffer proc))
             (outbuf (process-get proc :outbuf))
             (query str))
        ;; strip off \Gs, which will break parsing
        ;; FIXME handle multiple queries sanely
        (save-match-data
          (while (string-match "\\\\G" query)
            (setq query (replace-match ";" nil t str))))
        ;; add the query to the proc buffer
        (with-current-buffer procbuf
          (goto-char (point-max))
          (insert (concat query "\n"))
          (goto-char (point-max)))
        ;; add the query to the out buffer
        (with-current-buffer outbuf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\n")
            (insert (concat "* " (format-time-string "%Y-%m-%d %H:%M:%S"
                                                     (current-time)) "\n"))
            (insert "#+BEGIN_SRC sql :eval never\n")
            (insert (concat query "\n"))
            (insert "#+END_SRC\n\n")))
        (process-send-string proc
                             (concat query ";SELECT 'mysql-query-done';\n")))
      (message "No active process")))

(defun foo-reformat-mysql-org (str)
  "Reformat STR from tab-separated values to Org table format."
  (let* ((lines (split-string str (pcre-to-elisp "\n") t))
         (fields (mapcar '(lambda (line)
                           (split-string line (pcre-to-elisp "\t") t)) (butlast lines 2))))
    (mapconcat #'identity
               (mapcar #'(lambda (row)
                           (concat "| " (mapconcat #'identity row " | ") " |"))
                       fields)
               "\n")))

(defun foo-goto-point-max (buf-or-name)
  (let ((last-window (selected-window)))
    (select-window (get-buffer-window buf-or-name (selected-frame)))
    (goto-char (point-max))
    (select-window last-window)))
