;; FIXME require dash

(defvar sql-modern-configurations '()
  "Database configurations. [...]")

(add-to-list 'sql-modern-configurations
             '(:name sqlmod-test
               :command ("/usr/bin/mysql"
                         "--protocol" "tcp"
                         "--host" "192.168.56.210"
                         "--port" "3306"
                         "--user" "sqlmod"
                         "--password=password"
                         "--prompt" ""
                         "--batch" "--force" "--raw" "--reconnect"
                         "sqlmod")))

(defvar sql-modern-sessions '()
  "Running sessions. [alist] [...]")

(defun sql-modern-start-session (conf)
  (let ((session `(:conf ,conf
                         :proc nil
                         :inbuf nil
                         :outbuf nil))
        session-name proc inbuf outbuf)
    (setq session-name (concat (symbol-name (plist-get conf :name))
                               "-"
                               (number-to-string
                                (1+ (sql-modern--find-instance-count conf)))))
    ;; TODO stand up session proc and bufs
    (add-to-list 'sql-modern-sessions
                 (cons session-name session))))

(defun sql-modern-stop-session (session-name)
  (setq sql-modern-sessions
        (cl-remove-if #'(lambda (sess)
                          (string= session-name (car sess)))
                      sql-modern-sessions)))

(defun sql-modern--find-instance-count (conf)
  (let ((want (plist-get conf :name))
        counts)
    (loop
       for rec being the elements of sql-modern-sessions
       doing (let* ((conf-name (plist-get (plist-get (cdr rec) :conf) :name))
                    (pair (or (assoc conf-name counts) (cons conf-name 0))))
               (incf (cdr pair))
               (if (assoc conf-name counts)
                   (setf (cdr (assoc conf-name counts))
                         (cdr pair))
                   (push pair counts))))
    (or (cdr (assoc want counts)) 0)))
