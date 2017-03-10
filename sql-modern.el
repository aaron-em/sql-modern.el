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
  )

(defun sql-modern--find-instance-count (conf)
  (let ((want (plist-get conf :name))
        count)
    (setq count
          (->> sql-modern-sessions
               (mapcar #'(lambda (rec)
                           (-> rec
                               (plist-get :conf)
                               (plist-get :name))))
               (message "%s")
               (cl-reduce #'(lambda (acc name)
                              (let (lis)
                                (if (not (listp acc))
                                    (setq lis `((,name . 1)))
                                    (setq lis acc))
                                (if (null (assoc name lis))
                                    (add-to-list 'lis `(,name . 1))
                                    (incf (cdr (assoc name lis))))
                                lis)))
               (assoc want)
               (cdr)))
    (or count 0)))
