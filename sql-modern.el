(defvar sql-modern-keep-output t
  "Whether to kill the session output buffer when tearing down
the session. [...]")

(defvar sql-modern-configurations '()
  "Database configurations. [...]")

(add-to-list 'sql-modern-configurations
             '(:name sqlmod-test-fail
               :engine mysql
               :command ("/usr/bin/mysql"
                         "--protocol" "tcp"
                         "--host" "localhost"
                         "--port" "3306"
                         "--user" "sqlmod"
                         "--password=welp"
                         "--prompt" ""
                         "--batch" "--force" "--raw" "--reconnect"
                         "sqlmod")))

(add-to-list 'sql-modern-configurations
             '(:name sqlmod-test-pass
               :engine mysql
               :command ("/usr/bin/mysql"
                         "--protocol" "tcp"
                         "--host" "localhost"
                         "--port" "3306"
                         "--user" "sqlmod"
                         "--password=password"
                         "--prompt" ""
                         "--batch" "--force" "--raw" "--reconnect"
                         "sqlmod")))

(defvar sql-modern-sessions '()
  "Running sessions. [alist] [...]")

(defun sql-modern-start-session (conf)
  (let ((session `((name . nil)
                   (conf . ,conf)
                   (proc . nil)
                   (procbuf . nil)
                   (inbuf . nil)
                   (outbuf . nil)))
        session-name proc)

    (setq session-name (concat (symbol-name (plist-get conf :name))
                               "-"
                               (number-to-string
                                (1+ (sql-modern--find-instance-count conf)))))
    (setf (cdr (assoc 'name session))
          session-name)

    ;; Stand up session buffers
    (setf (cdr (assoc 'procbuf session))
          (sql-modern--make-session-buffer 'process session-name))
    (setf (cdr (assoc 'inbuf session))
          (sql-modern--make-session-buffer 'input session-name))
    (setf (cdr (assoc 'outbuf session))
          (sql-modern--make-session-buffer 'output session-name))

    ;; Stand up session database interaction process
    (setq proc (sql-modern--make-session-process conf session))
    (setf (cdr (assoc 'proc session)) proc)

    ;; If we got a clean start, add this session to the list
    (if (process-live-p proc)
        (add-to-list 'sql-modern-sessions
                     (cons session-name session)))))

(defun sql-modern-stop-session (session-name)
  (let* ((session (sql-modern--get-session-by-name session-name)))
    (sql-modern--cleanup-session session)))

(defun sql-modern--cleanup-session (session)
  (let ((session-name (cdr (assoc 'name session)))
        (proc (cdr (assoc 'proc session))))
    ;; Kill session buffers, except outbuf when sql-modern-keep-output
    (loop for key in '(procbuf inbuf outbuf)
       doing (let ((buf (cdr (assoc key session))))
               (cond
                 ((and (eq key 'outbuf)
                       (not sql-modern-keep-output))
                  (kill-buffer buf))
                 (t (kill-buffer buf)))))

    ;; Kill session process, if it's not already dead
    ;; FIXME disable sentinel warning, since we mean to do this
    (when (process-live-p proc)
      (kill-process proc))
    
    ;; Finally, remove session from session list
    (setq sql-modern-sessions
          (cl-remove-if #'(lambda (sess)
                            (string= session-name (car sess)))
                        sql-modern-sessions))))

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

(defun sql-modern--get-session-by-name (session-name)
  (car (cl-remove-if-not #'(lambda (sess)
                            (string= session-name (car sess)))
                        sql-modern-sessions)))

(defun sql-modern--make-session-buffer (type basename)
  (let ((name (cond
                ((eq type 'process)
                 (concat " *" basename ": process*"))
                ((eq type 'input)
                 (concat "*" basename ": sql-input*"))
                ((eq type 'output)
                 (concat "*" basename ": output*"))))
        (mode (cond
                ((eq type 'input) #'sqlm-input-mode)
                ((eq type 'output) #'sqlm-output-mode)
                (t #'fundamental-mode)))
        buf)
    (when (null name)
      (error (concat "Unknown session buffer type: " (symbol-name type))))
    (setq buf (get-buffer-create name))
    (with-current-buffer buf
      (funcall mode)
      (delete-region (point-min) (point-max))
      (goto-char (point-min)))
    buf))

(defun sql-modern--make-session-process (conf session)
  (let ((proc (apply #'start-process
                     (concat "sqlm: "
                             (symbol-name (plist-get conf :engine))
                             ": "
                             (cdr (assoc 'name session)))
                     (cdr (assoc 'procbuf session))
                     (plist-get conf :command))))

    (when (null (process-live-p proc))
      (error "Session [session-name] failed to start a process"))
    
    (process-put proc :conf conf)
    (process-put proc :session session)
    
    (set-process-sentinel proc #'sql-modern--sql-process-sentinel)
    (set-process-filter proc #'sql-modern--sql-process-filter)
    
    proc))

(defun sql-modern--sql-process-sentinel (proc event)
  (let ((conf (process-get proc :conf))
        (session (process-get proc :session))
        err)
    ;; TODO capture and present process output
    (with-current-buffer (cdr (assoc 'procbuf session))
      (setq err (buffer-substring-no-properties (point) (point-max))))
    (sql-modern--cleanup-session session)
    (error (concat "Process '" (process-name proc)
                   "' " event err))))

(defun sql-modern--sql-process-filter (proc string)
  ;; TODO when binding, wrap in a lexical scope containing conf and session
  ;; TODO handle input for all buffers (send to proc, insert into output)
  ;; TODO figure out how to handle process death
  ;; (message "%s: %s" proc string)
  (with-current-buffer (process-buffer proc)
    (let ((posn (point)))
      (goto-char (point-max))
      (insert string)
      (goto-char posn))))

(define-derived-mode sqlm-input-mode
    sql-mode
  "SQLm In"
  "..."
  nil)

(define-key sqlm-input-mode-map (kbd "C-c C-c")
  #'sql-modern-input--send-paragraph)
(define-key sqlm-input-mode-map (kbd "C-c C-r")
  #'sql-modern-input--send-region)

(define-derived-mode sqlm-output-mode
    org-mode
  "SQLm Out"
  "..."
  (read-only-mode t))

(defun sql-modern-input--send-region (from to)
  (interactive "r")
  (message "%s" (buffer-substring-no-properties from to)))

(defun sql-modern-input--send-paragraph nil
  (interactive)
  (let ((from (save-excursion
                (backward-paragraph)
                (point)))
        (to (save-excursion
              (forward-paragraph)
              (point))))
    (sql-modern-input--send-region from to)))
