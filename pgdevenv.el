;;; pgdevenv.el --- Manage your PostgreSQL development envs
;;
;; Copyright (C) 2012 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; GIT: https://github.com/dimitri/pgdevenv-el
;; Version: 1.1
;; Created: 2012-09-28
;; Keywords: emacs postgresql development environment shell debug gdb
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.

(require 'cl)				; of course.

(defgroup pgdev nil
  "Run several PostgreSQL development versions"
  :group 'comm)

(defcustom pgdev-cc-path "/usr/bin/gcc"
  "Local path leading to `gcc' or your other favorite C compiler"
  :group 'pgdev)

(defcustom pgdev-ccache-path "/usr/bin/ccache"
  "Local path leading to `ccache'"
  :group 'pgdev)

(defcustom pgdev-gdb-path "/usr/bin/gdb"
  "Local path leading to `gdb'"
  :group 'pgdev)

(defcustom pgdev-configure-option-list
  '("--enable-cassert"
    "--enable-debug"
    "--enable-depend"
    "--with-perl"
    "--with-python"
    "--with-tcl")
  "PostgreSQL ./configure options, --prefix is automatically prepended"
  :group 'pgdev)

(defcustom pgdev-clone-root "~/dev/PostgreSQL"
  "Top directory where to git clone the PostgreSQL development branches"
  :group 'pgdev)

(defcustom pgdev-install-root "~/pgsql"
  "Top directory where to git clone the PostgreSQL development branches"
  :group 'pgdev)

(defcustom pgdev-logfile-root "/tmp"
  "directory where to store the logs"
  :group 'pgdev)

(defcustom pgdev-my-git-url "https://github.com/dimitri/postgres.git"
  "URL of the your own repository"
  :group 'pgdev)

(defcustom pgdev-pg-git-url "git://git.postgresql.org/git/postgresql.git"
  "URL of the PostgreSQL upstream repository (or your mirror of it)"
  :group 'pgdev)

(defcustom pgdev-my-branches
  '(("ddl" "postgres" "master" "54393"))
  "Deveopper owned branches"
  :group 'pgdev)

(defcustom pgdev-pg-branches
  '(("9.2" "pg9.2" "REL9_2_STABLE" "54392")
    ("9.1" "pg9.1" "REL9_1_STABLE" "54391")
    ("9.0" "pg9.0" "REL9_0_STABLE" "54390")
    ("8.4" "pg8.4" "REL8_4_STABLE" "54384")
    ("8.3" "pg8.3" "REL8_3_STABLE" "54383"))
  "NAME DIRECTORY TAG PORT"
  :group 'pgdev)

;;
;; Basics, must have
;;
(defun pgdev-read-branch-name ()
  "Interactively read a branch name with completion"
  (let* ((branches
	  (loop for (name branch port) in (append pgdev-my-branches
						  pgdev-pg-branches)
		collect name)))
    (completing-read "PostgreSQL branch: " branches)))

(defun pgdev-get-full-details (branch)
  "From given BRANCH name, return (NAME BRANCH PORT URL)"
  (let* ((my (assoc branch pgdev-my-branches))
	 (pg (assoc branch pgdev-pg-branches)))
    (destructuring-bind (url name dir branch port)
	(if my (cons pgdev-my-git-url my)
	  (cons pgdev-pg-git-url pg))
      (list name dir branch port url))))

(defun pgdev-get-bin-dir (branch)
  "We assume that install prefix directory is branch name"
  (expand-file-name (format "%s/bin" branch) pgdev-install-root))

(defun pgdev-duplicate-path (p1 p2)
  "Trick duplicate test into pruning path to different prefixes"
  ;; now the comparison
  (or (string= p1 p2)
      (and (string= p1 pgbin) (member p2 all-pgbins))
      (and (string= p2 pgbin) (member p1 all-pgbins))))

(defun pgdev-set-path (branch)
  "Reset PATH to the right value for given BRANCH"
  ;; we rely on dynamic binding of pgbin in pgdev-duplicate-path
  (let* ((pgbin   (pgdev-get-bin-dir branch))
	 (entries (split-string (getenv "PATH") ":"))
	 (others-pgbin-list
	  ;; name dir git-branch port, avoid name clashes though
	  (loop for (n d b p) in (append pgdev-my-branches
					 pgdev-pg-branches)
		unless (string= n branch)
		collect (expand-file-name (format "%s/bin" n)
					  pgdev-install-root)))
	 (newpath
	  (loop with np
		for entry in (cons pgbin entries)
		unless (or (member entry np)
			   (member entry others-pgbin-list))
		collect entry into np
		finally return np)))
    (setenv "PATH" (mapconcat 'identity newpath ":"))))

;;;###autoload
(defun pgdev-open-shell (branch)
  "Open a shell for given branch"
  (interactive (list (pgdev-read-branch-name)))
  (let* ((cwd    (expand-file-name branch pgdev-install-root))
	 (pgdata (expand-file-name "data" cwd))
	 (buf    (format "*Shell: PostgreSQL %s*" branch)))

    (destructuring-bind (branch dir git-branch port url)
	(pgdev-get-full-details branch)

      ;; branch working directory
      (let* ((bwd (expand-file-name dir pgdev-clone-root))
	     (default-directory (file-name-as-directory bwd)))

	;; create the target branch working directory if necessary
	(unless (file-directory-p bwd) (mkdir bwd 'parents))

	;; Set the Environment prior to starting the shell
	(setenv "PAGER" "/bin/cat")
	(setenv "PGDATA" pgdata)
	(setenv "PGPORT" port)
	(setenv "CC" (format "%s %s" pgdev-ccache-path pgdev-cc-path))
	(pgdev-set-path branch)

	;; now start the shell, then set some buffer-local variables
	(with-current-buffer (shell buf)
	  ;; add some keymap entries
	  (define-key shell-mode-map (kbd "C-c - c") 'pgdev-insert-configure)
	  (define-key shell-mode-map (kbd "C-c - g") 'pgdev-insert-git-clone)
	  (define-key shell-mode-map (kbd "C-c - s") 'pgdev-insert-pgctl-start)
	  (define-key shell-mode-map (kbd "C-c - S") 'pgdev-insert-pgctl-stop)
	  (define-key shell-mode-map (kbd "C-c - r") 'pgdev-install-and-restart)
	  (define-key shell-mode-map (kbd "C-c - R") 'pgdev-reinitdb)
	  (define-key shell-mode-map (kbd "C-c - D") 'pgdev-debug)
	  (define-key shell-mode-map (kbd "C-c - d") 'pgdev-debug-this-psql)
	  (define-key shell-mode-map (kbd "C-c - f") 'pgdev-edit-config)

	  ;; make local buffer variables to ease coding the keymap entries
	  (let ((pgdev-current-prefix cwd)
		(pgdev-current-pgdata pgdata)
		(pgdev-current-clone-root
		 (expand-file-name dir pgdev-clone-root))
		(pgdev-current-branch branch)
		(pgdev-current-git-branch git-branch)
		(pgdev-current-port port)
		(pgdev-current-url url))

	    (make-local-variable 'pgdev-current-prefix)
	    (make-local-variable 'pgdev-current-pgdata)
	    (make-local-variable 'pgdev-current-clone-root)
	    (make-local-variable 'pgdev-current-branch)
	    (make-local-variable 'pgdev-current-git-branch)
	    (make-local-variable 'pgdev-current-port)
	    (make-local-variable 'pgdev-current-url)))))))

;;
;; Auto type some commands where parts are depending on the current branch
;; we're working in
;;
;;;###autoload
(defun pgdev-insert-git-clone ()
  "Open a shell a clone PostgreSQL in there"
  (interactive)
  (insert
   (format "git clone %s -b %s %s"
	   pgdev-current-url
	   pgdev-current-git-branch
	   pgdev-current-clone-root)))

;;;###autoload
(defun pgdev-insert-configure ()
  "Insert the ./configure command in the current Shell buffer"
  (interactive)
  (insert (mapconcat 'identity
		     (append (list "./configure"
				   "--prefix" pgdev-current-prefix)
			     pgdev-configure-option-list)
		     " ")))

;;;###autoload
(defun pgdev-insert-pgctl-start ()
  "Insert and execute pg_ctl -l <logfile> start"
  (interactive)
  (insert (format "pg_ctl -l %s/pgsql-%s.log start"
		  pgdev-logfile-root
		  pgdev-current-branch))
  (comint-send-input nil t))

;;;###autoload
(defun pgdev-insert-pgctl-stop ()
  "Execute pg_ctl stop"
  (interactive)
  (insert "pg_ctl stop")
  (comint-send-input nil t))

;;;###autoload
(defun pgdev-reinitdb ()
  "Execute rm -rf $PGDATA && initdb && createdb `whoami`"
  (interactive)
  (insert (format "rm -rf \"%s\" && initdb && createdb %s"
		  pgdev-current-pgdata (user-login-name)))
  (comint-send-input nil t))

;;;###autoload
(defun pgdev-install-and-restart ()
  "send input `make install && pg_ctl restart`"
  (interactive)
  (insert "make install && pg_ctl restart")
  (comint-send-input nil t))

;;;###autoload
(defun pgdev-debug ()
  "Must be run from a pgdev shell, opens gdb in another shell"
  (interactive)
  (insert "psql")
  (comint-send-input nil t)
  (pgdev-debug-this-psql))

;;;###autoload
(defun pgdev-debug-this-psql ()
  "Must be run while psql is already running in a pgdev shell"
  (interactive)
  (insert "select pg_backend_pid();")
  (comint-send-input nil t)
  (sit-for 2)				; wait our answer
  (let* ((pid   (save-excursion
		  (goto-char (point-max))
		  (re-search-backward " \\([0-9]\\{3,\\}\\)")
		  (match-string 1)))
	 (bname (format "*Shell: Debug PostgreSQL %s*" pgdev-current-branch))
	 (buf   (get-buffer-create bname))
	 (win   (get-buffer-window buf)))
    (if win
	(select-window win)
      (select-window (split-window))
      (shell bname))
    (with-current-buffer buf
      (toggle-truncate-lines 1)
      (insert (format "%s -p %s" pgdev-gdb-path pid))
      (comint-send-input nil t))))

;;;###autoload
(defun pgdev-edit-config ()
  "Must be run from a pgdev shell, visit the postgresql.conf file"
  (interactive)
  (find-file
   (expand-file-name "postgresql.conf"
    (file-name-as-directory (expand-file-name "data" pgdev-current-prefix)))))

;; finally, our entry point

;;;###autoload
(global-set-key (kbd "C-c - n") 'pgdev-open-shell)

;;
;; Some editing help, in a pgdev-sql minor mode
;;
;; C-M-h   mark-pgsql-query
;; C-M-x   eval-pgsql-query
;;

;;;###autoload
(defvar *pgdev-sql-commands*
  '("ABORT"
    "ALTER AGGREGATE"
    "ALTER COLLATION"
    "ALTER CONVERSION"
    "ALTER DATABASE"
    "ALTER DEFAULT PRIVILEGES"
    "ALTER DOMAIN"
    "ALTER EVENT TRIGGER"
    "ALTER EXTENSION"
    "ALTER FOREIGN DATA WRAPPER"
    "ALTER FOREIGN TABLE"
    "ALTER FUNCTION"
    "ALTER GROUP"
    "ALTER INDEX"
    "ALTER LANGUAGE"
    "ALTER LARGE OBJECT"
    "ALTER OPERATOR"
    "ALTER OPERATOR CLASS"
    "ALTER OPERATOR FAMILY"
    "ALTER ROLE"
    "ALTER SCHEMA"
    "ALTER SEQUENCE"
    "ALTER SERVER"
    "ALTER TABLE"
    "ALTER TABLESPACE"
    "ALTER TEXT SEARCH CONFIGURATION"
    "ALTER TEXT SEARCH DICTIONARY"
    "ALTER TEXT SEARCH PARSER"
    "ALTER TEXT SEARCH TEMPLATE"
    "ALTER TRIGGER"
    "ALTER TYPE"
    "ALTER USER"
    "ALTER USER MAPPING"
    "ALTER VIEW"
    "ANALYZE"
    "BEGIN"
    "CHECKPOINT"
    "CLOSE"
    "CLUSTER"
    "COMMENT"
    "COMMIT"
    "COMMIT PREPARED"
    "COPY"
    "CREATE AGGREGATE"
    "CREATE CAST"
    "CREATE COLLATION"
    "CREATE CONVERSION"
    "CREATE DATABASE"
    "CREATE DOMAIN"
    "CREATE EVENT TRIGGER"
    "CREATE EXTENSION"
    "CREATE FOREIGN DATA WRAPPER"
    "CREATE FOREIGN TABLE"
    "CREATE FUNCTION"
    "CREATE GROUP"
    "CREATE INDEX"
    "CREATE LANGUAGE"
    "CREATE OPERATOR"
    "CREATE OPERATOR CLASS"
    "CREATE OPERATOR FAMILY"
    "CREATE ROLE"
    "CREATE RULE"
    "CREATE SCHEMA"
    "CREATE SEQUENCE"
    "CREATE SERVER"
    "CREATE TABLE"
    "CREATE TABLE AS"
    "CREATE TABLESPACE"
    "CREATE TEXT SEARCH CONFIGURATION"
    "CREATE TEXT SEARCH DICTIONARY"
    "CREATE TEXT SEARCH PARSER"
    "CREATE TEXT SEARCH TEMPLATE"
    "CREATE TRIGGER"
    "CREATE TYPE"
    "CREATE USER"
    "CREATE USER MAPPING"
    "CREATE VIEW"
    "DEALLOCATE"
    "DECLARE"
    "DELETE"
    "DISCARD"
    "DO"
    "DROP AGGREGATE"
    "DROP CAST"
    "DROP COLLATION"
    "DROP CONVERSION"
    "DROP DATABASE"
    "DROP DOMAIN"
    "DROP EVENT TRIGGER"
    "DROP EXTENSION"
    "DROP FOREIGN DATA WRAPPER"
    "DROP FOREIGN TABLE"
    "DROP FUNCTION"
    "DROP GROUP"
    "DROP INDEX"
    "DROP LANGUAGE"
    "DROP OPERATOR"
    "DROP OPERATOR CLASS"
    "DROP OPERATOR FAMILY"
    "DROP OWNED"
    "DROP ROLE"
    "DROP RULE"
    "DROP SCHEMA"
    "DROP SEQUENCE"
    "DROP SERVER"
    "DROP TABLE"
    "DROP TABLESPACE"
    "DROP TEXT SEARCH CONFIGURATION"
    "DROP TEXT SEARCH DICTIONARY"
    "DROP TEXT SEARCH PARSER"
    "DROP TEXT SEARCH TEMPLATE"
    "DROP TRIGGER"
    "DROP TYPE"
    "DROP USER"
    "DROP USER MAPPING"
    "DROP VIEW"
    "END"
    "EXECUTE"
    "EXPLAIN"
    "FETCH"
    "GRANT"
    "INSERT"
    "LISTEN"
    "LOAD"
    "LOCK"
    "MOVE"
    "NOTIFY"
    "PREPARE"
    "PREPARE TRANSACTION"
    "REASSIGN OWNED"
    "REINDEX"
    "RELEASE SAVEPOINT"
    "RESET"
    "REVOKE"
    "ROLLBACK"
    "ROLLBACK PREPARED"
    "ROLLBACK TO SAVEPOINT"
    "SAVEPOINT"
    "SECURITY LABEL"
    "SELECT"
    "SELECT INTO"
    "SET"
    "SET CONSTRAINTS"
    "SET ROLE"
    "SET SESSION AUTHORIZATION"
    "SET TRANSACTION"
    "SHOW"
    "START TRANSACTION"
    "TABLE"
    "TRUNCATE"
    "UNLISTEN"
    "UPDATE"
    "VACUUM"
    "VALUES"
    "WITH")
  "List of PostgreSQL SQL commands, used to find start of current query")

;;;###autoload
(defun pgdev-current-func ()
  "Return a list of begin and end position of SQL function at point, if any"
  (save-excursion
    (let* ((current-pos (point))
	   (prev-create-function
	    (progn
	      (beginning-of-line)
	      (if (looking-at "create.*function") (point)
		(re-search-backward "create.*function" nil 'noerror))))
	   (open-as-$$
	    (when prev-create-function
	      ;; limit the search to next semi-colon
	      (let ((next-semi-col (re-search-forward ";" nil t)))
		(goto-char prev-create-function)
		(re-search-forward "AS[^$]*\\$\\([^$\n]*\\)\\$" next-semi-col t))))
	   ($$-name
	    (when open-as-$$ (match-string-no-properties 1)))
	   (close-as-$$
	    (when open-as-$$
	      (re-search-forward (format "\\$%s\\$" $$-name) nil t)
	      (point)))
	   (terminal-semicolon
	    (when close-as-$$
	      (re-search-forward ";" nil t) (point))))
      ;; return function's boundaries, or nil if point is not in a function
      (when (and open-as-$$ close-as-$$
		 (<= prev-create-function current-pos)
		 (<= current-pos terminal-semicolon))
	(list prev-create-function terminal-semicolon)))))

;;;###autoload
(defun pgdev-beginning-of-query ()
  "Move backward to the beginning of a query."
  ;; first search of a create function as that could embed about any other
  ;; SQL command (see PLpgSQL), then look at other cases.
  (interactive)
  ;; when already looking-at the beginning of a query, move backward one
  ;; char before the previous semicolon
  (when (looking-at (regexp-opt *pgdev-sql-commands*))
    (re-search-backward (rx (or buffer-start ";"))))
  ;; now the real search
  (let ((current-func (pgdev-current-func)))
    (if current-func
	(goto-char (first current-func))
      ;; not editing a function
      (re-search-backward (rx (or buffer-start ";")))
      (when (looking-at ";") (forward-char)) ; skip the semicolon itself
      (while
	  ;; skip blanks and comments
	  (or
	   (while (looking-at (rx (or (any blank space) eol)))
	     (forward-char))
	   (while (looking-at (rx "--" (* not-newline) eol))
	     (forward-line))))
      (unless (looking-at (regexp-opt *pgdev-sql-commands*))
	(error "Couldn't find beginning of current SQL query.")))))

;;;###autoload
(defun pgdev-end-of-query (&optional arg)
  "Move forward to the end of a top level query."
  ;; take care of semicolons embedded in a literal
  (interactive)
  (let ((current-func (pgdev-current-func)))
    (if current-func
	(goto-char (second current-func))
      ;; not a function, usual simpler rules
      (pgdev-beginning-of-query)
      (let* ((begin (point)))
	(re-search-forward ";" nil t)))))

;;;###autoload
(defun pgdev-mark-query ()
  "Put mark at end of the current SQL query and point at beginning"
  (interactive)
  (pgdev-beginning-of-query)
  (push-mark (point))
  (pgdev-end-of-query)
  (exchange-point-and-mark))

;;;###autoload
(defvar *pgdev-eval-last-branch* nil)

;;;###autoload
(defun pgdev-reset-eval-branch ()
  "reset where to evaluate the next SQL query"
  (interactive)
  (setq *pgdev-eval-last-branch* nil))

;;;###autoload
(defun pgdev-eval-query (&optional branch)
  "Evaluate PostgreSQL query at point in target psql buffer"
  (interactive)
  (let* ((branch (or *pgdev-eval-last-branch*
		     (pgdev-read-branch-name)))
	 (buf    (format "*Shell: PostgreSQL %s*" branch))
	 (dummy  (pgdev-mark-query))
	 (query  (buffer-substring-no-properties (region-beginning)
						 (region-end))))
    (with-current-buffer buf
      (setq *pgdev-eval-last-branch* branch)
      (insert (format "%s" query))
      (comint-send-input nil t))))

;;;###autoload
(defvar pgdev-sql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-a")    'pgdev-beginning-of-query)
    (define-key map (kbd "C-M-e")    'pgdev-end-of-query)
    (define-key map (kbd "C-M-h")    'pgdev-mark-query)
    (define-key map (kbd "C-M-x")    'pgdev-eval-query)
    map)
  "Keymap for `pgdev-sql-mode'.")

;;;###autoload
(define-minor-mode pgdev-sql-mode
  :keymap pgdev-sql-mode-map)

;;;###autoload
(add-hook 'sql-mode 'pgdev-sql-mode)

(provide 'pgdevenv)

