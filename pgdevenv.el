;;; pgdevenv.el --- Manage your PostgreSQL development envs
;;
;; Copyright (C) 2012 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; GIT: https://github.com/dimitri/pgdevenv-el
;; Version: 1.0
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

(defcustom pgdev-ccache-path "/sw/bin/ccache"
  "Local path leading to `ccache'"
  :group 'pgdev)

(defcustom pgdev-gdb-path "/usr/bin/gdb"
  "Local path leading to `gdb'"
  :group 'pgdev)

(defcustom pgdev-configure-option-list
  '("--enable-cassert"
    "--enable-debug"
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
  (let* ((cwd  (expand-file-name branch pgdev-install-root))
	 (buf  (format "*Shell: PostgreSQL %s*" branch)))
    (destructuring-bind (branch dir git-branch port url)
	(pgdev-get-full-details branch)
      ;; branch working directory
      (let* ((bwd (expand-file-name dir pgdev-clone-root))
	     (default-directory (file-name-as-directory bwd)))
	;; create the target branch working directory if necessary
	(unless (file-directory-p bwd) (mkdir bwd 'parents))

	;; Set the Environment prior to starting the shell
	(setenv "PAGER" "/bin/cat")
	(setenv "PGDATA" (expand-file-name "data" cwd))
	(setenv "PGPORT" port)
	(setenv "CC" (format "%s %s" pgdev-ccache-path pgdev-cc-path))
	(pgdev-set-path branch)

	;; now start the shell, then set some buffer-local variables
	(with-current-buffer (shell buf)
	  ;; add some keymap entries
	  (define-key shell-mode-map (kbd "C-c - c") 'pgdev-insert-configure)
	  (define-key shell-mode-map (kbd "C-c - g") 'pgdev-insert-git-clone)
	  (define-key shell-mode-map (kbd "C-c - s") 'pgdev-insert-pgctl-start)
	  (define-key shell-mode-map (kbd "C-c - r") 'pgdev-install-and-restart)
	  (define-key shell-mode-map (kbd "C-c - D") 'pgdev-debug)
	  (define-key shell-mode-map (kbd "C-c - d") 'pgdev-debug-this-psql)

	  ;; make local buffer variables to ease coding the keymap entries
	  (let ((pgdev-current-prefix cwd)
		(pgdev-current-clone-root
		 (expand-file-name dir pgdev-clone-root))
		(pgdev-current-branch branch)
		(pgdev-current-git-branch git-branch)
		(pgdev-current-port port)
		(pgdev-current-url url))
	    (make-local-variable 'pgdev-current-prefix)
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
  "Insert pg_ctl -l <logfile> start"
  (interactive)
  (insert (format "pg_ctl -l %s/pgsql-%s.log start"
		  pgdev-logfile-root
		  pgdev-current-branch)))

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
  (sleep-for 1)				; wait our answer
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
      (insert (format "%s -p %s" pgdev-gdb-path pid))
      (comint-send-input nil t))))

;; finally, our entry point

;;;###autoload
(global-set-key (kbd "C-c - n") 'pgdev-open-shell)

(provide 'pgdevenv)

