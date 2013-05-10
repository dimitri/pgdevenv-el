;;; pgsql-minor-mode.el --- Allow C-M-h and C-M-x on SQL queries.
;;
;; Copyright (C) 2012 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; GIT: https://github.com/dimitri/pgdevenv-el
;; Version: 1.1
;; Created: 2012-09-28
;; Keywords: emacs postgresql eval-query
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.

(require 'cl)				; of course.

;;
;; Some editing help, in a pgsql minor mode
;;
;; C-M-h   mark-pgsql-query
;; C-M-x   eval-pgsql-query
;;

(defvar *pgsql-end-of-query-rx* '(or ";" (and "\\" ".")))
(defvar *pgsql-end-of-query-re* (rx (eval *pgsql-end-of-query-rx*)))

(defvar *pgsql-commands*
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

(defun pgsql-current-func ()
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
(defun pgsql-beginning-of-query (&optional dontmove)
  "Move backward to the beginning of a query."
  ;; first search of a create function as that could embed about any other
  ;; SQL command (see PLpgSQL), then look at other cases.
  (interactive)

  ;; when already looking-at the beginning of a query, move backward one
  ;; char before the previous semicolon
  (when (and (not dontmove)
	     (or (looking-at (regexp-opt *pgsql-commands*))
		 (looking-at (rx buffer-end))))
    (re-search-backward (rx (or buffer-start
				(eval *pgsql-end-of-query-rx*)))))

  ;; now the real search
  (let ((current-func (pgsql-current-func)))
    (if current-func
	(goto-char (first current-func))
      ;; not editing a function
      (re-search-backward (rx (or buffer-start
				  (eval *pgsql-end-of-query-rx*))))
      (when (looking-at *pgsql-end-of-query-re*)
	(goto-char (match-end 0))) ; skip the semicolon itself
      ;; skip blanks and comments
      (while (looking-at (rx (or (any blank space) eol "--")))
	(while (looking-at (rx (or (any blank space) eol)))
	  (forward-char))
	(while (looking-at (rx "--" (* not-newline) eol))
	  (forward-line)))
      (unless (looking-at (regexp-opt *pgsql-commands*))
	(error "Couldn't find beginning of current SQL query.")))))

;;;###autoload
(defun pgsql-end-of-query (&optional arg)
  "Move forward to the end of a top level query."
  ;; TODO: take care of semicolons embedded in a literal
  (interactive)
  (let ((current-func (pgsql-current-func)))
    (if current-func
	(goto-char (second current-func))
      ;; not a function, usual simpler rules
      (pgsql-beginning-of-query 'dontmove)
      (let* ((begin (point)))
	(re-search-forward *pgsql-end-of-query-re* nil t)))))

;;;###autoload
(defun pgsql-mark-query ()
  "Put mark at end of the current SQL query and point at beginning"
  (interactive)
  (pgsql-beginning-of-query 'dontmove)
  (push-mark (point))
  (pgsql-end-of-query)
  (exchange-point-and-mark))

;;;###autoload
(defvar *pgsql-eval-last-branch* nil)

;;;###autoload
(defun pgsql-reset-eval-branch ()
  "reset where to evaluate the next SQL query"
  (interactive)
  (setq *pgsql-eval-last-branch* nil))

;;;###autoload
(defun pgsql-eval-query (&optional branch)
  "Evaluate PostgreSQL query at point in target psql buffer"
  (interactive)
  (save-excursion
    (let* ((branch (or *pgsql-eval-last-branch*
		       (pgsql-read-branch-name)))
	   (buf    (format "*Shell: PostgreSQL %s*" branch))
	   (dummy  (pgsql-mark-query))
	   (query  (buffer-substring-no-properties (region-beginning)
						   (region-end)))
	   (query-no-tabs (with-temp-buffer
			    (insert query)
			    (untabify (point-min) (point-max))
			    (buffer-string))))
      (with-current-buffer buf
	(setq *pgsql-eval-last-branch* branch)
	(insert (format "%s" query-no-tabs))
	(comint-send-input nil t)))))

(defvar pgsql-mode-is-on nil
  "Buffer local variable to store pgsql-mode state.")

(defvar pgsql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-a")    'pgsql-beginning-of-query)
    (define-key map (kbd "C-M-e")    'pgsql-end-of-query)
    (define-key map (kbd "C-M-h")    'pgsql-mark-query)
    (define-key map (kbd "C-M-x")    'pgsql-eval-query)
    map)
  "keymap for `pgsql-mode'.")

;;;###autoload
(define-minor-mode pgsql-mode
  "postgresql sql minor mode."
  :global nil
  :variable pgsql-mode-is-on
  :keymap pgsql-mode-map
  (make-local-variable 'pgsql-mode-is-on))

(provide 'pgsql-minor-mode)
