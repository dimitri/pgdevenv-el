# PostgreSQL Development tool for Emacs

I'm a PostgreSQL Contributor, and an Emacs user. When developing PostgreSQL
I tend to do lots of repetitive tasks. So much so that I want to automate
them.

This facility allows you to have shortcuts to easily use your own PostgreSQL
environments when working on the source code. It will define the `PGPORT`,
`PGDATA` and `PATH` for you, and also `CC` while at it, in a newly opened
`M-x shell` session. It's made to easily manage a separate `M-x shell`
session per branch you're working on.

The main interest is that this tool will then bind some keys to type in
repeatitive stanzas automatically so that you don't have to, including
getting the current *backend pid* and opening a parallel `gdb` session
attached to it.

# pgdevenv

The `pgdevenv` feature aims at providing shortcuts to use in a `M-x shell`
session in order to help hacking PostgreSQL source code. It takes care of
the most frequent commands you're going to be using.

## Usage

Main shortcut:

    C-c - n     pgdev-open-shell

Extra shortcuts available from the special shell:

    C-c - g		pgdev-insert-git-clone
    C-c - c		pgdev-insert-configure
    C-c - f		pgdev-edit-config
    C-c - s		pgdev-insert-pgctl-start
    C-c - S		pgdev-insert-pgctl-stop
    C-c - r     pgdev-install-and-restart
    C-c - R		pgdev-reinitdb
    C-c - m		pgdev-make
    C-c - M		pgdev-maintainer-clean-rebuild
    C-c - d		pgdev-debug-this-psql
    C-c - D		pgdev-debug

## Setup

The main thing you want to review adapt are those couple of *custom*
variables:

~~~lisp
(defcustom pgdev-clone-root "~/dev/PostgreSQL"
  "Top directory where to git clone the PostgreSQL development branches"
  :group 'pgdev)

(defcustom pgdev-install-root "~/pgsql"
  "Top directory where to git clone the PostgreSQL development branches"
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

(defcustom pgdev-make-extra-options "-j 4"
  "Extra options to give make for"
  :group 'pgdev)
~~~

As you can see the facility targets PostgreSQL developpers having their own
private fork where they develop features in so called *git feature
branches*, and also want to be able to occasionnaly work on a *stable
branch*, while fixing a bug typically.

You might be also interrested into changing those:

~~~lisp
(defcustom pgdev-my-git-url "https://github.com/dimitri/postgres.git"
  "URL of the your own repository"
  :group 'pgdev)

(defcustom pgdev-pg-git-url "git://git.postgresql.org/git/postgresql.git"
  "URL of the PostgreSQL upstream repository (or your mirror of it)"
  :group 'pgdev)
~~~

As you can see the default `pgdev-my-git-url` setting might not work for you...

## Clones

I didn't yet try to do something really smart about multiple cloning here. I
know I want separate branches in separate directories when it's time to
prepare a patch to fix a bug, and I've heard that using `git` it's possible
to have a kind of a *master shallow clone* to rule them all. I guess doing
some manual steps and then pointing `pgdev-my-git-url` to a local git
repository would do it though.

If you want to have a try and tell me how it works for you, be my guest.


Hope you enjoy! (Yes I know the audience is not huge.)

# pgsql-minor-mode

That's a minor mode offering support for some advanced SQL editing options:

    C-M-a       pgsql-beginning-of-query
    C-M-e       pgsql-end-of-query
    C-M-h       pgsql-mark-query
    C-M-x       pgsql-eval-query

Equiped with that you can rejoice and use those shortcuts that work so well
in other programming modes, particulary the lisp based ones.

The evaluation of a query works by sending the query text to a buffer in
which the `psql` interactive command is running, typically a `M-x shell`
session prepared with the previous tools: `pgdevenv`. 

The `C-M-x` shortcut when used for the first time in a SQL buffer will ask
you which buffer to *send* the query in, providing you with `pgdevenv`
environments auto completion.
