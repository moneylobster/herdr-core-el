;;; herdr-core.el --- Emacs client for herdr-core -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The herdr-core.el authors

;; Author: herdr-core.el contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools, terminals
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; herdr-core.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; herdr-core.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; herdr-core.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs client for herdr-core: the JSON-RPC terminal runtime behind the
;; Herdr app.  `hc connect' is spoken to over one JSON object per line, over
;; SSH or a local shell.
;;
;; The machine that runs `hc' is called the agent machine.  `herdr-core-host'
;; names it over SSH; nil means this machine.  A terminal is a process the
;; core owns, identified by an address; the package caches them and lists
;; them in a dashboard (`herdr-core-dashboard').
;;
;; From the dashboard a terminal can be attached to a real terminal emulator
;; (`herdr-core-attach'), read as text (`herdr-core-terminal'), sent messages
;; (`herdr-core-send', `herdr-core-compose') or controlled with keys,
;; interrupt and kill.  `herdr-core-mode' keeps the connections and the cache
;; fresh and shows agent counts in the mode line.
;;
;; `herdr-core-project-spawn-agent' starts an agent in the current project's
;; directory on the agent machine; `herdr-core-project-agent' attaches it.

;;; Code:

(require 'herdr-core-context)
(require 'tabulated-list)
(require 'json nil t)
(require 'project nil t)

(declare-function herdr-core--attach-impl "herdr-core-attach" (address))
(declare-function herdr-core--attach-backend "herdr-core-attach" ())
(declare-function herdr-core--attach-command "herdr-core-attach" (address))
(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function vterm "vterm" (&optional buffer-name))
(declare-function eat-exec "eat" (buffer name command startfile switches))
(declare-function notifications-notify "notifications" (&rest params))
(declare-function alert "alert" (message &rest args))
(declare-function json-parse-string "json" (string &rest args))
(declare-function json-serialize "json" (object &rest args))
(declare-function project-root "project" (project))
(declare-function project-current "project" (&optional maybe-prompt directory))

;;; Customization

(defgroup herdr-core nil
  "Emacs client for herdr-core."
  :group 'tools
  :prefix "herdr-core-")

(defcustom herdr-core-host nil
  "SSH target of the agent machine, or nil for a local core.
The agent machine is the machine that runs `hc'.  For example \"agentbox\"
or \"user@host:22\"."
  :type '(choice (const :tag "Local" nil) string)
  :group 'herdr-core)

(defcustom herdr-core-ssh-program "ssh"
  "SSH program used to reach the herdr-core machine."
  :type 'string
  :group 'herdr-core)

(defcustom herdr-core-ssh-args
  '("-T" "-o" "BatchMode=yes" "-o" "ConnectTimeout=10"
    "-o" "ServerAliveInterval=15" "-o" "ServerAliveCountMax=3")
  "Arguments passed to `herdr-core-ssh-program' for the JSON-RPC connection."
  :type '(repeat string)
  :group 'herdr-core)

(defun herdr-core--default-command (action)
  "Return the default hc ACTION command.
`hc' is taken from `PATH' on every system: the local core is the default, and
the same name works over SSH when the remote `PATH' has it.  Point the
defcustom at an absolute path when it does not."
  (concat "hc " action))

(defcustom herdr-core-remote-command (herdr-core--default-command "connect")
  "Command that speaks JSON-RPC on the herdr-core machine.
With `herdr-core-host' nil it runs on this machine (through a shell, or as
argv on Windows, where it must be absolute or on the `PATH': no `$HOME' or
`~').  With a host it runs in the remote shell."
  :type 'string
  :group 'herdr-core)

(defcustom herdr-core-attach-command (herdr-core--default-command "attach")
  "Command used to attach a terminal in a real terminal emulator.
The local core is the default; with `herdr-core-host' it runs over ssh."
  :type 'string
  :group 'herdr-core)

(defcustom herdr-core-attach-ssh-args '("-t" "-o" "BatchMode=yes" "-o" "ConnectTimeout=10")
  "Arguments passed to ssh for interactive attachment."
  :type '(repeat string)
  :group 'herdr-core)

(defcustom herdr-core-session nil
  "Session name passed to `hc'; nil uses the default session."
  :type '(choice (const :tag "Default" nil) string)
  :group 'herdr-core)

(defcustom herdr-core-request-timeout 20
  "Seconds to wait for a JSON-RPC reply."
  :type 'number
  :group 'herdr-core)

(defcustom herdr-core-refresh-interval 1.5
  "Seconds between refreshes of a working terminal view."
  :type 'number
  :group 'herdr-core)

(defcustom herdr-core-poll-interval 5
  "Seconds between polls when events are unavailable."
  :type 'number
  :group 'herdr-core)

(defcustom herdr-core-history-lines 1000
  "Lines of history requested for a terminal that is not working."
  :type 'integer
  :group 'herdr-core)

(defcustom herdr-core-notify t
  "Whether to notify when an agent becomes blocked or fails.
Notifications are skipped for the terminal you are looking at."
  :type 'boolean
  :group 'herdr-core)

(defcustom herdr-core-image-max-bytes 8388608
  "Largest file, in bytes, that `herdr-core-put-file' sends."
  :type 'integer
  :group 'herdr-core)

(defcustom herdr-core-context-max-chars 120000
  "Largest rendered context, in characters."
  :type 'integer
  :group 'herdr-core)

(defcustom herdr-core-attach-backend 'auto
  "Terminal backend used by `herdr-core-attach'.
`auto' prefers ghostel, then vterm, then eat, then the text mirror."
  :type '(choice (const auto) (const ghostel) (const vterm) (const eat) (const mirror))
  :group 'herdr-core)

(defcustom herdr-core-attach-observe nil
  "Whether `herdr-core-attach' is read-only.
When non-nil, --observe is passed to hc attach: the terminal is shown but
cannot take input."
  :type 'boolean
  :group 'herdr-core)

(defcustom herdr-core-attach-history 500
  "Lines of history passed to hc attach; nil passes none."
  :type '(choice (const :tag "None" nil) integer)
  :group 'herdr-core)

(defcustom herdr-core-agent-commands '("omp" "claude" "codex" "pi")
  "Agent commands offered by `herdr-core-spawn-agent'.
Templates known to the core are offered as well."
  :type '(repeat string)
  :group 'herdr-core)

(defcustom herdr-core-project-root-map nil
  "Alist mapping local project roots to directories on the agent machine.
Each element is (LOCAL-ROOT . AGENT-ROOT).  LOCAL-ROOT is a directory on
this computer, AGENT-ROOT the directory holding the same files on the
machine that runs `hc'.  The longest matching LOCAL-ROOT wins.  It is
consulted when the project has no TRAMP root on `herdr-core-host'."
  :type '(alist :key-type string :value-type string)
  :group 'herdr-core)

(defcustom herdr-core-default-directory nil
  "Initial answer when a directory on the agent machine is asked for."
  :type '(choice (const :tag "None" nil) string)
  :group 'herdr-core)

(defcustom herdr-core-adopt-space t
  "Where terminals started here go in the Herdr App.
t (the default) gives each project a space of its own.  The space is the
one named after the current project or the spawn directory, else the one
whose first terminal works in that directory, else the one made in that
directory; when none matches, a new space is made.  Every terminal gets a
new tab in that space, so a second agent for a project opens a tab rather
than a pane in the first tab.

A string names the space to use, for example \"milvus\".  nil leaves
terminals out of the app's spaces: the core runs them and this package
lists them, but Herdr App does not show them.

Adoption writes the `herdr.space' and `herdr.tab' labels that Herdr App
uses to place a terminal, so the app shows it as a tab in that space and
keeps it."
  :type '(choice (const :tag "Do not adopt" nil)
                 (const :tag "One space per project" t)
                 string)
  :group 'herdr-core)

;;; Faces

(defface herdr-core-working '((t :foreground "#e8912a" :weight bold))
  "Face for a working agent."
  :group 'herdr-core)

(defface herdr-core-blocked '((t :foreground "#e5484d" :weight bold))
  "Face for a blocked or failed agent."
  :group 'herdr-core)

(defface herdr-core-done '((t :foreground "#30a46c"))
  "Face for a done agent status."
  :group 'herdr-core)

(defface herdr-core-idle '((t :foreground "gray60"))
  "Face for an idle terminal."
  :group 'herdr-core)

(defface herdr-core-finished '((t :foreground "gray40"))
  "Face for a finished terminal."
  :group 'herdr-core)

;;; Shared state

(defvar herdr-core-mode)

(defvar-local herdr-core--target nil
  "Terminal address of the current herdr buffer.")

(defvar-local herdr-core--last-text nil
  "Last screen text shown in the mirror view.")

(defvar-local herdr-core--auto-refresh t
  "Non-nil when the mirror view refreshes itself.")

(defvar-local herdr-core--last-read 0
  "Time of the last successful read in the mirror view.")

(defvar herdr-core--debounce-timer nil)
(defvar herdr-core--safety-timer nil)
(defvar herdr-core--view-timer nil)
(defvar herdr-core--poll-timer nil)
(defvar herdr-core--events-retry-timer nil)
(defvar herdr-core--events-subscribed nil
  "Non-nil when events.subscribe succeeded.")

(defvar herdr-core--last-error nil
  "Last connection or refresh error, as a JSON-RPC error alist.")

(defvar herdr-core--last-stderr nil
  "Last line of the core's stderr, kept for the dashboard header.")

;;; JSON

(defun herdr-core--get (object key)
  "Return KEY from JSON OBJECT, an alist."
  (cdr (assoc key object)))

(defun herdr-core--true-p (value)
  "Return non-nil when VALUE is JSON true."
  (and value (not (eq value :false))))

(defun herdr-core--json-string-keys (object)
  "Return parsed OBJECT with string keys and lists for arrays.
`json-parse-string' yields symbol keys in alists and vectors for arrays,
while the rest of the package reads keys with `assoc' on strings."
  (cond
   ((vectorp object)
    (mapcar #'herdr-core--json-string-keys (append object nil)))
   ((consp object)
    (mapcar (lambda (pair)
              (cons (if (symbolp (car pair))
                        (symbol-name (car pair))
                      (car pair))
                    (herdr-core--json-string-keys (cdr pair))))
            object))
   (t object)))

(defun herdr-core--json-parse (string)
  "Parse STRING as one JSON object."
  (condition-case err
      (if (fboundp 'json-parse-string)
          (herdr-core--json-string-keys
           (funcall 'json-parse-string string
                    :object-type 'alist :array-type 'array
                    :null-object nil :false-object :false))
        (require 'json)
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'string)
              (json-null nil)
              (json-false :false))
          (json-read-from-string string)))
    (error
     (herdr-core--report-error "json" (error-message-string err))
     nil)))

(defun herdr-core--json-normalize (object)
  "Return OBJECT in the shape `json-serialize' accepts.
Requests are built as alists with string keys and plain lists for arrays,
but `json-serialize' wants symbol keys and vectors.  Plists and alists with
symbol keys keep their keys; values are normalized recursively."
  (cond
   ((vectorp object)
    (vconcat (mapcar #'herdr-core--json-normalize object)))
   ((consp object)
    (cond
     ((keywordp (car object))           ; plist
      (let (result)
        (while (consp object)
          (push (car object) result)
          (push (herdr-core--json-normalize (cadr object)) result)
          (setq object (cddr object)))
        (nreverse result)))
     ((consp (car object))              ; alist
      (mapcar (lambda (pair)
                (cons (if (stringp (car pair))
                          (intern (car pair))
                        (car pair))
                      (herdr-core--json-normalize (cdr pair))))
              object))
     (t                                 ; array
      (vconcat (mapcar #'herdr-core--json-normalize object)))))
   (t object)))

(defun herdr-core--json-encode (object)
  "Encode OBJECT as UTF-8 bytes.
Nil encodes as JSON null, matching `herdr-core--json-parse'."
  (let* ((object (herdr-core--json-normalize object))
         (string (if (fboundp 'json-serialize)
                     (funcall 'json-serialize object :null-object nil)
                   (require 'json)
                   (let ((json-false :false)
                         (json-null nil))
                     (json-encode object)))))
    (if (multibyte-string-p string)
        (encode-coding-string string 'utf-8)
      string)))

;;; Errors

(defvar herdr-core--last-reported nil
  "Last error reported, as (TEXT . TIME).")

(defun herdr-core--report-error (context message)
  "Report MESSAGE once per minute."
  (let ((text (format "hc: %s: %s" context message)))
    (unless (and herdr-core--last-reported
                 (equal (car herdr-core--last-reported) text)
                 (< (- (float-time) (cdr herdr-core--last-reported)) 60))
      (setq herdr-core--last-reported (cons text (float-time)))
      (message "%s" text))))

(defun herdr-core--error-kind (error)
  "Return the kind of JSON-RPC ERROR."
  (herdr-core--get (herdr-core--get error "data") "kind"))

(defun herdr-core--error-message (error)
  "Return the message of JSON-RPC ERROR."
  (or (herdr-core--get error "message") "unknown error"))

(defun herdr-core--first-line (string)
  "Return the first line of STRING, trimmed."
  (let ((line (car (split-string (or string "") "\n" t))))
    (and line (replace-regexp-in-string "\\`[ \t]+\\|[ \t]+\\'" "" line))))

;;; Transport

(defvar herdr-core--api nil
  "Connection plist for requests.")

(defvar herdr-core--events nil
  "Connection plist for events.")

(defun herdr-core--posix-quote (string)
  "Quote STRING for a POSIX shell, whatever the local system is."
  (concat "'" (replace-regexp-in-string "'" "'\\''" string nil t) "'"))

(defun herdr-core--local-argv (command variable)
  "Return COMMAND as argv for a local core.
On Windows there is no shell to expand `$HOME', so COMMAND must name the
program directly; VARIABLE names the defcustom for error messages."
  (if (eq system-type 'windows-nt)
      (progn
        (when (string-match-p "\\$\\|\\`~" command)
          (user-error "Set %s to an absolute path on Windows (no $HOME or ~)"
                      variable))
        (split-string-and-unquote command))
    (list "sh" "-c" command)))

(defun herdr-core--connect-line ()
  "Return the `hc connect' command line for a remote shell."
  (concat herdr-core-remote-command
          (when herdr-core-session
            (concat " --session "
                    (herdr-core--posix-quote herdr-core-session)))))

(defun herdr-core--command ()
  "Return the argv that runs `hc connect'."
  (cond
   (herdr-core-host
    (append (list herdr-core-ssh-program)
            herdr-core-ssh-args
            (list herdr-core-host (herdr-core--connect-line))))
   ((eq system-type 'windows-nt)
    (append (herdr-core--local-argv herdr-core-remote-command
                                    'herdr-core-remote-command)
            (when herdr-core-session (list "--session" herdr-core-session))))
   (t
    (list "sh" "-c" (herdr-core--connect-line)))))

(defun herdr-core--conn-role (conn)
  "Return the role of CONN."
  (plist-get conn :role))

(defun herdr-core--conn-for (process)
  "Return the connection that owns PROCESS."
  (cond
   ((and herdr-core--api (eq (plist-get herdr-core--api :proc) process))
    herdr-core--api)
   ((and herdr-core--events (eq (plist-get herdr-core--events :proc) process))
    herdr-core--events)))

(defun herdr-core--conn-put (conn key value)
  "Set KEY to VALUE in CONN and store CONN back in its global.
Return the updated connection."
  (let ((new (plist-put (copy-sequence conn) key value)))
    (if (eq (herdr-core--conn-role new) 'api)
        (setq herdr-core--api new)
      (setq herdr-core--events new))
    new))

(defun herdr-core--ensure (role)
  "Return a live connection for ROLE, creating it when needed."
  (let ((conn (if (eq role 'api) herdr-core--api herdr-core--events)))
    (if (and conn (plist-get conn :alive) (process-live-p (plist-get conn :proc)))
        conn
      (let* ((name (if (eq role 'api) "herdr-core" "herdr-core-events"))
             (stderr (get-buffer-create
                      (format " *herdr-core-%s-stderr*" role)))
             (process (make-process
                       :name name
                       :buffer (get-buffer-create " *herdr-core-io*")
                       :command (herdr-core--command)
                       :connection-type 'pipe
                       :coding 'binary
                       :noquery t
                       :stderr stderr
                       :filter #'herdr-core--filter
                       :sentinel #'herdr-core--sentinel)))
        (setq conn (list :role role :proc process :inbuf "" :pending
                         (make-hash-table :test 'equal) :next-id 1 :alive t
                         :stderr stderr))
        (when (eq role 'api)
          (setq herdr-core--last-error nil
                herdr-core--last-stderr nil))
        (if (eq role 'api)
            (setq herdr-core--api conn)
          (setq herdr-core--events conn))
        conn))))

(defun herdr-core--handle-line (conn line)
  "Handle one LINE from CONN."
  (let ((line (replace-regexp-in-string "\r\\'" "" line)))
    (unless (= (length line) 0)
      (let ((message (herdr-core--json-parse (decode-coding-string line 'utf-8))))
        (when message
          (let ((id (herdr-core--get message "id")))
            (if (and id (integerp id))
                (herdr-core--resolve conn id message)
              (herdr-core--dispatch-notification message))))))))

(defun herdr-core--resolve (conn id message)
  "Deliver MESSAGE to the pending request ID on CONN."
  (let* ((pending (plist-get conn :pending))
         (entry (gethash id pending)))
    (when entry
      (remhash id pending)
      (when (timerp (cdr entry)) (cancel-timer (cdr entry)))
      (condition-case err
          (funcall (car entry)
                   (herdr-core--get message "result")
                   (herdr-core--get message "error"))
        (error
         (herdr-core--report-error "callback" (error-message-string err)))))))

(defun herdr-core--filter (process chunk)
  "Collect CHUNK from PROCESS and handle complete lines."
  (let ((conn (herdr-core--conn-for process)))
    (when conn
      (let ((inbuf (concat (or (plist-get conn :inbuf) "") chunk))
            (position 0)
            newline)
        (while (setq newline (string-match "\n" inbuf position))
          (herdr-core--handle-line conn (substring inbuf position newline))
          (setq position (1+ newline)))
        (herdr-core--conn-put conn :inbuf (substring inbuf position))))))

(defun herdr-core--request-on (conn method params callback &optional timeout)
  "Send METHOD with PARAMS on CONN, calling CALLBACK with (RESULT ERROR)."
  (let ((process (plist-get conn :proc)))
    (if (not (and process (process-live-p process)))
        (funcall callback nil
                 (list (cons "kind" "connection_closed")
                       (cons "message" "not connected")))
      (let* ((id (plist-get conn :next-id))
             (message (append (list (cons "jsonrpc" "2.0")
                                    (cons "id" id)
                                    (cons "method" method))
                              (when params (list (cons "params" params)))))
             (line (concat (herdr-core--json-encode message) "\n"))
             (pending (plist-get conn :pending))
             (timer (run-at-time
                     (or timeout herdr-core-request-timeout) nil
                     (lambda ()
                       (let ((entry (gethash id (plist-get conn :pending))))
                         (when entry
                           (remhash id (plist-get conn :pending))
                           (condition-case nil
                               (funcall (car entry) nil
                                        (list (cons "kind" "client_timeout")
                                              (cons "message" "no reply")))
                             (error nil))))))))
        (puthash id (cons callback timer) pending)
        (herdr-core--conn-put conn :next-id (1+ id))
        (condition-case err
            (process-send-string process line)
          (error
           (remhash id pending)
           (cancel-timer timer)
           (funcall callback nil
                    (list (cons "kind" "connection_closed")
                          (cons "message" (error-message-string err))))))))))

(defun herdr-core--request (method params callback &optional timeout)
  "Send METHOD with PARAMS on the request connection.
CALLBACK is called with (RESULT ERROR)."
  (condition-case err
      (herdr-core--request-on (herdr-core--ensure 'api) method params callback timeout)
    (error
     (herdr-core--report-error "request" (error-message-string err))
     (funcall callback nil
              (list (cons "kind" "connection_closed")
                    (cons "message" (error-message-string err)))))))

(defun herdr-core--request-sync (method params &optional timeout)
  "Send METHOD with PARAMS and wait for the result.  For tests."
  (let ((done nil) (result nil) (error nil)
        (deadline (+ (float-time) (or timeout herdr-core-request-timeout))))
    (herdr-core--request method params
                         (lambda (r e) (setq result r error e done t))
                         timeout)
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless done
      (error "herdr-core: %s timed out" method))
    (when error
      (error "herdr-core: %s failed: %s" method (herdr-core--error-kind error)))
    result))

(defun herdr-core--show-stderr (conn)
  "Print the tail of CONN's stderr and return its last useful line."
  (let ((buffer (plist-get conn :stderr))
        (useful nil))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (line (split-string (buffer-string) "\n" t))
          (unless (string-match-p "\\`Process .* stderr finished\\'" line)
            (push line useful)))))
    (setq useful (nreverse useful))
    (when useful
      (message "hc: core stderr:\n%s"
               (mapconcat #'identity
                          (last useful (min 20 (length useful))) "\n"))
      (herdr-core--first-line (car (last useful))))))

(defun herdr-core--sentinel (process event)
  "Handle PROCESS state change EVENT."
  (let ((conn (herdr-core--conn-for process)))
    (when conn
      (herdr-core--conn-put conn :alive nil)
      (let ((pending (plist-get conn :pending))
            (ids nil))
        (maphash (lambda (id _) (push id ids)) pending)
        (dolist (id ids)
          (let ((entry (gethash id pending)))
            (when entry
              (remhash id pending)
              (when (timerp (cdr entry)) (cancel-timer (cdr entry)))
              (condition-case nil
                  (funcall (car entry) nil
                           (list (cons "kind" "connection_closed")
                                 (cons "message" event)))
                (error nil))))))
      (let ((tail (herdr-core--show-stderr conn)))
        (when (eq (herdr-core--conn-role conn) 'api)
          (setq herdr-core--last-stderr tail
                herdr-core--last-error
                (list (cons "kind" "connection_closed")
                      (cons "message" event)))))
      (herdr-core--update-mode-line)
      (when (and (eq (herdr-core--conn-role conn) 'events) herdr-core-mode)
        (herdr-core--events-schedule-retry)))))

(defun herdr-core-shutdown ()
  "Close both connections and cancel every timer."
  (interactive)
  (dolist (conn (list herdr-core--api herdr-core--events))
    (when conn
      (let ((process (plist-get conn :proc)))
        (when (process-live-p process) (delete-process process)))
      (herdr-core--conn-put conn :alive nil)))
  (dolist (timer (list herdr-core--debounce-timer
                       herdr-core--safety-timer
                       herdr-core--view-timer
                       herdr-core--poll-timer
                       herdr-core--events-retry-timer))
    (when (timerp timer) (cancel-timer timer)))
  (setq herdr-core--api nil
        herdr-core--events nil
        herdr-core--debounce-timer nil
        herdr-core--safety-timer nil
        herdr-core--view-timer nil
        herdr-core--poll-timer nil
        herdr-core--events-retry-timer nil
        herdr-core--events-subscribed nil
        herdr-core--last-error nil
        herdr-core--last-stderr nil)
  (herdr-core--update-mode-line))

;;; Cache and refresh

(defvar herdr-core--terminals (make-hash-table :test 'equal)
  "Cached terminals by address.")

(defvar herdr-core--machines nil
  "Cached machines.")

(defvar herdr-core--templates nil
  "Cached templates.")

(defvar herdr-core--templates-fetched nil
  "Non-nil once templates were fetched.")

(defvar herdr-core--all nil
  "Non-nil when finished terminals are listed.")

(defvar herdr-core--hidden-finished 0
  "Finished terminals hidden by the core.")

(defvar herdr-core--seen-turn (make-hash-table :test 'equal)
  "Turn last seen, by address.")

(defvar herdr-core--event-count 0
  "Number of event notifications received.")

(defvar herdr-core--last-heartbeat nil
  "Time of the last heartbeat.")

(defvar herdr-core--events-failed nil
  "Non-nil when events are unavailable.")

(defvar herdr-core--events-backoff 5
  "Current retry delay for events, in seconds.")

(defvar herdr-core--events-seq nil
  "Sequence number of the last events snapshot.")

(defvar herdr-core--project-agents (make-hash-table :test 'equal)
  "Agent address remembered per local project root.")

(defvar herdr-core--last-agent nil
  "Address of the agent most recently used.")

(defun herdr-core--hash-keys (table)
  "Return the keys of TABLE."
  (let (keys)
    (maphash (lambda (key _) (push key keys)) table)
    keys))

(defun herdr-core--terminal-name (terminal &optional fallback)
  "Return the display name of TERMINAL, or FALLBACK."
  (or (and terminal (herdr-core--get terminal "name"))
      (and terminal (herdr-core--get terminal "address"))
      fallback
      "terminal"))

(defun herdr-core--terminal-live-p (address)
  "Return non-nil when ADDRESS is cached and live."
  (let ((terminal (gethash address herdr-core--terminals)))
    (and terminal (equal (herdr-core--get terminal "state") "live"))))

(defun herdr-core--working-p (terminal)
  "Return non-nil when TERMINAL is working."
  (let ((status (and terminal (herdr-core--get terminal "status"))))
    (and status (equal (herdr-core--get status "state") "working"))))

(defun herdr-core--terminal-cwd (terminal)
  "Return the working directory of TERMINAL."
  (and terminal
       (or (herdr-core--get (herdr-core--get terminal "foreground") "cwd")
           (herdr-core--get terminal "cwd"))))

(defun herdr-core--terminal-sort-ms (terminal)
  "Return the sort timestamp of TERMINAL."
  (or (herdr-core--get terminal "status_ms")
      (herdr-core--get terminal "created_ms")
      0))

(defun herdr-core--mark-seen (address)
  "Mark the current turn of ADDRESS as seen."
  (let ((terminal (gethash address herdr-core--terminals)))
    (when terminal
      (puthash address (or (herdr-core--get terminal "turn") 0)
               herdr-core--seen-turn)))
  (herdr-core--update-mode-line))

(defun herdr-core--viewing-p (address)
  "Return non-nil when a window shows the mirror view of ADDRESS."
  (let ((buffer (get-buffer (format "*hc:%s*" address))))
    (and buffer (get-buffer-window buffer))))

(defun herdr-core--notify (title body)
  "Notify BODY under TITLE when `herdr-core-notify' is on."
  (when herdr-core-notify
    (cond
     ((fboundp 'alert) (funcall 'alert body :title title :severity 'normal))
     ((fboundp 'notifications-notify)
      (funcall 'notifications-notify :title title :body body))
     (t (message "%s: %s" title body)))))

(defun herdr-core--status-message (status terminal)
  "Return a one-line status message for STATUS of TERMINAL."
  (let ((msg (or (and status (herdr-core--get status "msg")) ""))
        (title (or (and status (herdr-core--get status "title")) "")))
    (cond
     ((not (equal msg "")) msg)
     ((not (equal title "")) title)
     (t (or (herdr-core--get terminal "title") "")))))

(defun herdr-core--notify-transition (address)
  "Notify about a status change of ADDRESS."
  (let* ((terminal (gethash address herdr-core--terminals))
         (status (and terminal (herdr-core--get terminal "status")))
         (state (and status (herdr-core--get status "state"))))
    (unless (herdr-core--viewing-p address)
      (herdr-core--notify (herdr-core--terminal-name terminal address)
                          (format "%s — %s" (or state "changed")
                                  (herdr-core--status-message status terminal))))))

(defun herdr-core--apply-list (result)
  "Replace the cache with RESULT and handle transitions."
  (let ((old herdr-core--terminals)
        (new (make-hash-table :test 'equal))
        (transitions nil))
    (dolist (terminal (append (herdr-core--get result "terminals") nil))
      (puthash (herdr-core--get terminal "address") terminal new))
    (setq herdr-core--terminals new)
    (setq herdr-core--hidden-finished
          (or (herdr-core--get result "hidden_finished") 0))
    (let ((machines (herdr-core--get result "machines")))
      (when machines (setq herdr-core--machines machines)))
    (dolist (address (herdr-core--hash-keys new))
      (let* ((terminal (gethash address new))
             (previous (gethash address old))
             (status (herdr-core--get terminal "status"))
             (state (and status (herdr-core--get status "state")))
             (previous-status (and previous (herdr-core--get previous "status")))
             (previous-state (and previous-status (herdr-core--get previous-status "state")))
             (turn (or (herdr-core--get terminal "turn") 0))
             (previous-turn (or (and previous (herdr-core--get previous "turn")) 0)))
        (unless (gethash address herdr-core--seen-turn)
          (puthash address turn herdr-core--seen-turn))
        (let ((blocked (and (member state '("blocked" "error"))
                            (not (equal state previous-state))))
              (advanced (> turn previous-turn))
              (finished (and (equal (herdr-core--get terminal "state") "finished")
                             (not (and previous
                                       (equal (herdr-core--get previous "state")
                                              "finished"))))))
          (when (or blocked advanced finished)
            (push (cons address (or blocked advanced)) transitions)))))
    (dolist (transition transitions)
      (when (cdr transition)
        (herdr-core--notify-transition (car transition))))
    (maphash (lambda (address _)
               (unless (gethash address new)
                 (herdr-core--mark-gone address)))
             old)
    (herdr-core--dashboard-refresh)
    (herdr-core--update-mode-line)
    (herdr-core--refresh-views)))

(defun herdr-core--mark-gone (address)
  "Mark the view of ADDRESS as gone."
  (let ((buffer (get-buffer (format "*hc:%s*" address))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq header-line-format (format " hc:%s terminal gone" address))
        (setq herdr-core--auto-refresh nil)))))

(defun herdr-core--fetch-templates ()
  "Fetch templates once."
  (unless herdr-core--templates-fetched
    (setq herdr-core--templates-fetched t)
    (herdr-core--request "templates.list" nil
                         (lambda (result _error)
                           (when result
                             (setq herdr-core--templates
                                   (or (herdr-core--get result "templates") nil)))))))

(defun herdr-core--refresh (&optional callback)
  "Refresh the terminal cache, then call CALLBACK with the error or nil."
  (herdr-core--request
   "terminal.list"
   (list (cons "all" (if herdr-core--all t :false)))
   (lambda (result error)
     (if error
         (progn
           (setq herdr-core--last-error error)
           (herdr-core--update-mode-line)
           (herdr-core--dashboard-refresh)
           (when callback (funcall callback error)))
       (setq herdr-core--last-error nil)
       (herdr-core--apply-list result)
       (herdr-core--fetch-templates)
       (when callback (funcall callback nil))))))

;;; Events

(defun herdr-core--dispatch-notification (message)
  "Handle a notification MESSAGE."
  (let ((method (herdr-core--get message "method")))
    (cond
     ((member method '("event" "missed"))
      (setq herdr-core--event-count (1+ herdr-core--event-count))
      (herdr-core--invalidate))
     ((equal method "heartbeat")
      (setq herdr-core--last-heartbeat (float-time))))))

(defun herdr-core--invalidate ()
  "Refresh soon, coalescing bursts of events."
  (when (timerp herdr-core--debounce-timer)
    (cancel-timer herdr-core--debounce-timer))
  (setq herdr-core--debounce-timer
        (run-at-time 1 nil #'herdr-core--refresh)))

(defun herdr-core--poll-start ()
  "Refresh on a timer while events are unavailable."
  (unless (timerp herdr-core--poll-timer)
    (setq herdr-core--poll-timer
          (run-with-timer herdr-core-poll-interval herdr-core-poll-interval
                          #'herdr-core--refresh))))

(defun herdr-core--poll-stop ()
  "Stop the polling fallback."
  (when (timerp herdr-core--poll-timer)
    (cancel-timer herdr-core--poll-timer))
  (setq herdr-core--poll-timer nil))

(defun herdr-core--events-start ()
  "Subscribe to events, retrying later when that fails."
  (unless (and herdr-core--events-subscribed
               herdr-core--events
               (plist-get herdr-core--events :alive)
               (process-live-p (plist-get herdr-core--events :proc)))
    (condition-case err
        (let ((conn (herdr-core--ensure 'events)))
          (herdr-core--request-on
           conn "events.subscribe" nil
           (lambda (result error)
             (if error
                 (when (and herdr-core--events
                            (plist-get herdr-core--events :alive))
                   (setq herdr-core--events-failed t
                         herdr-core--events-subscribed nil)
                   (herdr-core--report-error "events.subscribe"
                                             (herdr-core--error-message error))
                   (herdr-core--poll-start))
               (setq herdr-core--events-failed nil
                     herdr-core--events-subscribed t
                     herdr-core--events-backoff 5
                     herdr-core--events-seq (herdr-core--get result "seq"))
               (herdr-core--poll-stop)))))
      (error
       (setq herdr-core--events-failed t)
       (herdr-core--poll-start)
       (herdr-core--report-error "events" (error-message-string err))))))

(defun herdr-core--events-schedule-retry ()
  "Try to resubscribe to events after a delay."
  (when herdr-core-mode
    (when (timerp herdr-core--events-retry-timer)
      (cancel-timer herdr-core--events-retry-timer))
    (setq herdr-core--events-subscribed nil)
    (setq herdr-core--events-retry-timer
          (run-at-time (or herdr-core--events-backoff 5) nil
                       (lambda ()
                         (setq herdr-core--events-retry-timer nil)
                         (setq herdr-core--events-backoff
                               (min 60 (* 2 (or herdr-core--events-backoff 5))))
                         (when herdr-core-mode
                           (herdr-core--events-start)))))))

;;; Status strings

(defun herdr-core--truncate (string length)
  "Truncate STRING to LENGTH characters, ending with an ellipsis."
  (if (> (length string) length)
      (concat (substring string 0 (max 0 (1- length))) "\u2026")
    string))

(defun herdr-core--status-string (terminal)
  "Return the status column text for TERMINAL."
  (let ((state (herdr-core--get terminal "state"))
        (status (herdr-core--get terminal "status")))
    (cond
     ((equal state "finished")
      (let ((code (herdr-core--get terminal "exit_code"))
            (reason (herdr-core--get terminal "reason")))
        (cond
         ((integerp code) (format "finished (exit %d)" code))
         ((and reason (not (equal reason ""))) (format "finished (%s)" reason))
         (t "finished"))))
     (status
      (let* ((status-state (or (herdr-core--get status "state") ""))
             (kind (herdr-core--get status "kind"))
             (progress (herdr-core--get status "progress"))
             (message (replace-regexp-in-string
                       "[\n\r]+" " " (herdr-core--status-message status terminal))))
        (concat status-state
                (if kind (format " (%s)" kind) "")
                (if (integerp progress) (format " %d%%" progress) "")
                (if (and message (not (equal message "")))
                    (format " %s" (herdr-core--truncate message 40))
                  ""))))
     (t "-"))))

(defun herdr-core--status-face (terminal)
  "Return the face for TERMINAL's status."
  (let ((status (herdr-core--get terminal "status"))
        (state (herdr-core--get terminal "state")))
    (cond
     ((equal state "finished") 'herdr-core-finished)
     ((null status) 'herdr-core-idle)
     ((equal (herdr-core--get status "state") "working") 'herdr-core-working)
     ((member (herdr-core--get status "state") '("blocked" "error")) 'herdr-core-blocked)
     ((equal (herdr-core--get status "state") "done") 'herdr-core-done)
     (t 'herdr-core-idle))))

(defun herdr-core--age-string (terminal)
  "Return the age of TERMINAL."
  (let ((ms (or (herdr-core--get terminal "status_ms")
                (herdr-core--get terminal "created_ms"))))
    (if (integerp ms)
        (let ((seconds (max 0 (/ (- (truncate (* (float-time) 1000)) ms) 1000))))
          (cond
           ((< seconds 60) "<60s")
           ((< seconds 3600) (format "%dm" (/ seconds 60)))
           ((< seconds 86400) (format "%dh" (/ seconds 3600)))
           (t (format "%dd" (/ seconds 86400)))))
      "-")))

;;; Dashboard

(defvar herdr-core--filter-active nil
  "Non-nil when idle, seen terminals are hidden.")

(defvar-local herdr-core--dashboard-project-root nil
  "Remote directory that the dashboard is limited to.")

(defun herdr-core--dashboard-target ()
  "Return the terminal address at point, or signal."
  (or (tabulated-list-get-id)
      (user-error "No terminal at point")))

(defun herdr-core--interactive-target ()
  "Return the target of the current buffer, or signal in a dashboard."
  (cond
   ((derived-mode-p 'herdr-core-dashboard-mode) (herdr-core--dashboard-target))
   ((derived-mode-p 'herdr-core-terminal-mode)
    (or herdr-core--target (user-error "No terminal")))
   ((derived-mode-p 'herdr-core-compose-mode) herdr-core--target)
   (t nil)))

(defun herdr-core--path-under-p (path root)
  "Return non-nil when PATH is ROOT or below it."
  (and path root
       (string-prefix-p (file-name-as-directory root)
                        (file-name-as-directory path))))

(defun herdr-core--terminal-depth (terminal)
  "Return the parent depth of TERMINAL."
  (let ((depth 0)
        (parent (herdr-core--get terminal "parent"))
        (guard 0))
    (while (and parent (< guard 8))
      (setq depth (1+ depth))
      (let ((up (gethash parent herdr-core--terminals)))
        (setq parent (and up (herdr-core--get up "parent"))))
      (setq guard (1+ guard)))
    depth))

(defun herdr-core--terminal-children (address)
  "Return the cached children of ADDRESS, oldest first."
  (let (children)
    (maphash (lambda (child-address terminal)
               (when (equal (herdr-core--get terminal "parent") address)
                 (push (cons child-address terminal) children)))
             herdr-core--terminals)
    (sort children
          (lambda (a b)
            (< (or (herdr-core--get (cdr a) "created_ms") 0)
               (or (herdr-core--get (cdr b) "created_ms") 0))))))

(defun herdr-core--terminal-roots ()
  "Return cached terminals without a cached parent, newest status first."
  (let (roots)
    (maphash (lambda (address terminal)
               (let ((parent (herdr-core--get terminal "parent")))
                 (unless (and parent (gethash parent herdr-core--terminals))
                   (push (cons address terminal) roots))))
             herdr-core--terminals)
    (sort roots
          (lambda (a b)
            (> (herdr-core--terminal-sort-ms (cdr a))
               (herdr-core--terminal-sort-ms (cdr b)))))))

(defun herdr-core--flatten-terminal (address terminal depth seen)
  "Return (ADDRESS TERMINAL DEPTH) rows for TERMINAL and its descendants."
  (let ((row (list (list address terminal depth)))
        (children nil))
    (unless (memq address seen)
      (setq children
            (mapcar (lambda (child)
                      (herdr-core--flatten-terminal
                       (car child) (cdr child) (1+ depth)
                       (cons address seen)))
                    (herdr-core--terminal-children address))))
    (append row (apply #'append children))))

(defun herdr-core--dashboard-rows ()
  "Return every cached terminal in display order."
  (let ((rows (apply #'append
                     (mapcar (lambda (root)
                               (herdr-core--flatten-terminal
                                (car root) (cdr root) 0 nil))
                             (herdr-core--terminal-roots))))
        (seen nil))
    (dolist (row rows) (push (car row) seen))
    (maphash (lambda (address terminal)
               (unless (memq address seen)
                 (setq rows (append rows (list (list address terminal 0))))))
             herdr-core--terminals)
    rows))

(defun herdr-core--filtered-out-p (terminal)
  "Return non-nil when TERMINAL is hidden by the active filter."
  (and herdr-core--filter-active
       (or (equal (herdr-core--get terminal "state") "finished")
           (let ((status (herdr-core--get terminal "status")))
             (and status
                  (equal (herdr-core--get status "state") "idle")
                  (<= (or (herdr-core--get terminal "turn") 0)
                      (or (gethash (herdr-core--get terminal "address")
                                   herdr-core--seen-turn)
                          0)))))))

(defun herdr-core--name-cell (address terminal depth)
  "Return the name cell for TERMINAL at ADDRESS with DEPTH."
  (concat (make-string (* 2 depth) ?\s)
          (if (> depth 0) "\u2514 " "")
          (if (herdr-core--get terminal "name")
              (format "%s (%s)" (herdr-core--get terminal "name") address)
            address)))

(defun herdr-core--title-cell (terminal)
  "Return the title cell for TERMINAL."
  (let ((title (or (herdr-core--get terminal "title") "")))
    (replace-regexp-in-string "[\n\r]+" " " title)))

(defun herdr-core--project-cell (terminal)
  "Return the project cell for TERMINAL."
  (let ((cwd (herdr-core--terminal-cwd terminal)))
    (or (and cwd (car (last (split-string cwd "/" t)))) "")))

(defun herdr-core--dashboard-entries ()
  "Return the tabulated list entries for the dashboard."
  (let ((root herdr-core--dashboard-project-root)
        (rows nil))
    (dolist (row (herdr-core--dashboard-rows))
      (let ((address (nth 0 row))
            (terminal (nth 1 row))
            (depth (nth 2 row)))
        (when (and (or (null root)
                       (herdr-core--path-under-p (herdr-core--terminal-cwd terminal) root))
                   (not (herdr-core--filtered-out-p terminal)))
          (push (list address
                      (vector
                       (herdr-core--name-cell address terminal depth)
                       (propertize (herdr-core--status-string terminal)
                                   'face (herdr-core--status-face terminal))
                       (herdr-core--title-cell terminal)
                       (herdr-core--project-cell terminal)
                       (herdr-core--age-string terminal)))
                rows))))
    (nreverse rows)))

(defun herdr-core--counts ()
  "Return (TOTAL WORKING BLOCKED UNSEEN)."
  (let ((total 0) (working 0) (blocked 0) (unseen 0))
    (maphash (lambda (address terminal)
               (setq total (1+ total))
               (let ((status (herdr-core--get terminal "status")))
                 (when (and status (equal (herdr-core--get status "state") "working"))
                   (setq working (1+ working)))
                 (when (and status
                            (member (herdr-core--get status "state")
                                    '("blocked" "error")))
                   (setq blocked (1+ blocked))))
               (when (> (or (herdr-core--get terminal "turn") 0)
                        (or (gethash address herdr-core--seen-turn) 0))
                 (setq unseen (1+ unseen))))
             herdr-core--terminals)
    (list total working blocked unseen)))

(defun herdr-core--dashboard-header-line ()
  "Return the dashboard header line."
  (let* ((counts (herdr-core--counts))
         (base (format "hc: %d terminals, %d working, %d blocked/error | RET attach  v view  N spawn  s send  c compose  ? help"
                       (nth 0 counts) (nth 1 counts) (nth 2 counts)))
         (connected (and herdr-core--api
                          (plist-get herdr-core--api :alive)
                          (process-live-p (plist-get herdr-core--api :proc))))
         (reason (or herdr-core--last-stderr
                     (and herdr-core--last-error
                          (herdr-core--first-line
                           (herdr-core--error-message herdr-core--last-error))))))
    (concat
     (unless connected
       (format "[offline%s] "
               (if (and reason (not (equal reason "")))
                   (format ": %s" (herdr-core--truncate reason 60))
                 "")))
     base
     (when herdr-core--dashboard-project-root
       (format "; project: %s" herdr-core--dashboard-project-root))
     (when (and (integerp herdr-core--hidden-finished)
                (> herdr-core--hidden-finished 0))
       (format "; %d finished hidden (A)" herdr-core--hidden-finished)))))

(defvar herdr-core-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'herdr-core-attach)
    (define-key map "v" #'herdr-core-terminal)
    (define-key map "s" #'herdr-core-send)
    (define-key map "c" #'herdr-core-compose)
    (define-key map "N" #'herdr-core-spawn-agent)
    (define-key map "n" #'herdr-core-run)
    (define-key map "i" #'herdr-core-interrupt)
    (define-key map "k" #'herdr-core-kill)
    (define-key map "x" #'herdr-core-rm)
    (define-key map "w" #'herdr-core-wait)
    (define-key map "W" #'herdr-core-wait-children)
    (define-key map "p" #'herdr-core-put-file)
    (define-key map "g" #'herdr-core-refresh)
    (define-key map "f" #'herdr-core-toggle-filter)
    (define-key map "A" #'herdr-core-toggle-all)
    (define-key map "P" #'herdr-core-toggle-project-filter)
    (define-key map "q" #'quit-window)
    (define-key map "?" #'describe-mode)
    map)
  "Keymap for `herdr-core-dashboard-mode'.")

(define-derived-mode herdr-core-dashboard-mode tabulated-list-mode "hc"
  "Major mode for the herdr-core dashboard.

Each row is one terminal, with children indented under their parent.  The
state column holds the agent status, the title column its last window
title, the project column the last directory name of its working
directory, and the age column the time since that changed.

\\{herdr-core-dashboard-mode-map}"
  (setq tabulated-list-format [("Name" 28 nil) ("State" 22 nil)
                               ("Title" 45 nil) ("Project" 16 nil)
                               ("Age" 6 nil)])
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-entries #'herdr-core--dashboard-entries)
  (setq header-line-format #'herdr-core--dashboard-header-line)
  (add-hook 'tabulated-list-revert-hook #'herdr-core--revert nil t)
  (tabulated-list-init-header)
  (require 'herdr-core-attach nil t))

(defun herdr-core--revert ()
  "Refresh from the server when the buffer is reverted."
  (herdr-core--refresh))

(defun herdr-core--dashboard-refresh ()
  "Redraw the dashboard buffer when it is live."
  (let ((buffer (get-buffer "*herdr-core*")))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'herdr-core-dashboard-mode)
          (let ((point (point)))
            (tabulated-list-print t)
            (goto-char (min point (point-max)))))))))

(defun herdr-core-dashboard ()
  "Show the dashboard of herdr-core terminals.
The buffer lists the terminals the core reports, with agent status, title,
project directory and age.  From it you can attach a real
terminal, read the text mirror, send messages, spawn agents and control
terminals; `herdr-core-dashboard-mode' lists the keys."
  (interactive)
  (condition-case err
      (herdr-core--ensure 'api)
    (error
     (user-error "herdr-core: cannot connect: %s"
                 (herdr-core--first-line (error-message-string err)))))
  (herdr-core--events-start)
  (let ((buffer (get-buffer-create "*herdr-core*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'herdr-core-dashboard-mode)
        (herdr-core-dashboard-mode)))
    (pop-to-buffer buffer)
    (herdr-core--refresh)))

(defun herdr-core-toggle-filter ()
  "Toggle hiding of idle and finished terminals.
An idle terminal is hidden once you have looked at it since its last
activity; finished terminals are always hidden while the filter is on."
  (interactive)
  (setq herdr-core--filter-active (not herdr-core--filter-active))
  (herdr-core--dashboard-refresh))

(defun herdr-core-toggle-all ()
  "Toggle whether finished terminals are listed.
The core hides them by default; this asks for them and keeps asking until
toggled off again."
  (interactive)
  (setq herdr-core--all (not herdr-core--all))
  (herdr-core--refresh))

(defun herdr-core--local-project-root ()
  "Return the local project root, or `default-directory'."
  (let ((project (and (fboundp 'project-current)
                      (project-current nil (expand-file-name default-directory)))))
    (or (and project (fboundp 'project-root) (project-root project))
        (and project (cdr project))
        (expand-file-name default-directory))))

(defun herdr-core-toggle-project-filter ()
  "Toggle limiting the dashboard to the current project.
The project's root is translated to the agent machine, and only terminals
whose working directory is under it are listed."
  (interactive)
  (with-current-buffer "*herdr-core*"
    (setq-local herdr-core--dashboard-project-root
                (unless herdr-core--dashboard-project-root
                  (herdr-core--remote-path (herdr-core--local-project-root))))
    (herdr-core--dashboard-refresh)))

;;; Mirror view

(defun herdr-core--view-buffer (address)
  "Return the mirror buffer of ADDRESS."
  (get-buffer-create (format "*hc:%s*" address)))

(defun herdr-core--view-header (address result terminal)
  "Set the header line of the view of ADDRESS from RESULT and TERMINAL."
  (let ((name (herdr-core--terminal-name terminal address))
        (state (or (herdr-core--get terminal "state") "?"))
        (turn (or (herdr-core--get terminal "turn") 0))
        (cwd (or (herdr-core--terminal-cwd terminal) "")))
    (setq header-line-format
          (concat (format " hc:%s [%s turn %s] %s" name state turn cwd)
                  (if (eq (herdr-core--get result "complete") :false)
                      " (partial: still working)"
                    "")))))

(defun herdr-core--view-refresh (buffer &optional force)
  "Refresh the mirror view in BUFFER.  FORCE ignores auto-refresh."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((address herdr-core--target)
             (terminal (and address (gethash address herdr-core--terminals)))
             (working (herdr-core--working-p terminal))
             (lines (if working 0 herdr-core-history-lines)))
        (when (and address (or force herdr-core--auto-refresh))
          (herdr-core--request
           "terminal.read"
           (list (cons "terminal" address) (cons "lines" lines))
           (lambda (result error)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (if error
                     (cond
                      ((equal (herdr-core--error-kind error) "not_found")
                       (setq header-line-format
                             (format " hc:%s terminal gone" address)
                             herdr-core--auto-refresh nil))
                      (t (herdr-core--report-error
                          (format "read %s" address)
                          (herdr-core--error-message error))))
                   (let ((text (or (herdr-core--get result "text") "")))
                     (unless (equal text herdr-core--last-text)
                       (let ((point (point))
                             (window (get-buffer-window buffer))
                             (start (and (get-buffer-window buffer)
                                         (window-start (get-buffer-window buffer)))))
                         (with-silent-modifications
                           (let ((inhibit-read-only t))
                             (erase-buffer)
                             (insert text)
                             (goto-char (min point (point-max)))
                             (when (and window start)
                               (set-window-start window start 'noforce)))))
                       (setq herdr-core--last-text text))
                     (setq herdr-core--last-read (float-time))
                     (herdr-core--view-header
                      address result
                      (gethash address herdr-core--terminals))
                     (unless (herdr-core--working-p
                              (gethash address herdr-core--terminals))
                       (herdr-core--mark-seen address)))))))))))))

(defun herdr-core--refresh-views ()
  "Refresh every auto-refreshing mirror buffer."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (and (derived-mode-p 'herdr-core-terminal-mode)
                      herdr-core--auto-refresh)))
      (herdr-core--view-refresh buffer))))

(defun herdr-core--view-timer-tick ()
  "Refresh visible mirror buffers and stop when none is displayed."
  (let ((any nil))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (derived-mode-p 'herdr-core-terminal-mode)))
        (let ((window (get-buffer-window buffer)))
          (when window
            (setq any t)
            (with-current-buffer buffer
              (let ((stale (- (float-time) (or herdr-core--last-read 0))))
                (when (and herdr-core--auto-refresh
                           (or (and (herdr-core--working-p
                                     (gethash herdr-core--target
                                              herdr-core--terminals))
                                    (> stale herdr-core-refresh-interval))
                               (and (eq buffer (window-buffer (selected-window)))
                                    (> stale 10))))
                  (herdr-core--view-refresh buffer))))))))
    (unless any
      (when (timerp herdr-core--view-timer)
        (cancel-timer herdr-core--view-timer))
      (setq herdr-core--view-timer nil))))

(defun herdr-core--view-timer-start ()
  "Start the mirror refresh timer when needed."
  (unless (timerp herdr-core--view-timer)
    (setq herdr-core--view-timer
          (run-with-timer 1 1 #'herdr-core--view-timer-tick))))

(defvar herdr-core-terminal-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "a" #'herdr-core-attach)
    (define-key map "g" #'herdr-core-refresh)
    (define-key map "s" #'herdr-core-send)
    (define-key map "c" #'herdr-core-compose)
    (define-key map "i" #'herdr-core-interrupt)
    (define-key map "k" #'herdr-core-send-keys)
    (define-key map "K" #'herdr-core-kill)
    (define-key map "p" #'herdr-core-put-file)
    (define-key map "o" #'herdr-core-open-file-at-point)
    (define-key map "w" #'herdr-core-wait)
    (define-key map "t" #'herdr-core-toggle-auto-refresh)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `herdr-core-terminal-mode'.")

(define-derived-mode herdr-core-terminal-mode special-mode "hc-view"
  "Major mode for the text mirror of a herdr-core terminal.

The buffer holds the terminal's screen as plain text and refreshes on a
timer while the agent works.  It cannot take input; use
`herdr-core-attach' for a real terminal.

\\{herdr-core-terminal-mode-map}"
  (setq-local herdr-core--last-text nil)
  (setq-local herdr-core--auto-refresh t)
  (setq-local herdr-core--last-read 0)
  (add-hook 'kill-buffer-hook #'herdr-core--view-timer-check nil t)
  (require 'herdr-core-attach nil t))

(defun herdr-core--view-timer-check ()
  "Stop the mirror timer when no mirror buffer is displayed."
  (let ((any nil))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (derived-mode-p 'herdr-core-terminal-mode))
                 (get-buffer-window buffer))
        (setq any t)))
    (when (and (not any) (timerp herdr-core--view-timer))
      (cancel-timer herdr-core--view-timer)
      (setq herdr-core--view-timer nil))))

(defun herdr-core-terminal (address)
  "Show the text mirror of ADDRESS.
The buffer holds the terminal's screen as plain text and refreshes on a
timer while the agent works.  It cannot take input; use
`herdr-core-attach' for a real terminal."
  (interactive (list (herdr-core--interactive-target)))
  (let ((buffer (herdr-core--view-buffer address)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'herdr-core-terminal-mode)
        (herdr-core-terminal-mode))
      (setq-local herdr-core--target address))
    (pop-to-buffer buffer)
    (herdr-core--mark-seen address)
    (herdr-core--view-refresh buffer t)
    (herdr-core--view-timer-start)))

(defun herdr-core-toggle-auto-refresh ()
  "Toggle automatic refresh of the current mirror view.
With refresh off the buffer changes only on `herdr-core-refresh'."
  (interactive)
  (setq herdr-core--auto-refresh (not herdr-core--auto-refresh))
  (when herdr-core--auto-refresh
    (herdr-core--view-refresh (current-buffer) t))
  (message "hc: auto-refresh %s" (if herdr-core--auto-refresh "on" "off")))

;;; Targets

(defun herdr-core--live-terminals ()
  "Return live terminals as (ADDRESS . TERMINAL), newest status first."
  (let (result)
    (maphash (lambda (address terminal)
               (when (equal (herdr-core--get terminal "state") "live")
                 (push (cons address terminal) result)))
             herdr-core--terminals)
    (sort result
          (lambda (a b)
            (> (herdr-core--terminal-sort-ms (cdr a))
               (herdr-core--terminal-sort-ms (cdr b)))))))

(defun herdr-core--read-target ()
  "Prompt for a terminal."
  (let* ((candidates (herdr-core--live-terminals))
         (alist (mapcar (lambda (candidate)
                          (cons (format "%s (%s) \u2014 %s"
                                        (herdr-core--terminal-name (cdr candidate))
                                        (car candidate)
                                        (herdr-core--status-string (cdr candidate)))
                                (car candidate)))
                        candidates))
         (choice (completing-read "Terminal: " alist nil t)))
    (or (cdr (assoc choice alist))
        (user-error "No terminal selected"))))

(defun herdr-core--resolve-target ()
  "Resolve the target terminal for a command."
  (or (and (derived-mode-p 'herdr-core-compose-mode) herdr-core--target)
      (and (derived-mode-p 'herdr-core-terminal-mode) herdr-core--target)
      (herdr-core--project-agent-address (herdr-core--local-project-root))
      (and herdr-core--last-agent
           (herdr-core--terminal-live-p herdr-core--last-agent)
           herdr-core--last-agent)
      (herdr-core--read-target)))

(defun herdr-core--buffer-target ()
  "Return the target of the current buffer, if any."
  (cond
   ((derived-mode-p 'herdr-core-dashboard-mode) (tabulated-list-get-id))
   ((derived-mode-p 'herdr-core-terminal-mode) herdr-core--target)
   ((derived-mode-p 'herdr-core-compose-mode) herdr-core--target)
   (t nil)))

(defun herdr-core--remember-agent (address)
  "Remember ADDRESS as the agent of the current project."
  (setq herdr-core--last-agent address)
  (let* ((local-root (ignore-errors (herdr-core--local-project-root)))
         (remote-root (and local-root (herdr-core--remote-path local-root t)))
         (terminal (gethash address herdr-core--terminals))
         (cwd (herdr-core--terminal-cwd terminal)))
    (when (and remote-root cwd (herdr-core--path-under-p cwd remote-root))
      (puthash (file-name-as-directory local-root) address herdr-core--project-agents))))

(defun herdr-core--project-agent-address (&optional root)
  "Return the agent address of the project at ROOT, or nil."
  (let* ((local-root (file-name-as-directory
                      (or root (herdr-core--local-project-root))))
         (remembered (gethash local-root herdr-core--project-agents))
         (remote-root (herdr-core--remote-path local-root t))
         (best nil)
         (best-ms -1))
    (cond
     ((and remembered
           (herdr-core--terminal-live-p remembered)
           (or (null remote-root)
               (herdr-core--path-under-p
                (herdr-core--terminal-cwd (gethash remembered herdr-core--terminals))
                remote-root)))
      remembered)
     (remote-root
      (maphash (lambda (address terminal)
                 (let ((cwd (herdr-core--terminal-cwd terminal))
                       (ms (or (herdr-core--get terminal "status_ms") 0)))
                   (when (and (equal (herdr-core--get terminal "state") "live")
                              (herdr-core--path-under-p cwd remote-root)
                              (> ms best-ms))
                     (setq best address best-ms ms))))
               herdr-core--terminals)
      best)
     (t nil))))

(defun herdr-core-toggle-agent ()
  "Attach the current project's agent, or bury its buffer.
Attaches the agent in a terminal emulator; when its buffer is already
shown in a window, hides that window instead."
  (interactive)
  (let ((address (herdr-core--project-agent-address
                  (herdr-core--local-project-root))))
    (unless address (user-error "No agent for this project"))
    (let ((buffer (get-buffer (format "*hc:%s terminal*" address))))
      (if (and buffer (get-buffer-window buffer))
          (bury-buffer buffer)
        (herdr-core-attach address)))))

;;; Chat

(defun herdr-core--region-entry (start end)
  "Return a context entry for the region between START and END."
  (let ((path (buffer-file-name)))
    (list :type 'region
          :label (format "%s (lines %d-%d)"
                         (if path (concat "file: " path) (buffer-name))
                         (line-number-at-pos start)
                         (line-number-at-pos (if (> end start) (1- end) end)))
          :path path
          :mode major-mode
          :range (cons (line-number-at-pos start)
                       (line-number-at-pos (if (> end start) (1- end) end)))
          :text (buffer-substring-no-properties start end))))

(defun herdr-core--buffer-entry ()
  "Return a context entry for the whole current buffer."
  (let ((path (buffer-file-name)))
    (list :type 'buffer
          :label (format "buffer: %s" (buffer-name))
          :path path
          :mode major-mode
          :text (buffer-substring-no-properties (point-min) (point-max)))))

(defun herdr-core--send-text ()
  "Return the message text for `herdr-core-send'.
In a compose buffer the whole buffer is the message; elsewhere one-off
context is staged for a prefix argument or an active region, then the
message is read."
  (if (derived-mode-p 'herdr-core-compose-mode)
      (buffer-string)
    (let ((entries nil))
      (when (and current-prefix-arg
                 (not (or (derived-mode-p 'herdr-core-dashboard-mode)
                          (derived-mode-p 'herdr-core-terminal-mode)
                          (derived-mode-p 'herdr-core-compose-mode))))
        (push (herdr-core--buffer-entry) entries))
      (when (use-region-p)
        (push (herdr-core--region-entry (region-beginning) (region-end)) entries))
      (list (read-string "Message: ") (nreverse entries)))))

(defun herdr-core--compose-text (text entries)
  "Render ENTRIES and TEXT into one message."
  (let ((all (append herdr-core-context-entries entries)))
    (if all
        (concat (herdr-core-context-render all) "\n### Message\n" text)
      text)))

(defun herdr-core--blocked-message (address)
  "Return the blocked message of ADDRESS."
  (let* ((terminal (gethash address herdr-core--terminals))
         (status (and terminal (herdr-core--get terminal "status"))))
    (or (and status (herdr-core--status-message status terminal)) "blocked")))

(defun herdr-core--send-request (address text force)
  "Send TEXT to ADDRESS, forcing when FORCE is non-nil."
  (herdr-core--request
   "terminal.send"
   (append (list (cons "terminal" address) (cons "text" text))
           (when force (list (cons "force" t))))
   (lambda (result error)
     (cond
      (error
       (let ((kind (herdr-core--error-kind error)))
         (cond
          ((equal kind "blocked")
           (when (yes-or-no-p
                  (format "Agent is blocked (%s). Send anyway? "
                          (herdr-core--blocked-message address)))
             (herdr-core--send-request address text t)))
          ((equal kind "not_found")
           (herdr-core--mark-gone address)
           (herdr-core--report-error (format "send %s" address)
                                     (herdr-core--error-message error)))
          (t (herdr-core--report-error (format "send %s" address)
                                       (herdr-core--error-message error))))))
      (t
       (let* ((terminal (gethash address herdr-core--terminals))
              (turn (herdr-core--get result "turn"))
              (started (herdr-core--get result "started")))
         (message "Sent to %s (turn %s)%s"
                  (herdr-core--terminal-name terminal address)
                  (if (integerp turn) turn "?")
                  (if (eq started :false)
                      " \u2014 did not start; check the screen"
                    ""))
         (herdr-core--remember-agent address)
         (herdr-core--refresh)))))))

(defun herdr-core-send (text &optional target force entries)
  "Send TEXT to TARGET as a message to its agent.
TARGET defaults to the terminal at point in a herdr-core buffer, else the
current project's agent, else the agent used last, else a prompt.  ENTRIES is
one-off context for this message, rendered before TEXT together with
`herdr-core-context-entries'.

Interactively the message is read in the minibuffer, except in a compose
buffer, whose text is sent as is.  An active region is staged as context;
with a prefix argument in a buffer that is not a herdr-core buffer the
whole buffer is staged and the send is forced.  FORCE sends even when the
agent is blocked, without asking."
  (interactive
   (let ((text (herdr-core--send-text)))
     (if (listp text)
         (list (nth 0 text) (herdr-core--interactive-target)
               current-prefix-arg (nth 1 text))
       (list text (herdr-core--interactive-target)
             current-prefix-arg nil))))
  (let* ((address (or target (herdr-core--resolve-target)))
         (final (herdr-core--compose-text text entries)))
    (herdr-core--send-request address final force)))

(defvar herdr-core-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'herdr-core-compose-send)
    (define-key map (kbd "C-c C-k") #'herdr-core-compose-cancel)
    (define-key map (kbd "C-c C-a") #'herdr-core-context-add)
    (define-key map (kbd "C-c C-x") #'herdr-core-context-clear)
    (define-key map (kbd "C-c C-v") #'herdr-core-context-show)
    (define-key map (kbd "C-c C-p") #'herdr-core-put-file)
    (define-key map (kbd "C-c C-o") #'herdr-core-open-file-at-point)
    map)
  "Keymap for `herdr-core-compose-mode'.")

(defun herdr-core--compose-header ()
  "Update the header line of the current compose buffer."
  (setq header-line-format
        (format "Context: %d entries, %d chars \u2014 C-c C-a add  C-c C-x clear  C-c C-v show"
                (length herdr-core-context-entries)
                (let ((total 0))
                  (dolist (entry herdr-core-context-entries)
                    (setq total (+ total (length (or (plist-get entry :text) "")))))
                  total))))

(defun herdr-core--compose-header-update ()
  "Update every live compose buffer header."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (derived-mode-p 'herdr-core-compose-mode)))
      (with-current-buffer buffer
        (herdr-core--compose-header)))))

(define-derived-mode herdr-core-compose-mode text-mode "hc-compose"
  "Major mode for composing a message to an agent.

The whole buffer is the message.  Staged context is prepended when it is
sent; the header line shows how much is staged.
`herdr-core-compose-send' sends and clears the buffer.

\\{herdr-core-compose-mode-map}"
  (setq-local herdr-core--target nil)
  (herdr-core--compose-header))

(defun herdr-core-compose (address &optional region-text)
  "Compose a message to ADDRESS in a buffer of its own.
REGION-TEXT, when given, is inserted first.  The buffer is displayed below
the selected window; `herdr-core-compose-send' sends it and
`herdr-core-compose-cancel' closes it."
  (interactive
   (list (herdr-core--resolve-target)
         (when (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))))
  (let ((buffer (get-buffer-create (format "*hc:%s compose*" address))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'herdr-core-compose-mode)
        (herdr-core-compose-mode))
      (setq-local herdr-core--target address)
      (when region-text (insert region-text))
      (herdr-core--compose-header))
    (display-buffer buffer '(display-buffer-below-selected (window-height . 8)))
    buffer))

(defun herdr-core-compose-send ()
  "Send the compose buffer to its terminal and clear it."
  (interactive)
  (let ((text (buffer-string))
        (address herdr-core--target))
    (unless address (user-error "No terminal for this compose buffer"))
    (herdr-core-send text address)
    (erase-buffer)
    (herdr-core--compose-header)))

(defun herdr-core-compose-cancel ()
  "Close the compose window without sending."
  (interactive)
  (quit-window))

;;; Keys, interrupt, kill, wait

(defvar herdr-core-key-names
  (append '("enter" "esc" "tab" "space" "backspace"
            "up" "down" "left" "right" "home" "end" "pgup" "pgdown")
          (mapcar (lambda (character) (format "ctrl+%c" character))
                  (number-sequence ?a ?z))
          (list "alt+x")
          (mapcar (lambda (number) (format "f%d" number))
                  (number-sequence 1 12)))
  "Key names accepted by `herdr-core-send-keys'.")

(defun herdr-core-send-keys (keys &optional target)
  "Send KEYS to TARGET.
KEYS is a list of key names such as \"enter\", \"ctrl+c\" or \"f5\"; see
`herdr-core-key-names' for all accepted names.  Interactively they are
read as a space-separated list."
  (interactive
   (list (split-string
          (read-string "Keys: " nil nil
                       (mapconcat #'identity herdr-core-key-names " "))
          "[ \t]+" t)
         (herdr-core--interactive-target)))
  (let ((address (or target (herdr-core--resolve-target))))
    (herdr-core--request
     "terminal.keys"
     (list (cons "terminal" address) (cons "keys" (append keys nil)))
     (lambda (result error)
       (if error
           (herdr-core--report-error (format "keys %s" address)
                                     (herdr-core--error-message error))
         (message "hc: sent %s to %s (turn %s)"
                  (mapconcat #'identity keys " ")
                  address (herdr-core--get result "turn"))
         (herdr-core--refresh))))))

(defun herdr-core-interrupt (&optional target)
  "Interrupt TARGET after confirmation.
The core sends the interrupt key and reports the outcome."
  (interactive (list (herdr-core--interactive-target)))
  (let* ((address (or target (herdr-core--resolve-target)))
         (name (herdr-core--terminal-name
                (gethash address herdr-core--terminals) address)))
    (when (yes-or-no-p (format "Interrupt %s? " name))
      (herdr-core--request
       "terminal.interrupt"
       (list (cons "terminal" address) (cons "timeout" 10))
       (lambda (result error)
         (if error
             (herdr-core--report-error (format "interrupt %s" address)
                                       (herdr-core--error-message error))
           (message "hc: %s (sent %s)"
                    (or (herdr-core--get result "outcome") "unknown")
                    (or (herdr-core--get result "key") "?"))
           (herdr-core--refresh)))))))

(defun herdr-core-kill (&optional target)
  "Kill TARGET after confirmation."
  (interactive (list (herdr-core--interactive-target)))
  (let* ((address (or target (herdr-core--resolve-target)))
         (name (herdr-core--terminal-name
                (gethash address herdr-core--terminals) address)))
    (when (yes-or-no-p (format "Kill %s? " name))
      (herdr-core--request
       "terminal.kill" (list (cons "terminal" address))
       (lambda (_result error)
         (if error
             (herdr-core--report-error (format "kill %s" address)
                                       (herdr-core--error-message error))
           (message "hc: killed %s" name)
           (herdr-core--refresh)))))))

(defun herdr-core-rm (&optional target)
  "Remove the finished terminal TARGET from the core.
Only a terminal whose process has ended can be removed."
  (interactive (list (herdr-core--interactive-target)))
  (let* ((address (or target (herdr-core--resolve-target)))
         (terminal (gethash address herdr-core--terminals)))
    (unless (and terminal (equal (herdr-core--get terminal "state") "finished"))
      (user-error "Only a finished terminal can be removed"))
    (when (yes-or-no-p (format "Remove %s? " address))
      (herdr-core--request
       "terminal.rm" (list (cons "terminal" address))
       (lambda (_result error)
         (if error
             (herdr-core--report-error (format "rm %s" address)
                                       (herdr-core--error-message error))
           (message "hc: removed %s" address)
           (herdr-core--refresh)))))))

(defun herdr-core--wait-callback (result error)
  "Report a terminal.wait RESULT or ERROR."
  (if error
      (herdr-core--report-error "wait" (herdr-core--error-message error))
    (let ((reason (herdr-core--get result "reason"))
          (status (herdr-core--get result "status"))
          (code (herdr-core--get result "exit_code")))
      (if (equal reason "timeout")
          (message "hc: still running")
        (message "hc: ended=%s reason=%s status=%s exit=%s"
                 (herdr-core--true-p (herdr-core--get result "ended"))
                 (or reason "-")
                 (or (and status (herdr-core--get status "state")) "-")
                 (if (integerp code) code "-"))
        (herdr-core--notify "herdr-core"
                            (format "wait finished (%s)" (or reason "ended")))
        (herdr-core--refresh)))))

(defun herdr-core-wait (&optional target output)
  "Wait for TARGET to stop working, or for OUTPUT to appear.
OUTPUT is a string to look for in the terminal's output; nil waits for the
agent to leave the working state.  Gives up after 300 seconds and reports
why."
  (interactive
   (list (herdr-core--interactive-target)
         (read-string "Wait for output (optional): ")))
  (let ((address (or target (herdr-core--resolve-target))))
    (herdr-core--request
     "terminal.wait"
     (append (list (cons "terminal" address) (cons "timeout" 300))
             (when (and output (not (equal output "")))
               (list (cons "output" output))))
     #'herdr-core--wait-callback
     330)))

(defun herdr-core-wait-children (&optional target)
  "Wait for every child of TARGET to stop working.
See `herdr-core-wait'."
  (interactive (list (herdr-core--interactive-target)))
  (let ((address (or target (herdr-core--resolve-target))))
    (herdr-core--request
     "terminal.wait"
     (list (cons "terminal" address) (cons "children" "all")
           (cons "timeout" 300))
     #'herdr-core--wait-callback
     330)))

;;; Attach dispatch

(defun herdr-core--require-attach ()
  "Load the attach backend library."
  (require 'herdr-core-attach))

(defun herdr-core-attach (address)
  "Attach ADDRESS to a real terminal emulator.
`herdr-core-attach-backend' chooses ghostel, vterm, eat or the text mirror
of `herdr-core-terminal'.  The terminal keeps running when the attach
ends; detach with C-\\ in the emulator.  With `herdr-core-attach-observe'
non-nil the attach is read-only."
  (interactive (list (herdr-core--interactive-target)))
  (herdr-core--require-attach)
  (herdr-core--attach-impl address))

;;; Projects and spawning

(defun herdr-core--host-name (target)
  "Return the host part of the SSH TARGET."
  (let ((host (car (last (split-string target "@")))))
    (car (split-string host ":"))))

(defun herdr-core--mapped-path (path)
  "Return PATH translated by `herdr-core-project-root-map', or nil."
  (let ((best nil)
        (best-length -1))
    (dolist (pair herdr-core-project-root-map)
      (let ((from (car pair)))
        (when (and from
                   (string-prefix-p from path)
                   (> (length from) best-length))
          (setq best pair best-length (length from)))))
    (when best
      (concat (cdr best) (substring path (length (car best)))))))

(defun herdr-core--remote-path (local-path &optional no-prompt)
  "Return LOCAL-PATH translated to a directory on the agent machine.
A TRAMP path on `herdr-core-host' contributes its path after the host; a
TRAMP path on any other host yields nil.  Otherwise
`herdr-core-project-root-map' is consulted, then LOCAL-PATH itself when the
core is local.  Otherwise ask for the directory and remember the answer;
with NO-PROMPT return nil instead of asking."
  (cond
   ((null local-path) nil)
   ((file-remote-p local-path)
    (let ((prefix (file-remote-p local-path)))
      (when (string-match "\\`/ssh:\\(?:\\([^@:/]+\\)@\\)?\\([^:/]+\\):" prefix)
        (let ((host (match-string 2 prefix))
              (rest (substring local-path (length prefix))))
          (when (and herdr-core-host
                     (equal host (herdr-core--host-name herdr-core-host)))
            (if (string-prefix-p "/" rest) rest (concat "/" rest)))))))
   (t
    (let ((mapped (herdr-core--mapped-path local-path)))
      (cond
       (mapped mapped)
       ((null herdr-core-host) local-path)
       (no-prompt nil)
       (t
        (let ((answer (read-string
                       (format "Directory on the agent machine for %s: " local-path)
                       herdr-core-default-directory)))
          (when (or (null answer) (equal answer ""))
            (user-error "A remote directory is required"))
          (add-to-list 'herdr-core-project-root-map
                       (cons (file-name-as-directory local-path)
                             (file-name-as-directory answer)))
          (herdr-core--mapped-path local-path))))))))

(defun herdr-core--slug (name)
  "Return NAME as a terminal name."
  (let ((slug (replace-regexp-in-string "[^a-z0-9_.-]" "-" (downcase name))))
    (setq slug (replace-regexp-in-string "\\`-+" "" slug))
    (setq slug (replace-regexp-in-string "-+\\'" "" slug))
    (if (equal slug "") "agent" slug)))

(defun herdr-core--unique-name (name)
  "Return NAME with a numeric suffix when a live terminal uses it."
  (let ((candidate name)
        (index 1))
    (while (let ((terminal (and (stringp candidate)
                                (gethash candidate herdr-core--terminals))))
             (and terminal
                  (equal (herdr-core--get terminal "state") "live")))
      (setq index (1+ index)
            candidate (format "%s-%d" name index)))
    candidate))

(defun herdr-core--template-name (template)
  "Return the name of TEMPLATE."
  (herdr-core--get template "name"))

(defun herdr-core--read-agent-command ()
  "Read an agent command or template name."
  (completing-read "Agent: "
                   (append herdr-core-agent-commands
                           (mapcar #'herdr-core--template-name
                                   herdr-core--templates))
                   nil nil "omp"))

(defun herdr-core--read-remote-directory ()
  "Read the directory on the agent machine."
  (read-string "Directory on the agent machine: "
               (herdr-core--remote-path (herdr-core--local-project-root))))

(defun herdr-core--read-spawn-name (directory)
  "Read the terminal name for DIRECTORY."
  (let ((default (herdr-core--unique-name
                  (herdr-core--slug
                   (file-name-nondirectory (directory-file-name directory))))))
    (read-string (format "Name (default %s): " default) nil nil default)))

(defun herdr-core--path-key (path)
  "Return PATH in a form that compares paths on the agent machine.
Slashes are unified and a trailing slash removed.  With a local core on
Windows, case is folded as well."
  (when (and path (not (equal path "")))
    (let ((key (replace-regexp-in-string "\\\\" "/" path)))
      (setq key (directory-file-name key))
      (if (and (null herdr-core-host) (eq system-type 'windows-nt))
          (downcase key)
        key))))

(defun herdr-core--new-id (prefix seed)
  "Return PREFIX followed by a 32-character digest of SEED."
  (concat prefix (md5 (or seed ""))))

(defvar herdr-core--spawn-counter 0
  "Terminals started from this Emacs, used to keep tab ids apart.")

(defun herdr-core--new-tab-id (directory)
  "Return a tab id that no earlier spawn used."
  (setq herdr-core--spawn-counter (1+ herdr-core--spawn-counter))
  (herdr-core--new-id
   "tb" (format "%s-%s-%s-%s" (herdr-core--path-key directory)
                (float-time) (emacs-pid) herdr-core--spawn-counter)))

(defun herdr-core--doc-first-terminal (node)
  "Return the first terminal address in layout NODE, or nil."
  (when (consp node)
    (or (herdr-core--get (herdr-core--get node "content") "terminal")
        (let ((children (append (herdr-core--get node "children") nil))
              (found nil))
          (while (and children (not found))
            (setq found (herdr-core--doc-first-terminal (car children))
                  children (cdr children)))
          found))))

(defun herdr-core--space-live-p (space)
  "Return non-nil when SPACE still has a tab."
  (let ((tabs (herdr-core--get space "tabs")))
    (and tabs (consp tabs))))

(defun herdr-core--space-first-cwd (space on)
  "Return the working directory of the first terminal in SPACE.
ON qualifies terminal addresses read from another machine's document."
  (let* ((tab (car (append (herdr-core--get space "tabs") nil)))
         (address (and tab (herdr-core--doc-first-terminal
                            (herdr-core--get tab "root"))))
         (key (and address (if on (concat on "/" address) address))))
    (when key
      (herdr-core--terminal-cwd (gethash key herdr-core--terminals)))))

(defun herdr-core--adopt-space-by-name (spaces name)
  "Return the first live space of SPACES named NAME, ignoring case."
  (let ((wanted (downcase (or name "")))
        found)
    (when (not (equal wanted ""))
      (dolist (space spaces)
        (when (and (not found)
                   (herdr-core--space-live-p space)
                   (equal (downcase (or (herdr-core--get space "name") ""))
                          wanted))
          (setq found space))))
    found))

(defun herdr-core--adopt-space-at-folder (spaces directory)
  "Return the first live space of SPACES made in DIRECTORY."
  (let ((key (herdr-core--path-key directory))
        found)
    (when key
      (dolist (space spaces)
        (when (and (not found)
                   (herdr-core--space-live-p space)
                   (equal (herdr-core--path-key
                           (herdr-core--get space "folder"))
                          key))
          (setq found space))))
    found))

(defun herdr-core--adopt-space-for-directory (spaces directory on hint)
  "Return the space of SPACES that belongs to DIRECTORY, or nil.
The name is tried first, then the space whose first terminal works in
DIRECTORY, then one made in DIRECTORY.  ON qualifies terminal addresses
read from another machine's document."
  (let ((base (and (stringp directory)
                   (not (equal directory ""))
                   (file-name-nondirectory (directory-file-name directory))))
        (key (herdr-core--path-key directory))
        (space nil))
    (setq space (or (and hint (herdr-core--adopt-space-by-name spaces hint))
                    (and base (herdr-core--adopt-space-by-name spaces base))))
    (unless space
      (dolist (candidate spaces)
        (let ((cwd (and key (herdr-core--space-first-cwd candidate on))))
          (when (and (not space)
                     (herdr-core--space-live-p candidate)
                     (equal (herdr-core--path-key cwd) key))
            (setq space candidate)))))
    (or space (herdr-core--adopt-space-at-folder spaces directory))))

(defun herdr-core--adopt-target (doc directory &optional hint on)
  "Return (SPACE-ID . TAB-ID) for a terminal started in DIRECTORY.
The space is the one that belongs to DIRECTORY, or a new one when
`herdr-core-adopt-space' is t and DIRECTORY is non-nil; the tab is always
new, so each terminal gets a tab of its own.  HINT names the project.  ON
reads terminal addresses from another machine's document."
  (let* ((spaces (append (herdr-core--get doc "spaces") nil))
         (space (cond
                 ((stringp herdr-core-adopt-space)
                  (herdr-core--adopt-space-by-name
                   spaces herdr-core-adopt-space))
                 ((eq herdr-core-adopt-space t)
                  (herdr-core--adopt-space-for-directory
                   spaces directory on hint)))))
    (when (or space (and (eq herdr-core-adopt-space t) directory))
      (cons (if space
                (herdr-core--get space "id")
              (herdr-core--new-id "sp" (herdr-core--path-key directory)))
            (herdr-core--new-tab-id directory)))))

(defun herdr-core--project-hint ()
  "Return the current project's name, or nil.
The name is the last component of the project root.  It picks the Herdr
App space for a terminal started from the project when the space is named
after it."
  (let ((project (and (fboundp 'project-current) (project-current))))
    (when project
      (let ((root (ignore-errors (project-root project))))
        (and (stringp root)
             (file-name-nondirectory (directory-file-name root)))))))

(defun herdr-core--adoption-params (directory on hint callback)
  "Call CALLBACK with a `labels' parameter for DIRECTORY, or nil.
HINT names the current project.  Nothing is fetched when
`herdr-core-adopt-space' is nil.  ON reads another machine's app document;
DIRECTORY is a directory on that machine."
  (if (null herdr-core-adopt-space)
      (funcall callback nil)
    (herdr-core--request
     "doc.get"
     (append (list (cons "name" "herdr-app"))
             (when on (list (cons "on" on))))
     (lambda (result error)
       (let* ((doc (and result (not error) (herdr-core--get result "value")))
              (target (and (not error)
                           (herdr-core--adopt-target
                            doc directory hint on))))
         (cond
          (error
           (message "herdr-core: app document unavailable: %s"
                    (herdr-core--error-message error)))
          ((and (null target) (stringp herdr-core-adopt-space))
           (message "herdr-core: no space named %s to adopt %s"
                    herdr-core-adopt-space (or directory "the terminal"))))
         (funcall callback
                  (when target
                    (list (cons "labels"
                                (list (cons "herdr.space" (car target))
                                      (cons "herdr.tab" (cdr target))))))))))))

(defun herdr-core--spawn-agent (command directory task name &optional space-hint)
  "Spawn COMMAND in DIRECTORY with TASK, as terminal NAME.
SPACE-HINT names the project the terminal is for; it is used to pick the
Herdr App space when `herdr-core-adopt-space' is t."
  (unless (and directory (not (equal directory "")))
    (user-error "Directory is required"))
  (unless (and name (stringp name)
               (string-match "\\`[a-z0-9_.-]\\{1,32\\}\\'" name))
    (user-error "Invalid terminal name: %s" name))
  (let* ((tokens (split-string command "[ \t]+" t))
         (template (member (car tokens)
                           (mapcar #'herdr-core--template-name
                                   herdr-core--templates)))
         (task (or task ""))
         (params (append
                  (list (cons "cwd" directory)
                        (cons "name" name)
                        (cons "no_parent" t))
                  (if template
                      (list (cons "argv"
                                  (append tokens
                                          (unless (equal task "") (list task)))))
                    (list (cons "line"
                                (if (equal task "")
                                    command
                                  (concat command " "
                                          (herdr-core--posix-quote task)))))))))
    (herdr-core--adoption-params
     directory nil space-hint
     (lambda (adoption)
       (herdr-core--request
        "terminal.run" (append params adoption)
        (lambda (result error)
          (if error
              (herdr-core--report-error "spawn"
                                        (herdr-core--error-message error))
            (let* ((address (herdr-core--get result "address"))
                   (first (herdr-core--get result "first_message")))
              (herdr-core--remember-agent address)
              (herdr-core--refresh)
              (when (and first (eq (herdr-core--get first "sent") :false))
                (message "first message NOT sent (%s)"
                         (or (herdr-core--get first "reason") "unknown")))
              (require 'herdr-core-attach nil t)
              (if (and (fboundp 'herdr-core--attach-backend)
                       (not (eq (funcall 'herdr-core--attach-backend) 'mirror)))
                  (herdr-core-attach address)
                (herdr-core-terminal address))))))))))

(defun herdr-core-spawn-agent ()
  "Start an agent in a directory on the agent machine.
Prompts for the directory, the agent command or template, an optional
task and a terminal name.  The directory must exist on the machine that
runs hc and is where the agent starts; the task, when given, is passed as
the agent's first message.  The terminal opens in the Herdr App as a new
tab in the project's space (`herdr-core-adopt-space'), and is attached
here when a real terminal backend is available."
  (interactive)
  (let* ((directory (herdr-core--read-remote-directory))
         (command (herdr-core--read-agent-command))
         (task (read-string "Task (optional): ")))
    (herdr-core--spawn-agent command directory task
                             (herdr-core--read-spawn-name directory))))

(defun herdr-core-project-spawn-agent ()
  "Start an agent for the current project.
The project's local root is translated to the directory that holds the
same files on the agent machine: a TRAMP root on `herdr-core-host' supplies
its path, else `herdr-core-project-root-map' is consulted, else you are
asked once and the answer is remembered.  Translation only finds the path;
the agent always runs on the agent machine.  The agent starts in that
directory.  Then prompts for the agent command or template, an optional
task and a terminal name, as `herdr-core-spawn-agent' does.  The terminal
opens in the Herdr App as a new tab in the project's space
(`herdr-core-adopt-space')."
  (interactive)
  (let* ((root (herdr-core--local-project-root))
         (directory (herdr-core--remote-path root))
         (command (herdr-core--read-agent-command))
         (task (read-string "Task (optional): ")))
    (herdr-core--spawn-agent
     command directory task (herdr-core--read-spawn-name directory)
     (and root (file-name-nondirectory (directory-file-name root))))))

(defun herdr-core-run (spec &optional prefix)
  "Run SPEC on the agent machine and show its terminal.
SPEC is a command line, or a template name followed by arguments; it runs
in the core's default directory.  Without PREFIX, the current project's
name picks the Herdr App space (`herdr-core-adopt-space').  With PREFIX,
ask for the machine, directory, name and parent terminal instead: a
directory there also gives the terminal a space and a tab of its own, and
a terminal started under a parent gets no labels, because it is already a
sub-agent."
  (interactive (list (read-string "Run (command or template): ")
                     current-prefix-arg))
  (let* ((tokens (split-string spec "[ \t]+" t))
         (template (member (car tokens)
                           (mapcar #'herdr-core--template-name
                                   herdr-core--templates)))
         (params (if prefix
                     (let ((machine (completing-read
                                     "Machine: "
                                     (cons "" (mapcar (lambda (m)
                                                        (herdr-core--get m "name"))
                                                      herdr-core--machines))
                                     nil nil))
                           (directory (read-string "Directory: "))
                           (name (read-string "Name: "))
                           (parent (completing-read
                                    "Parent (optional): "
                                    (cons "" (mapcar #'car (herdr-core--live-terminals)))
                                    nil nil)))
                       (append (when (not (equal machine ""))
                                 (list (cons "on" machine)))
                               (when (not (equal directory ""))
                                 (list (cons "cwd" directory)))
                               (when (not (equal name ""))
                                 (list (cons "name" name)))
                               (if (equal parent "")
                                   (list (cons "no_parent" t))
                                 (list (cons "parent" parent)))))
                   (list (cons "no_parent" t)))))
    (let ((directory (cdr (assoc "cwd" params)))
          (on (cdr (assoc "on" params)))
          (has-parent (assoc "parent" params))
          (run (append params
                       (if template
                           (list (cons "argv" tokens))
                         (list (cons "line" spec))))))
      (herdr-core--adoption-params
       directory on (unless prefix (herdr-core--project-hint))
       (lambda (adoption)
         (herdr-core--request
          "terminal.run"
          (if has-parent run (append run adoption))
          (lambda (result error)
            (if error
                (herdr-core--report-error "run"
                                          (herdr-core--error-message error))
              (let ((address (herdr-core--get result "address")))
                (herdr-core--refresh)
                (herdr-core-terminal address))))))))))

(defun herdr-core-project-dashboard ()
  "Show the dashboard limited to the current project.
The project's local root is translated to the agent machine, and only
terminals whose working directory is under it are listed."
  (interactive)
  (let ((root (herdr-core--remote-path (herdr-core--local-project-root))))
    (herdr-core-dashboard)
    (with-current-buffer "*herdr-core*"
      (setq-local herdr-core--dashboard-project-root root)
      (herdr-core--dashboard-refresh))))

(defun herdr-core-project-agent ()
  "Attach the agent of the current project.
The agent is a live terminal working under the project's directory on the
agent machine, preferring the one used last and then the most recently
active."
  (interactive)
  (let ((address (herdr-core--project-agent-address
                  (herdr-core--local-project-root))))
    (if address
        (herdr-core-attach address)
      (user-error "No agent for this project"))))

;;; Images and files

(defun herdr-core-put-file (file &optional target)
  "Send FILE to TARGET and report the path it was written to.
TARGET defaults as in `herdr-core-send'.  The returned path is inserted at
point in a compose buffer and copied to the kill ring otherwise.  Files
larger than `herdr-core-image-max-bytes' are refused."
  (interactive
   (list (read-file-name "File to send: ") (herdr-core--interactive-target)))
  (let* ((address (or target (herdr-core--resolve-target)))
         (compose (and (derived-mode-p 'herdr-core-compose-mode)
                       (current-buffer)))
         (bytes (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally file)
                  (buffer-string))))
    (when (> (length bytes) herdr-core-image-max-bytes)
      (user-error "File is larger than %d bytes" herdr-core-image-max-bytes))
    (herdr-core--request
     "terminal.put"
     (list (cons "terminal" address)
           (cons "data" (base64-encode-string bytes t))
           (cons "name" (file-name-nondirectory file)))
     (lambda (result error)
       (if error
           (herdr-core--report-error (format "put %s" address)
                                     (herdr-core--error-message error))
         (let ((path (herdr-core--get result "path"))
               (count (herdr-core--get result "bytes")))
           (cond
            ((and compose (buffer-live-p compose))
             (with-current-buffer compose
               (when (derived-mode-p 'herdr-core-compose-mode)
                 (insert path))))
            (t (kill-new path)))
           (message "Wrote %s (%s bytes)" path count)))))))

(defun herdr-core--target-cwd (terminal)
  "Return the working directory of TERMINAL, or \"/\"."
  (or (herdr-core--terminal-cwd terminal) "/"))

(defun herdr-core--split-address (address)
  "Return (MACHINE . TERMINAL) for ADDRESS."
  (let ((slash (string-match "/" address)))
    (if slash
        (cons (substring address 0 slash) (substring address (1+ slash)))
      (cons nil address))))

(defun herdr-core-open-file (path &optional target)
  "Fetch PATH from the machine of TARGET and visit it.
PATH is read on the agent machine; a relative PATH is resolved against the
terminal's working directory.  The file is written to a temporary file, so
images render with `image-mode'."
  (interactive
   (list (read-string "Path on the agent machine: ")
         (herdr-core--interactive-target)))
  (let* ((address (or target (herdr-core--resolve-target)))
         (terminal (gethash address herdr-core--terminals))
         (machine (car (herdr-core--split-address address)))
         (full (if (file-name-absolute-p path)
                   path
                 (concat (file-name-as-directory
                          (herdr-core--target-cwd terminal))
                         path))))
    (herdr-core--request
     "machine.exec"
     (append (list (cons "argv" (list "cat" full))
                   (cons "timeout" 30))
             (when machine (list (cons "on" machine))))
     (lambda (result error)
       (cond
        (error
         (herdr-core--report-error "open file"
                                   (herdr-core--error-message error)))
        ((not (equal (herdr-core--get result "code") 0))
         (herdr-core--report-error
          "open file"
          (format "cat failed (%s): %s"
                  (herdr-core--get result "code")
                  (let ((stderr (herdr-core--get result "stderr")))
                    (if stderr
                        (decode-coding-string
                         (base64-decode-string stderr) 'utf-8)
                      "")))))
        (t
         (when (herdr-core--true-p (herdr-core--get result "truncated"))
           (message "hc: file truncated at 8 MB"))
         (let* ((stdout (herdr-core--get result "stdout"))
                (bytes (base64-decode-string stdout))
                (file (make-temp-file "hc-" nil
                                      (file-name-extension path t))))
           (let ((coding-system-for-write 'no-conversion))
             (write-region bytes nil file nil 'silent))
           (find-file file))))))))

(defun herdr-core-open-file-at-point ()
  "Fetch and visit the file name at point.
Prompts for a path when there is no file name at point.  See
`herdr-core-open-file'."
  (interactive)
  (let ((path (thing-at-point 'filename)))
    (setq path (and path (replace-regexp-in-string "[].,;:)}\"']+\\'" "" path)))
    (unless (and path (not (equal path "")))
      (setq path (read-string "Path on the agent machine: ")))
    (herdr-core-open-file path (herdr-core--interactive-target))))

;;; Mode line and global mode

(defvar herdr-core--mode-line " hc:off"
  "Mode line indicator for herdr-core.")

(defun herdr-core--update-mode-line ()
  "Update `herdr-core--mode-line'."
  (setq herdr-core--mode-line
        (if (not (and herdr-core--api
                      (plist-get herdr-core--api :alive)
                      (process-live-p (plist-get herdr-core--api :proc))))
            " hc:off"
          (let ((counts (herdr-core--counts)))
            (format " hc:%dW %dB %dD" (nth 1 counts) (nth 2 counts)
                    (nth 3 counts))))))

(defun herdr-core--safety-refresh ()
  "Periodic refresh and connection check."
  (when herdr-core-mode
    (when (and herdr-core--api (plist-get herdr-core--api :alive))
      (herdr-core--refresh))
    (when (and (not herdr-core--events-subscribed)
               (not herdr-core--events-retry-timer))
      (herdr-core--events-start))))

(define-minor-mode herdr-core-mode
  "Global minor mode that keeps herdr-core connected and fresh.
Opens the API connection and the event subscription, polls when events are
unavailable, refreshes the cache and the dashboard on a timer, and shows
working, blocked and new-activity counts in the mode line."
  :global t
  :lighter nil
  (if herdr-core-mode
      (progn
        (add-to-list 'global-mode-string 'herdr-core--mode-line t)
        (condition-case err
            (herdr-core--ensure 'api)
          (error (herdr-core--report-error "connect"
                                           (error-message-string err))))
        (herdr-core--events-start)
        (unless (timerp herdr-core--safety-timer)
          (setq herdr-core--safety-timer
                (run-with-timer 30 30 #'herdr-core--safety-refresh))))
    (when (timerp herdr-core--safety-timer)
      (cancel-timer herdr-core--safety-timer))
    (setq herdr-core--safety-timer nil)
    (herdr-core-shutdown)
    (setq global-mode-string (delete 'herdr-core--mode-line global-mode-string))))

;;; Wiring

(setq herdr-core-context-path-resolver
      (lambda (path) (herdr-core--remote-path path t)))
(setq herdr-core-context-open-function #'herdr-core-open-file)
(add-hook 'herdr-core-context-changed-hook #'herdr-core--compose-header-update)

(defun herdr-core-refresh ()
  "Reload terminals from the core and redraw open views."
  (interactive)
  (herdr-core--refresh)
  (herdr-core--refresh-views))

(provide 'herdr-core)

;;; herdr-core.el ends here
