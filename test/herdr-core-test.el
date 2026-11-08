;;; herdr-core-test.el --- Tests for herdr-core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The herdr-core.el authors

;; Author: herdr-core.el contributors
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; herdr-core-test.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; herdr-core-test.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; herdr-core-test.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ERT suite: unit tests for parsing, rendering, targets and commands, plus
;; integration tests against a real local herdr-core reached through
;; `hc connect'.  The integration tests only touch terminals they create.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'herdr-core)
(require 'herdr-core-attach)

(setq herdr-core-host nil
      herdr-core-session nil
      herdr-core-notify nil
      herdr-core-remote-command (concat (expand-file-name "~/.local/bin/hc") " connect")
      herdr-core-attach-history 500
      herdr-core-attach-observe nil)

;;; Helpers

(defun herdr-core-test--terminal (address &rest plist)
  "Return a stub terminal for ADDRESS with PLIST fields."
  (let (overrides)
    (while plist
      (push (cons (substring (symbol-name (pop plist)) 1) (pop plist))
            overrides))
    (append (nreverse overrides)
            (list (cons "address" address)
                  (cons "state" "live")
                  (cons "name" nil)
                  (cons "title" "")
                  (cons "cwd" "/tmp")
                  (cons "turn" 0)
                  (cons "created_ms" 1)
                  (cons "status_ms" nil)
                  (cons "status" nil)
                  (cons "parent" nil)))))

(defun herdr-core-test--default-answer (method _params)
  "Return a default (RESULT . ERROR) for METHOD."
  (cond
   ((equal method "terminal.list")
    (cons (list (cons "terminals" nil) (cons "hidden_finished" 0)) nil))
   ((equal method "templates.list")
    (cons (list (cons "templates" nil)) nil))
   ((equal method "terminal.send")
    (cons (list (cons "turn" 1) (cons "started" t)) nil))
   (t (cons nil nil))))

(defun herdr-core-test--stub (handler)
  "Return a `herdr-core--request' replacement calling HANDLER."
  (lambda (method params callback &optional _timeout)
    (let ((answer (funcall handler method params)))
      (funcall callback (car answer) (cdr answer)))))

;;; Parsing

(ert-deftest herdr-core-test-address-split ()
  (should (equal (herdr-core--split-address "rohan/t7")
                 '("rohan" . "t7")))
  (should (equal (herdr-core--split-address "t7")
                 '(nil . "t7"))))

(ert-deftest herdr-core-test-posix-quote ()
  (should (equal (herdr-core--posix-quote "plain") "'plain'"))
  (should (equal (herdr-core--posix-quote "a'b") "'a'\\''b'")))

(ert-deftest herdr-core-test-json-round-trip ()
  (let* ((encoded (herdr-core--json-encode
                   (list (cons "a" :false) (cons "b" nil) (cons "c" "x"))))
         (parsed (herdr-core--json-parse
                  (decode-coding-string encoded 'utf-8))))
    (should (eq (herdr-core--get parsed "a") :false))
    (should (null (herdr-core--get parsed "b")))
    (should (equal (herdr-core--get parsed "c") "x"))))

(ert-deftest herdr-core-test-json-utf8 ()
  (let* ((encoded (herdr-core--json-encode (list (cons "s" "h\u00e9llo \u03c0"))))
         (parsed (herdr-core--json-parse
                  (decode-coding-string encoded 'utf-8))))
    (should (equal (herdr-core--get parsed "s") "h\u00e9llo \u03c0"))))

(ert-deftest herdr-core-test-json-request-shape ()
  "Requests use string keys and plain lists; encoding must accept both."
  (let* ((encoded (herdr-core--json-encode
                   (list (cons "jsonrpc" "2.0")
                         (cons "id" 1)
                         (cons "method" "terminal.keys")
                         (cons "params"
                               (list (cons "terminal" "t1")
                                     (cons "keys" '("C-c" "x"))
                                     (cons "force" t)
                                     (cons "all" :false)
                                     (cons "nothing" nil))))))
         (text (decode-coding-string encoded 'utf-8))
         (parsed (herdr-core--json-parse text))
         (params (herdr-core--get parsed "params")))
    (should (equal (herdr-core--get parsed "jsonrpc") "2.0"))
    (should (equal (herdr-core--get parsed "id") 1))
    (should (equal (herdr-core--get parsed "method") "terminal.keys"))
    (should (equal (herdr-core--get params "keys") '("C-c" "x")))
    (should (eq (herdr-core--get params "force") t))
    (should (eq (herdr-core--get params "all") :false))
    (should (string-match-p "\"nothing\":null" text))))

(ert-deftest herdr-core-test-chunk-split ()
  (let ((original herdr-core--api)
        (lines nil))
    (setq herdr-core--api
          (list :role 'api :proc 'fake :inbuf ""
                :pending (make-hash-table :test 'equal)
                :next-id 1 :alive t))
    (unwind-protect
        (cl-letf (((symbol-function 'herdr-core--handle-line)
                   (lambda (_conn line) (push line lines))))
          (herdr-core--filter 'fake "one\ntw")
          (should (equal (plist-get herdr-core--api :inbuf) "tw"))
          (herdr-core--filter 'fake "o\n")
          (should (equal (plist-get herdr-core--api :inbuf) ""))
          (should (equal (nreverse lines) '("one" "two"))))
      (setq herdr-core--api original))))

;;; Status strings

(ert-deftest herdr-core-test-status-string ()
  (should (equal (herdr-core--status-string
                  (herdr-core-test--terminal
                   "t1" :status (list (cons "state" "working")
                                      (cons "progress" 45)
                                      (cons "msg" "")
                                      (cons "title" ""))))
                 "working 45%"))
  (should (equal (herdr-core--status-string
                  (herdr-core-test--terminal
                   "t1" :status (list (cons "state" "blocked")
                                      (cons "kind" "question")
                                      (cons "msg" "Allow?"))))
                 "blocked (question) Allow?"))
  (should (equal (herdr-core--status-string
                  (herdr-core-test--terminal "t1"))
                 "-"))
  (should (equal (herdr-core--status-string
                  (herdr-core-test--terminal
                   "t1" :state "finished" :exit_code 0 :status nil))
                 "finished (exit 0)")))

(defun put-alist (key value alist)
  "Set KEY to VALUE in ALIST, returning ALIST."
  (let ((cell (assoc key alist)))
    (if cell
        (setcdr cell value)
      (setq alist (cons (cons key value) alist)))
    alist))

(ert-deftest herdr-core-test-age-string ()
  (let ((terminal (herdr-core-test--terminal "t1")))
    (put-alist "status_ms" (- (truncate (* (float-time) 1000)) 90000) terminal)
    (should (equal (herdr-core--age-string terminal) "1m"))))

;;; Dashboard

(ert-deftest herdr-core-test-dashboard-entries ()
  (let ((herdr-core--terminals (make-hash-table :test 'equal))
        (herdr-core--filter-active nil))
    (puthash "t1"
             (herdr-core-test--terminal
              "t1" :name "build" :title "make\nall" :cwd "/home/nano/proj"
              :turn 3 :status_ms 10
              :status (list (cons "state" "working") (cons "progress" 45)
                            (cons "msg" "") (cons "title" "")))
             herdr-core--terminals)
    (let* ((entries (herdr-core--dashboard-entries))
           (entry (assoc "t1" entries)))
      (should entry)
      (should (equal (aref (cadr entry) 0) "build (t1)"))
      (should (equal (substring-no-properties (aref (cadr entry) 1))
                     "working 45% make all"))
      (should (equal (aref (cadr entry) 2) "make all"))
      (should (equal (aref (cadr entry) 3) "proj")))))

(ert-deftest herdr-core-test-dashboard-tree ()
  (let ((herdr-core--terminals (make-hash-table :test 'equal))
        (herdr-core--filter-active nil))
    (puthash "a" (herdr-core-test--terminal "a" :status_ms 100)
             herdr-core--terminals)
    (puthash "b" (herdr-core-test--terminal "b" :status_ms 200)
             herdr-core--terminals)
    (puthash "c" (herdr-core-test--terminal "c" :parent "a" :created_ms 5)
             herdr-core--terminals)
    (puthash "d" (herdr-core-test--terminal "d" :parent "a" :created_ms 3)
             herdr-core--terminals)
    (puthash "e" (herdr-core-test--terminal "e" :parent "zzz" :status_ms 50)
             herdr-core--terminals)
    (should (equal (mapcar #'car (herdr-core--dashboard-rows))
                   '("b" "a" "d" "c" "e")))
    (let ((entries (herdr-core--dashboard-entries)))
      (should (string-prefix-p "  \u2514 "
                               (aref (cadr (assoc "d" entries)) 0)))
      (should (equal (aref (cadr (assoc "a" entries)) 0) "a")))))

(ert-deftest herdr-core-test-dashboard-project-filter ()
  (let ((herdr-core--terminals (make-hash-table :test 'equal))
        (herdr-core--filter-active nil)
        (herdr-core--dashboard-project-root "/home/nano/proj/"))
    (puthash "t1" (herdr-core-test--terminal "t1" :cwd "/home/nano/proj/sub")
             herdr-core--terminals)
    (puthash "t2" (herdr-core-test--terminal "t2" :cwd "/home/other")
             herdr-core--terminals)
    (should (equal (mapcar #'car (herdr-core--dashboard-entries))
                   '("t1")))))

(ert-deftest herdr-core-test-dashboard-filter ()
  (let ((herdr-core--terminals (make-hash-table :test 'equal))
        (herdr-core--filter-active t)
        (herdr-core--dashboard-project-root nil)
        (herdr-core--seen-turn (make-hash-table :test 'equal)))
    (puthash "idle-seen"
             (herdr-core-test--terminal
              "idle-seen" :turn 2
              :status (list (cons "state" "idle")))
             herdr-core--terminals)
    (puthash "idle-new"
             (herdr-core-test--terminal
              "idle-new" :turn 3
              :status (list (cons "state" "idle")))
             herdr-core--terminals)
    (puthash "idle-seen" 2 herdr-core--seen-turn)
    (puthash "idle-new" 1 herdr-core--seen-turn)
    (should (equal (mapcar #'car (herdr-core--dashboard-entries))
                   '("idle-new")))))

;;; Context

(ert-deftest herdr-core-test-context-render-file ()
  (let* ((herdr-core-context-path-resolver nil)
         (herdr-core-context-entries nil)
         (file (make-temp-file "hc-ctx-" nil ".txt")))
    (unwind-protect
        (progn
          (write-region "hello\n" nil file nil 'silent)
          (herdr-core-context-add-file file)
          (should (= (length herdr-core-context-entries) 1))
          (should (equal (herdr-core-context-render herdr-core-context-entries)
                         (format "#### file: %s (text-mode)\n```text\nhello\n\n```\n"
                                 (expand-file-name file)))))
      (delete-file file))))

(ert-deftest herdr-core-test-context-render-buffer ()
  (let ((herdr-core-context-path-resolver nil)
        (herdr-core-context-entries nil)
        (buffer (generate-new-buffer "hc-ctx-buffer")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (fundamental-mode)
            (insert "abc"))
          (herdr-core-context-add-buffer buffer)
          (should (equal (herdr-core-context-render herdr-core-context-entries)
                         "#### file: hc-ctx-buffer (fundamental-mode)\n```text\nabc\n```\n")))
      (kill-buffer buffer))))

(ert-deftest herdr-core-test-context-render-region ()
  (let ((herdr-core-context-path-resolver nil)
        (herdr-core-context-entries nil)
        (buffer (generate-new-buffer "ctx")))
    (unwind-protect
        (with-current-buffer buffer
          (fundamental-mode)
          (insert "line1\nline2\n")
          (herdr-core-context-add-region (point-min) (point-max))
          (should (equal (herdr-core-context-render herdr-core-context-entries)
                         "#### buffer: ctx (lines 1-2)\n```text\nline1\nline2\n\n```\n")))
      (kill-buffer buffer))))

(ert-deftest herdr-core-test-context-render-remote-path ()
  (let* ((herdr-core-context-entries nil)
         (herdr-core-context-path-resolver (lambda (_path) "/remote/x.txt"))
         (file (make-temp-file "hc-ctx-" nil ".txt")))
    (unwind-protect
        (progn
          (write-region "hello\n" nil file nil 'silent)
          (herdr-core-context-add-file file)
          (should (plist-get (car herdr-core-context-entries) :remote-path))
          (should (null (plist-get (car herdr-core-context-entries) :text)))
          (should (equal (herdr-core-context-render herdr-core-context-entries)
                         "#### file (on the agent's machine): /remote/x.txt\nRead it there.\n")))
      (delete-file file))))

(ert-deftest herdr-core-test-context-lang ()
  (should (equal (herdr-core--context-lang 'emacs-lisp-mode) "emacs-lisp"))
  (should (equal (herdr-core--context-lang 'fundamental-mode) "text")))

(ert-deftest herdr-core-test-context-truncate ()
  (let ((herdr-core-context-max-chars 30)
        (entry (list :type 'file :label "file: x" :path "/x"
                     :mode 'text-mode
                     :text (make-string 100 ?a))))
    (let ((rendered (herdr-core-context-render (list entry))))
      (should (string-match-p "\\[context truncated\\]" rendered))
      (should (< (length rendered) 100)))))

(ert-deftest herdr-core-test-context-binary-rejected ()
  (let* ((herdr-core-context-path-resolver nil)
         (herdr-core-context-entries nil)
         (file (make-temp-file "hc-bin-" nil ".bin")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region "a\0b" nil file nil 'silent))
          (let ((error (should-error (herdr-core-context-add-file file)
                                     :type 'user-error)))
            (should (string-match-p "put-file"
                                    (error-message-string error)))))
      (delete-file file))))

(ert-deftest herdr-core-test-context-hook ()
  (let* ((herdr-core-context-path-resolver nil)
         (herdr-core-context-entries nil)
         (ran 0)
         (function (lambda () (setq ran (1+ ran)))))
    (add-hook 'herdr-core-context-changed-hook function)
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "x")
            (herdr-core-context-add-buffer))
          (should (= (length herdr-core-context-entries) 1))
          (should (= ran 1))
          (herdr-core-context-remove (car herdr-core-context-entries))
          (should (= (length herdr-core-context-entries) 0))
          (should (= ran 2)))
      (remove-hook 'herdr-core-context-changed-hook function))))

;;; Chat

(ert-deftest herdr-core-test-compose-send ()
  (let* ((herdr-core-context-entries
          (list (list :type 'file :label "file: /tmp/a.txt" :path "/tmp/a.txt"
                      :mode 'text-mode :text "hello")))
         (sent nil))
    (cl-letf (((symbol-function 'herdr-core--request)
               (herdr-core-test--stub
                (lambda (method params)
                  (when (equal method "terminal.send")
                    (setq sent (cons method params)))
                  (herdr-core-test--default-answer method params)))))
      (with-temp-buffer
        (herdr-core-compose-mode)
        (setq-local herdr-core--target "t7")
        (insert "do it")
        (herdr-core-compose-send)
        (should (equal (buffer-string) ""))))
    (should (equal (car sent) "terminal.send"))
    (should (equal (cdr (assoc "text" (cdr sent)))
                   (concat (herdr-core-context-render herdr-core-context-entries)
                           "\n### Message\ndo it")))))

(ert-deftest herdr-core-test-send-plain ()
  (let ((sent nil))
    (cl-letf (((symbol-function 'herdr-core--request)
               (herdr-core-test--stub
                (lambda (method params)
                  (when (equal method "terminal.send")
                    (setq sent params))
                  (herdr-core-test--default-answer method params)))))
      (herdr-core-send "hello" "t7"))
    (should (equal (cdr (assoc "text" sent)) "hello"))
    (should (null (assoc "force" sent)))))

(ert-deftest herdr-core-test-send-blocked-force ()
  (let ((calls nil)
        (answers (list (cons nil (list (cons "message" "blocked")
                                       (cons "data" (list (cons "kind" "blocked")))))
                       (cons (list (cons "turn" 2) (cons "started" t)) nil))))
    (cl-letf (((symbol-function 'herdr-core--request)
               (lambda (method params callback &optional _timeout)
                 (push (cons method params) calls)
                 (let ((answer (pop answers)))
                   (funcall callback (car answer) (cdr answer)))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'herdr-core--refresh) (lambda (&optional _) nil)))
      (herdr-core-send "hi" "t7"))
    (should (= (length calls) 2))
    (let ((first (cdr (nth 1 calls)))
          (second (cdr (nth 0 calls))))
      (should (equal (car (nth 1 calls)) "terminal.send"))
      (should (null (assoc "force" first)))
      (should (equal (cdr (assoc "force" second)) t))
      (should (equal (cdr (assoc "text" second)) "hi")))))

;;; Attach

(ert-deftest herdr-core-test-attach-command ()
  (let ((herdr-core-host "agentbox")
        (herdr-core-session "my session")
        (herdr-core-attach-observe t)
        (herdr-core-attach-history 500))
    (should (equal (herdr-core--attach-command "t7")
                   '("ssh" "-t" "-o" "BatchMode=yes" "-o" "ConnectTimeout=10"
                     "agentbox"
                     "HC_SESSION='my session' $HOME/.local/bin/hc attach 't7' --history 500 --observe"))))
  (let ((herdr-core-host nil)
        (herdr-core-session "my session")
        (herdr-core-attach-observe t)
        (herdr-core-attach-history 500))
    (should (equal (herdr-core--attach-command "t7")
                   (list "sh" "-c"
                         "HC_SESSION='my session' $HOME/.local/bin/hc attach 't7' --history 500 --observe")))))

(ert-deftest herdr-core-test-attach-backend-auto ()
  (let ((herdr-core-attach-backend 'auto))
    (should (eq (herdr-core--attach-backend) 'mirror))
    (cl-letf (((symbol-function 'locate-library)
               (lambda (name &rest _) (and (equal name "eat") "/fake/eat.el"))))
      (should (eq (herdr-core--attach-backend) 'eat)))
    (cl-letf (((symbol-function 'locate-library)
               (lambda (name &rest _) (and (equal name "vterm") "/fake/vterm.el"))))
      (should (eq (herdr-core--attach-backend) 'vterm)))
    (cl-letf (((symbol-function 'locate-library)
               (lambda (name &rest _) (and (equal name "ghostel") "/fake/ghostel.el"))))
      (should (eq (herdr-core--attach-backend) 'ghostel)))))

(ert-deftest herdr-core-test-attach-backend-explicit ()
  (let ((herdr-core-attach-backend 'eat))
    (should (eq (herdr-core--attach-backend) 'eat)))
  (let ((herdr-core-attach-backend 'mirror))
    (should (eq (herdr-core--attach-backend) 'mirror))))

(ert-deftest herdr-core-test-attach-ghostel-call ()
  (let ((herdr-core-host nil)
        (herdr-core-session nil)
        (herdr-core-attach-history nil)
        (herdr-core-attach-observe nil)
        (herdr-core-attach-backend 'auto)
        (captured nil))
    (provide 'ghostel)
    (unwind-protect
        (cl-letf (((symbol-function 'locate-library)
                   (lambda (name &rest _)
                     (and (equal name "ghostel") "/fake/ghostel.el")))
                  ((symbol-function 'ghostel-exec)
                   (lambda (buffer program &optional args _identity)
                     (setq captured (list buffer program args)))))
          (herdr-core-attach "t7")
          (should (bufferp (nth 0 captured)))
          (should (equal (buffer-name (nth 0 captured)) "*hc:t7 terminal*"))
          (should (equal (nth 1 captured) "sh"))
          (should (equal (nth 2 captured)
                         (cdr (herdr-core--attach-command "t7")))))
      (setq features (delq 'ghostel features))
      (let ((buffer (get-buffer "*hc:t7 terminal*")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

;;; Projects

(ert-deftest herdr-core-test-remote-path-tramp ()
  (let ((herdr-core-host "agentbox")
        (herdr-core-project-root-map nil))
    (should (equal (herdr-core--remote-path "/ssh:agentbox:/home/nano/src/x")
                   "/home/nano/src/x"))
    (should (null (herdr-core--remote-path "/ssh:other:/home/nano/src/x")))
    (should (equal (herdr-core--remote-path "/ssh:user@agentbox:/home/nano/src/x")
                   "/home/nano/src/x"))))

(ert-deftest herdr-core-test-remote-path-map ()
  (let ((herdr-core-host "agentbox")
        (herdr-core-project-root-map '(("C:/src/" . "/home/nano/src/"))))
    (should (equal (herdr-core--remote-path "C:/src/x") "/home/nano/src/x"))))

(ert-deftest herdr-core-test-remote-path-prompt ()
  (let ((herdr-core-host "agentbox")
        (herdr-core-project-root-map nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "/home/nano/src/")))
      (should (equal (herdr-core--remote-path "/tmp/proj/") "/home/nano/src/"))
      (should (equal herdr-core-project-root-map
                     '(("/tmp/proj/" . "/home/nano/src/")))))))

(ert-deftest herdr-core-test-remote-path-no-prompt ()
  (let ((herdr-core-host "agentbox")
        (herdr-core-project-root-map nil))
    (should (null (herdr-core--remote-path "/tmp/unmapped" t)))))

(ert-deftest herdr-core-test-remote-path-local ()
  (let ((herdr-core-host nil)
        (herdr-core-project-root-map nil))
    (should (equal (herdr-core--remote-path "/tmp/x") "/tmp/x"))))

(ert-deftest herdr-core-test-spawn-line ()
  (let ((herdr-core--templates (list (list (cons "name" "omp"))))
        (herdr-core--templates-fetched t)
        (captured nil))
    (cl-letf (((symbol-function 'herdr-core--request)
               (lambda (method params callback &optional _timeout)
                 (setq captured (cons method params))
                 (funcall callback (list (cons "address" "t9")) nil)))
              ((symbol-function 'herdr-core--refresh) (lambda (&optional _) nil))
              ((symbol-function 'herdr-core--remember-agent) (lambda (_) nil))
              ((symbol-function 'herdr-core--attach-backend) (lambda () 'mirror))
              ((symbol-function 'herdr-core-terminal) (lambda (_) nil)))
      (herdr-core--spawn-agent "claude --model x" "/home/nano/src/x/" "do it" "x"))
    (should (equal (car captured) "terminal.run"))
    (let ((params (cdr captured)))
      (should (equal (cdr (assoc "cwd" params)) "/home/nano/src/x/"))
      (should (equal (cdr (assoc "name" params)) "x"))
      (should (eq (cdr (assoc "no_parent" params)) t))
      (should (equal (cdr (assoc "line" params))
                     "claude --model x 'do it'"))
      (should (null (assoc "argv" params))))))

(ert-deftest herdr-core-test-spawn-template ()
  (let ((herdr-core--templates (list (list (cons "name" "omp"))))
        (herdr-core--templates-fetched t)
        (captured nil))
    (cl-letf (((symbol-function 'herdr-core--request)
               (lambda (method params callback &optional _timeout)
                 (setq captured (cons method params))
                 (funcall callback (list (cons "address" "t9")) nil)))
              ((symbol-function 'herdr-core--refresh) (lambda (&optional _) nil))
              ((symbol-function 'herdr-core--remember-agent) (lambda (_) nil))
              ((symbol-function 'herdr-core--attach-backend) (lambda () 'mirror))
              ((symbol-function 'herdr-core-terminal) (lambda (_) nil)))
      (herdr-core--spawn-agent "omp" "/home/nano/src/x/" "do it" "x"))
    (let ((params (cdr captured)))
      (should (equal (cdr (assoc "argv" params)) '("omp" "do it")))
      (should (null (assoc "line" params))))))

(defun herdr-core-test--app-doc ()
  "Return an app document with spaces for adoption tests."
  (list (cons "spaces"
              (list (list (cons "id" "spA")
                          (cons "name" "milvus")
                          (cons "folder" "/tmp")
                          (cons "tabs" (list (list (cons "id" "tbA")))))
                    (list (cons "id" "spB")
                          (cons "name" "herdr")
                          (cons "folder" "/home/nano/herdr")
                          (cons "tabs" (list (list (cons "id" "tbB")))))
                    (list (cons "id" "spC")
                          (cons "name" "wide")
                          (cons "folder" "/home/nano")
                          (cons "tabs" (list (list (cons "id" "tbC")))))
                    (list (cons "id" "spD")
                          (cons "name" "empty")
                          (cons "folder" "")
                          (cons "tabs" nil))))))

(defun herdr-core-test--new-tab-p (tab)
  "Return non-nil when TAB is a fresh tab id."
  (and (stringp tab)
       (string-prefix-p "tb" tab)
       (= (length tab) 34)))

(defun herdr-core-test--labels-p (params space)
  "Return non-nil when PARAMS carry labels for SPACE and a fresh tab."
  (let ((labels (cdr (assoc "labels" params))))
    (and (equal (cdr (assoc "herdr.space" labels)) space)
         (herdr-core-test--new-tab-p (cdr (assoc "herdr.tab" labels))))))

(defun herdr-core-test--doc-with-terminal (space-id tab-id terminal)
  "Return an app document whose SPACE-ID has TERMINAL in its first tab."
  (list (cons "spaces"
              (list (list (cons "id" space-id)
                          (cons "name" "")
                          (cons "folder" "/home/nano")
                          (cons "tabs"
                                (list
                                 (list (cons "id" tab-id)
                                       (cons "root"
                                             (list
                                              (cons "children"
                                                    (list
                                                     (list
                                                      (cons "content"
                                                            (list (cons "terminal"
                                                                        terminal))))))))))))))))

(ert-deftest herdr-core-test-adopt-target-by-name ()
  (let ((herdr-core-adopt-space t)
        (doc (herdr-core-test--app-doc)))
    (dolist (directory '("/home/nano/milvus" "/home/nano/Milvus/"))
      (let ((target (herdr-core--adopt-target doc directory)))
        (should (equal (car target) "spA"))
        (should (herdr-core-test--new-tab-p (cdr target)))
        (should (not (equal (cdr target) "tbA")))))))

(ert-deftest herdr-core-test-adopt-target-at-folder ()
  (let ((herdr-core-adopt-space t)
        (doc (herdr-core-test--app-doc)))
    (let ((target (herdr-core--adopt-target doc "/home/nano")))
      (should (equal (car target) "spC"))
      (should (herdr-core-test--new-tab-p (cdr target))))
    ;; A folder no longer captures everything below it.
    (dolist (directory '("/home/nano/herdr/sub" "/home/nano/other"))
      (let ((target (herdr-core--adopt-target doc directory)))
        (should (string-prefix-p "sp" (car target)))
        (should (not (member (car target) '("spB" "spC"))))))))

(ert-deftest herdr-core-test-adopt-target-explicit-name ()
  (let ((herdr-core-adopt-space "herdr")
        (doc (herdr-core-test--app-doc)))
    (let ((target (herdr-core--adopt-target doc nil)))
      (should (equal (car target) "spB"))
      (should (herdr-core-test--new-tab-p (cdr target))))
    (should (equal (car (herdr-core--adopt-target doc "/srv/elsewhere"))
                   "spB"))
    (let ((herdr-core-adopt-space "missing"))
      (should (null (herdr-core--adopt-target doc "/tmp"))))))

(ert-deftest herdr-core-test-adopt-target-space-without-tabs ()
  (let ((herdr-core-adopt-space "empty")
        (doc (herdr-core-test--app-doc)))
    (should (null (herdr-core--adopt-target doc "/home/nano")))))

(ert-deftest herdr-core-test-adopt-default ()
  (should (eq (default-value 'herdr-core-adopt-space) t)))

(ert-deftest herdr-core-test-adopt-target-first-terminal ()
  (let* ((herdr-core-adopt-space t)
         (herdr-core--terminals (make-hash-table :test 'equal))
         (doc (herdr-core-test--doc-with-terminal "spT" "tbT" "t3")))
    (puthash "t3" (herdr-core-test--terminal "t3" :cwd "/home/nano/proj")
             herdr-core--terminals)
    (let ((target (herdr-core--adopt-target doc "/home/nano/proj")))
      (should (equal (car target) "spT"))
      (should (herdr-core-test--new-tab-p (cdr target)))
      (should (not (equal (cdr target) "tbT"))))
    ;; Another project does not join the space.
    (let ((target (herdr-core--adopt-target doc "/home/nano/other")))
      (should (string-prefix-p "sp" (car target)))
      (should (not (equal (car target) "spT"))))))

(ert-deftest herdr-core-test-adopt-target-new-space ()
  (let ((herdr-core-adopt-space t)
        (herdr-core--terminals (make-hash-table :test 'equal)))
    (let ((a (herdr-core--adopt-target nil "/home/nano/new"))
          (b (herdr-core--adopt-target nil "/home/nano/new")))
      (should (string-prefix-p "sp" (car a)))
      (should (equal (car a) (car b)))
      (should (herdr-core-test--new-tab-p (cdr a)))
      (should (not (equal (cdr a) (cdr b)))))))

(ert-deftest herdr-core-test-path-key ()
  (should (equal (herdr-core--path-key "/home/nano/proj/")
                 "/home/nano/proj"))
  (when (and (eq system-type 'windows-nt) (null herdr-core-host))
    (should (equal (herdr-core--path-key "C:\\Users\\Anil\\Proj\\")
                   (herdr-core--path-key "c:/Users/Anil/Proj")))))

(ert-deftest herdr-core-test-adopt-target-project-hint ()
  (let ((herdr-core-adopt-space t)
        (doc (herdr-core-test--app-doc)))
    ;; The hint wins over a directory that matches nothing.
    (let ((target (herdr-core--adopt-target
                   doc "/home/nano/milvus/sub" "milvus")))
      (should (equal (car target) "spA"))
      (should (herdr-core-test--new-tab-p (cdr target))))
    (should (equal (car (herdr-core--adopt-target doc "/srv/other" "herdr"))
                   "spB"))))

(ert-deftest herdr-core-test-project-hint ()
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _) 'project))
            ((symbol-function 'project-root)
             (lambda (_) "/home/me/projects/milvus/")))
    (should (equal (herdr-core--project-hint) "milvus")))
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _) nil)))
    (should (null (herdr-core--project-hint)))))

(ert-deftest herdr-core-test-project-spawn-uses-project-name ()
  (let ((herdr-core-adopt-space t)
        (herdr-core--templates nil)
        (herdr-core--templates-fetched t)
        (run-params nil)
        (doc (list
              (cons "spaces"
                    (list (list (cons "id" "spProj")
                                (cons "name" "checkout")
                                (cons "folder" "")
                                (cons "tabs"
                                      (list (list (cons "id" "tbProj"))))))))))
    (cl-letf (((symbol-function 'herdr-core--local-project-root)
               (lambda () "/home/me/projects/checkout/"))
              ((symbol-function 'herdr-core--remote-path)
               (lambda (_path &optional _no-prompt) "/srv/code/"))
              ((symbol-function 'herdr-core--read-agent-command)
               (lambda () "claude"))
              ((symbol-function 'herdr-core--read-spawn-name)
               (lambda (_) "x"))
              ((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'herdr-core--request)
               (lambda (method params callback &optional _timeout)
                 (if (equal method "doc.get")
                     (funcall callback (list (cons "value" doc)) nil)
                   (setq run-params params)
                   (funcall callback (list (cons "address" "t9")) nil))))
              ((symbol-function 'herdr-core--refresh) (lambda (&optional _) nil))
              ((symbol-function 'herdr-core--remember-agent) (lambda (_) nil))
              ((symbol-function 'herdr-core--attach-backend) (lambda () 'mirror))
              ((symbol-function 'herdr-core-terminal) (lambda (_) nil)))
      (herdr-core-project-spawn-agent))
    (should (herdr-core-test--labels-p run-params "spProj"))))

(ert-deftest herdr-core-test-spawn-adopts ()
  (let ((herdr-core-adopt-space t)
        (herdr-core--templates nil)
        (herdr-core--templates-fetched t)
        (run-params nil)
        (methods nil))
    (cl-letf (((symbol-function 'herdr-core--request)
               (lambda (method params callback &optional _timeout)
                 (push method methods)
                 (if (equal method "doc.get")
                     (funcall callback
                              (list (cons "value" (herdr-core-test--app-doc)))
                              nil)
                   (setq run-params params)
                   (funcall callback (list (cons "address" "t9")) nil))))
              ((symbol-function 'herdr-core--refresh) (lambda (&optional _) nil))
              ((symbol-function 'herdr-core--remember-agent) (lambda (_) nil))
              ((symbol-function 'herdr-core--attach-backend) (lambda () 'mirror))
              ((symbol-function 'herdr-core-terminal) (lambda (_) nil)))
      (herdr-core--spawn-agent "claude" "/home/nano/milvus" "" "x"))
    (should (equal (nreverse methods) '("doc.get" "terminal.run")))
    (should (herdr-core-test--labels-p run-params "spA"))
    (should (eq (cdr (assoc "no_parent" run-params)) t))))

(ert-deftest herdr-core-test-spawn-without-adoption ()
  (let ((herdr-core-adopt-space nil)
        (herdr-core--templates nil)
        (herdr-core--templates-fetched t)
        (run-params nil)
        (methods nil))
    (cl-letf (((symbol-function 'herdr-core--request)
               (lambda (method params callback &optional _timeout)
                 (push method methods)
                 (setq run-params params)
                 (funcall callback (list (cons "address" "t9")) nil)))
              ((symbol-function 'herdr-core--refresh) (lambda (&optional _) nil))
              ((symbol-function 'herdr-core--remember-agent) (lambda (_) nil))
              ((symbol-function 'herdr-core--attach-backend) (lambda () 'mirror))
              ((symbol-function 'herdr-core-terminal) (lambda (_) nil)))
      (herdr-core--spawn-agent "claude" "/home/nano/milvus" "" "x"))
    (should (equal methods '("terminal.run")))
    (should (null (assoc "labels" run-params)))))

(ert-deftest herdr-core-test-run-adopts-explicit-space ()
  (let ((herdr-core-adopt-space "milvus")
        (herdr-core--templates nil)
        (herdr-core--templates-fetched t)
        (run-params nil))
    (cl-letf (((symbol-function 'herdr-core--request)
               (lambda (method params callback &optional _timeout)
                 (if (equal method "doc.get")
                     (funcall callback
                              (list (cons "value" (herdr-core-test--app-doc)))
                              nil)
                   (setq run-params params)
                   (funcall callback (list (cons "address" "t9")) nil))))
              ((symbol-function 'herdr-core--refresh) (lambda (&optional _) nil))
              ((symbol-function 'herdr-core-terminal) (lambda (_) nil)))
      (herdr-core-run "true"))
    (should (herdr-core-test--labels-p run-params "spA"))))

(ert-deftest herdr-core-test-project-agent-newest ()
  (let ((herdr-core--terminals (make-hash-table :test 'equal))
        (herdr-core--project-agents (make-hash-table :test 'equal))
        (herdr-core-project-root-map '(("/tmp/proj/" . "/home/nano/proj/")))
        (herdr-core-host "agentbox"))
    (puthash "t1"
             (herdr-core-test--terminal "t1" :cwd "/home/nano/proj/a"
                                        :status_ms 10)
             herdr-core--terminals)
    (puthash "t2"
             (herdr-core-test--terminal "t2" :cwd "/home/nano/proj/b"
                                        :status_ms 20)
             herdr-core--terminals)
    (puthash "t3"
             (herdr-core-test--terminal "t3" :cwd "/home/nano/other"
                                        :status_ms 30)
             herdr-core--terminals)
    (should (equal (herdr-core--project-agent-address "/tmp/proj/") "t2"))))

(ert-deftest herdr-core-test-remember-agent ()
  (let ((herdr-core--terminals (make-hash-table :test 'equal))
        (herdr-core--project-agents (make-hash-table :test 'equal))
        (herdr-core-project-root-map '(("/tmp/proj/" . "/home/nano/proj/")))
        (herdr-core-host "agentbox")
        (default-directory "/tmp/proj/"))
    (puthash "t1"
             (herdr-core-test--terminal "t1" :cwd "/home/nano/proj/a")
             herdr-core--terminals)
    (herdr-core--remember-agent "t1")
    (should (equal herdr-core--last-agent "t1"))
    (should (equal (gethash "/tmp/proj/" herdr-core--project-agents) "t1"))))

;;; Images

(ert-deftest herdr-core-test-put-file ()
  (let* ((file (make-temp-file "hc-img-" nil ".png"))
         (captured nil))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region (base64-decode-string
                           "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")
                          nil file nil 'silent))
          (cl-letf (((symbol-function 'herdr-core--request)
                     (lambda (method params callback &optional _timeout)
                       (setq captured (cons method params))
                       (funcall callback
                                (list (cons "path" "/tmp/up.png")
                                      (cons "bytes" 70))
                                nil))))
            (herdr-core-put-file file "t7"))
          (should (equal (car captured) "terminal.put"))
          (let ((params (cdr captured)))
            (should (equal (cdr (assoc "name" params))
                           (file-name-nondirectory file)))
            (should (string-suffix-p ".png" (cdr (assoc "name" params))))
            (should (equal (base64-decode-string (cdr (assoc "data" params)))
                           (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally file)
                             (buffer-string))))))
      (delete-file file))))

;;; Integration against the real local core

(defun herdr-core-test--read-contains (address needle timeout)
  "Poll ADDRESS until its screen contains NEEDLE, up to TIMEOUT seconds."
  (let ((deadline (+ (float-time) timeout))
        (found nil))
    (while (and (not found) (< (float-time) deadline))
      (let ((result (ignore-errors
                      (herdr-core--request-sync
                       "terminal.read"
                       (list (cons "terminal" address) (cons "lines" 0))
                       5))))
        (when (and result
                   (string-match-p (regexp-quote needle)
                                   (or (herdr-core--get result "text") "")))
          (setq found t)))
      (unless found (accept-process-output nil 0.2)))
    found))

(defun herdr-core-test--wait-finished (address timeout)
  "Wait until ADDRESS reports finished, up to TIMEOUT seconds."
  (let ((deadline (+ (float-time) timeout))
        (done nil))
    (while (and (not done) (< (float-time) deadline))
      (let ((result (ignore-errors
                      (herdr-core--request-sync
                       "terminal.get" (list (cons "terminal" address)) 5))))
        (when (and result (equal (herdr-core--get result "state") "finished"))
          (setq done t)))
      (unless done (accept-process-output nil 0.2)))
    done))

(defun herdr-core-test--discard (address)
  "Kill and remove ADDRESS."
  (ignore-errors
    (herdr-core--request-sync "terminal.kill"
                              (list (cons "terminal" address)) 10))
  (herdr-core-test--wait-finished address 10)
  (ignore-errors
    (herdr-core--request-sync "terminal.rm"
                              (list (cons "terminal" address)) 10)))

(ert-deftest herdr-core-test-integration-list ()
  (let ((result (herdr-core--request-sync
                 "terminal.list" (list (cons "all" :false)) 10)))
    (should (listp (herdr-core--get result "terminals")))))

(ert-deftest herdr-core-test-integration-run-read ()
  (let* ((name (format "hc-emacs-smoke-%d" (emacs-pid)))
         (run (herdr-core--request-sync
               "terminal.run"
               (list (cons "argv" (list "sh" "-c"
                                        "printf 'HC-SMOKE\\n'; sleep 60"))
                     (cons "name" name))
               20))
         (address (herdr-core--get run "address")))
    (unwind-protect
        (progn
          (should (equal (herdr-core--get run "name") name))
          (should (herdr-core-test--read-contains address "HC-SMOKE" 10)))
      (herdr-core-test--discard address))))

(ert-deftest herdr-core-test-integration-cwd ()
  (let* ((directory (make-temp-file "hc-cwd-" t))
         (run (herdr-core--request-sync
               "terminal.run"
               (list (cons "line" "pwd")
                     (cons "cwd" directory)
                     (cons "name" (format "hc-emacs-cwd-%d" (emacs-pid))))
               20))
         (address (herdr-core--get run "address")))
    (unwind-protect
        (progn
          (should (herdr-core-test--read-contains
                   address (directory-file-name directory) 10))
          (should (herdr-core-test--wait-finished address 10)))
      (herdr-core-test--discard address)
      (delete-directory directory t))))

(ert-deftest herdr-core-test-integration-send-wait ()
  (let* ((run (herdr-core--request-sync
               "terminal.run"
               (list (cons "argv" (list "sh" "-c"
                                        "IFS= read -r l; sleep 3; printf 'GOT:%s\\n' \"$l\""))
                     (cons "name" (format "hc-emacs-send-%d" (emacs-pid))))
               20))
         (address (herdr-core--get run "address")))
    (unwind-protect
        (progn
          (herdr-core--request-sync
           "terminal.send"
           (list (cons "terminal" address) (cons "text" "h\u00e9llo \u03c0"))
           20)
          (let ((result (herdr-core--request-sync
                         "terminal.wait"
                         (list (cons "terminal" address)
                               (cons "output" "GOT:")
                               (cons "timeout" 10))
                         30)))
            (should (herdr-core--true-p (herdr-core--get result "ended")))
            (should (equal (herdr-core--get result "reason") "output")))
          (should (herdr-core-test--read-contains address "GOT:h\u00e9llo \u03c0" 10)))
      (herdr-core-test--discard address))))

(ert-deftest herdr-core-test-integration-image ()
  (let* ((png "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")
         (run (herdr-core--request-sync
               "terminal.run"
               (list (cons "argv" (list "sh" "-c" "sleep 60"))
                     (cons "name" (format "hc-emacs-img-%d" (emacs-pid))))
               20))
         (address (herdr-core--get run "address")))
    (unwind-protect
        (let* ((put (herdr-core--request-sync
                     "terminal.put"
                     (list (cons "terminal" address)
                           (cons "data" png)
                           (cons "name" "smoke.png"))
                     20))
               (path (herdr-core--get put "path")))
          (should (string-suffix-p ".png" path))
          (let ((exec (herdr-core--request-sync
                       "machine.exec"
                       (list (cons "argv" (list "cat" path))
                             (cons "timeout" 30))
                       30)))
            (should (equal (herdr-core--get exec "code") 0))
            (should (equal (base64-decode-string (herdr-core--get exec "stdout"))
                           (base64-decode-string png)))
            (when (image-type-available-p 'png)
              (should (create-image
                       (base64-decode-string (herdr-core--get exec "stdout"))
                       'png t)))))
      (herdr-core-test--discard address))))

(ert-deftest herdr-core-test-integration-events ()
  (setq herdr-core--events-subscribed nil
        herdr-core--event-count 0)
  (herdr-core--events-start)
  (should (herdr-core--request-sync
           "terminal.list" (list (cons "all" :false)) 10))
  (let* ((run (herdr-core--request-sync
               "terminal.run"
               (list (cons "argv" (list "sh" "-c" "sleep 0.5"))
                     (cons "name" (format "hc-emacs-ev-%d" (emacs-pid))))
               20))
         (address (herdr-core--get run "address"))
         (deadline (+ (float-time) 5)))
    (unwind-protect
        (progn
          (while (and (= herdr-core--event-count 0)
                      (< (float-time) deadline))
            (accept-process-output nil 0.1))
          (should (> herdr-core--event-count 0)))
      (herdr-core-test--discard address))))

(ert-deftest herdr-core-test-integration-concurrent ()
  (let ((calls 0)
        (done 0)
        (answers nil))
    (dotimes (_ 10)
      (herdr-core--request
       "terminal.list" (list (cons "all" :false))
       (lambda (result error)
         (setq calls (1+ calls))
         (push (cons result error) answers)
         (setq done (1+ done)))))
    (let ((deadline (+ (float-time) 15)))
      (while (and (< done 10) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (= calls 10))
    (should (= done 10))
    (should (cl-every (lambda (answer) (null (cdr answer))) answers))))

;;; herdr-core-test.el ends here
