;;; herdr-core-attach.el --- Real terminals for herdr-core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The herdr-core.el authors

;; Author: herdr-core.el contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools, terminals
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; herdr-core-attach.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; herdr-core-attach.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; herdr-core-attach.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Attachment of a herdr-core terminal to a real terminal emulator inside
;; Emacs: ghostel (preferred), vterm or eat.  `hc attach' runs on the agent
;; machine over ssh with a tty; the emulator owns the process.  When no
;; emulator is available the text mirror of `herdr-core' is used instead.

;;; Code:

(require 'herdr-core)

(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function vterm "vterm" (&optional buffer-name))
(declare-function eat-exec "eat" (buffer name command startfile switches))
(declare-function eat-mode "eat" ())

(defvar vterm-shell)
(defvar vterm-buffer-name)
(defvar eat-term-name)
(defvar ghostel--process)

(defun herdr-core--attach-backend ()
  "Return the terminal backend to use.
With `herdr-core-attach-backend' set to `auto', prefer ghostel, then vterm,
then eat, then the text mirror."
  (if (eq herdr-core-attach-backend 'auto)
      (cond
       ((or (featurep 'ghostel) (locate-library "ghostel")) 'ghostel)
       ((locate-library "vterm") 'vterm)
       ((locate-library "eat") 'eat)
       (t 'mirror))
    herdr-core-attach-backend))

(defun herdr-core--attach-command (address)
  "Return the argv that attaches ADDRESS in a terminal emulator."
  (let ((remote (concat
                 (when herdr-core-session
                   (concat "HC_SESSION="
                           (herdr-core--posix-quote herdr-core-session)
                           " "))
                 herdr-core-attach-command
                 " " (herdr-core--posix-quote address)
                 (when herdr-core-attach-history
                   (concat " --history "
                           (number-to-string herdr-core-attach-history)))
                 (when herdr-core-attach-observe " --observe"))))
    (if herdr-core-host
        (cons herdr-core-ssh-program
              (append herdr-core-attach-ssh-args (list herdr-core-host remote)))
      (list "sh" "-c" remote))))

(defun herdr-core--attach-buffer-live-p (backend buffer)
  "Return non-nil when BUFFER holds a live BACKEND terminal."
  (and (buffer-live-p buffer)
       (cond
        ((eq backend 'ghostel)
         (and (boundp 'ghostel--process)
              (with-current-buffer buffer
                (process-live-p ghostel--process))))
        (t
         (let ((process (get-buffer-process buffer)))
           (and process (process-live-p process)))))))

(defun herdr-core--attach-ghostel (buffer address)
  "Run `hc attach ADDRESS' in BUFFER with ghostel."
  (require 'ghostel)
  (unless (fboundp 'ghostel-exec)
    (user-error "herdr-core: ghostel-exec is unavailable; update ghostel"))
  (let ((command (herdr-core--attach-command address)))
    (funcall 'ghostel-exec buffer (car command) (cdr command))))

(defun herdr-core--attach-vterm (buffer address)
  "Run `hc attach ADDRESS' in BUFFER with vterm."
  (require 'vterm)
  (let* ((command (herdr-core--attach-command address))
         (vterm-buffer-name (buffer-name buffer))
         (vterm-shell (mapconcat #'shell-quote-argument command " ")))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))
    (funcall 'vterm vterm-buffer-name)
    (get-buffer vterm-buffer-name)))

(defun herdr-core--attach-eat (buffer address)
  "Run `hc attach ADDRESS' in BUFFER with eat."
  (require 'eat)
  (let ((command (herdr-core--attach-command address)))
    (with-current-buffer buffer
      (let ((eat-term-name "xterm-256color"))
        (unless (derived-mode-p 'eat-mode)
          (funcall 'eat-mode))
        (funcall 'eat-exec buffer (buffer-name buffer)
                 (car command) nil (cdr command))))))

(defun herdr-core--attach-with (backend address)
  "Attach ADDRESS using BACKEND, creating its buffer when needed."
  (let ((buffer (get-buffer-create (format "*hc:%s terminal*" address))))
    (if (herdr-core--attach-buffer-live-p backend buffer)
        (pop-to-buffer buffer)
      (let ((target
             (cond
              ((eq backend 'ghostel)
               (herdr-core--attach-ghostel buffer address)
               buffer)
              ((eq backend 'vterm)
               (herdr-core--attach-vterm buffer address))
              ((eq backend 'eat)
               (herdr-core--attach-eat buffer address)
               buffer)
              (t
               (herdr-core-terminal address)
               nil))))
        (when (buffer-live-p target)
          (pop-to-buffer target))))))

(defun herdr-core--attach-order ()
  "Return the backends to try, best first, ending at the mirror."
  (if (eq herdr-core-attach-backend 'auto)
      '(ghostel vterm eat mirror)
    (list herdr-core-attach-backend)))

(defun herdr-core--attach-impl (address)
  "Attach ADDRESS, falling back along the backend order to the mirror."
  (let* ((terminal (gethash address herdr-core--terminals))
         (state (herdr-core--get terminal "state")))
    (when (equal state "finished")
      (user-error "terminal finished"))
    (herdr-core--mark-seen address)
    (when (and (not (eq herdr-core-attach-backend 'auto))
               (not (eq herdr-core-attach-backend 'mirror))
               (not (locate-library (symbol-name herdr-core-attach-backend))))
      (user-error "herdr-core: %s is not installed" herdr-core-attach-backend))
    (let ((order (herdr-core--attach-order))
          (attached nil))
      (while (and order (not attached))
        (let ((backend (pop order)))
          (cond
           ((eq backend 'mirror)
            (herdr-core-terminal address)
            (setq attached t))
           ((not (locate-library (symbol-name backend)))
            (when (eq backend herdr-core-attach-backend)
              (user-error "herdr-core: %s is not installed" backend)))
           (t
            (condition-case err
                (progn
                  (herdr-core--attach-with backend address)
                  (setq attached t))
              (error
               (message "hc: %s failed: %s" backend
                        (error-message-string err))))))))
      (unless attached
        (herdr-core-terminal address)))))

(provide 'herdr-core-attach)

;;; herdr-core-attach.el ends here
