;;; herdr-core-context.el --- Context staging for herdr-core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The herdr-core.el authors

;; Author: herdr-core.el contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools, terminals
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; herdr-core-context.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; herdr-core-context.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; herdr-core-context.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Staged context for herdr-core: buffers, files and regions rendered as
;; Markdown and prepended to a message sent to an agent.  This library never
;; requires `herdr-core'; the core wires it in through
;; `herdr-core-context-path-resolver' and `herdr-core-context-open-function'.

;;; Code:

(require 'tabulated-list)
(require 'project nil t)

(defgroup herdr-core nil
  "Emacs client for herdr-core."
  :group 'tools
  :prefix "herdr-core-")

(defvar herdr-core-context-max-chars)
(defvar herdr-core-context-entries nil
  "Context entries staged for the messages that follow.
Each entry is a plist with :type (buffer, file or region), :label, :text,
:path, :mode, :range and :remote-path.")

(defvar herdr-core-context-changed-hook nil
  "Hook run after the staged context changes.")

(defvar herdr-core-context-path-resolver nil
  "Function mapping a local file to a path on the agent machine.
It is called with one path and returns the remote path, or nil when the file
must be inlined as text.  The core sets this to a non-prompting resolver.")

(defvar herdr-core-context-open-function nil
  "Function used to visit an entry that only carries a remote path.
Called with the remote path and a terminal address.  The core sets this to
`herdr-core-open-file'.")

(defvar herdr-core-context-list-mode)

;;; Rendering

(defun herdr-core--context-limit ()
  "Return the context size limit."
  (if (boundp 'herdr-core-context-max-chars)
      herdr-core-context-max-chars
    120000))

(defun herdr-core--context-lang (mode)
  "Return the fenced-code language token for MODE."
  (let ((name (if mode (symbol-name mode) "text")))
    (setq name (replace-regexp-in-string "-mode\\'" "" name))
    (if (equal name "fundamental") "text" name)))

(defun herdr-core--context-strip-label (label)
  "Return LABEL without a leading \"buffer: \"."
  (replace-regexp-in-string "\\`buffer: " "" (or label "")))

(defun herdr-core--context-render-one (entry)
  "Render one context ENTRY as Markdown, or nil when it is empty."
  (let ((type (plist-get entry :type))
        (remote (plist-get entry :remote-path))
        (path (plist-get entry :path))
        (label (plist-get entry :label))
        (text (plist-get entry :text))
        (mode (plist-get entry :mode))
        (range (plist-get entry :range)))
    (cond
     ((and remote (memq type '(file buffer)))
      (format "#### file (on the agent's machine): %s\nRead it there.\n" remote))
     ((eq type 'region)
      (format "#### %s\n```%s\n%s\n```\n"
              (or label
                  (format "buffer: unknown (lines %s-%s)"
                          (if range (number-to-string (car range)) "?")
                          (if range (number-to-string (cdr range)) "?")))
              (herdr-core--context-lang mode)
              (or text "")))
     (t
      (let ((where (or path (herdr-core--context-strip-label label))))
        (format "#### file: %s (%s)\n```%s\n%s\n```\n"
                where
                (if mode (symbol-name mode) "text")
                (herdr-core--context-lang mode)
                (or text "")))))))

(defun herdr-core-context-render (entries)
  "Render ENTRIES as Markdown, capped at `herdr-core-context-max-chars'."
  (let ((limit (herdr-core--context-limit))
        (out "")
        (truncated nil))
    (catch 'done
      (dolist (entry entries)
        (let ((piece (herdr-core--context-render-one entry)))
          (when piece
            (let ((room (- limit (length out))))
              (cond
               ((<= (length piece) room)
                (setq out (concat out piece)))
               ((> room 0)
                (setq out (concat out (substring piece 0 room)
                                  "\n... [context truncated]\n")
                      truncated t)
                (throw 'done nil))
               (t
                (setq out (concat out "\n... [context truncated]\n")
                      truncated t)
                (throw 'done nil))))))))
    (when truncated
      (message "hc: context truncated at %d chars" limit))
    out))

;;; Entries

(defun herdr-core--context-notify-changed ()
  "Tell the rest of the package that the staged context changed."
  (run-hooks 'herdr-core-context-changed-hook))

(defun herdr-core--context-resolve (path)
  "Return the agent-side path for PATH, or nil when it must be inlined."
  (when (and path herdr-core-context-path-resolver)
    (condition-case err
        (funcall herdr-core-context-path-resolver path)
      (error
       (message "hc: cannot map %s: %s" path (error-message-string err))
       nil))))

(defun herdr-core-context-add-buffer (&optional buffer)
  "Stage the whole of BUFFER (default: the current buffer) as context.
With a prefix argument, read the buffer name first."
  (interactive
   (list (when current-prefix-arg (read-buffer "Buffer: " (current-buffer) t))))
  (let* ((buf (or buffer (current-buffer)))
         (name (buffer-name buf))
         (file (buffer-file-name buf))
         (remote (herdr-core--context-resolve file))
         (entry nil))
    (if remote
        (setq entry (list :type 'buffer :label (concat "buffer: " name)
                          :buffer name
                          :path file :remote-path remote))
      (with-current-buffer buf
        (setq entry (list :type 'buffer :label (concat "buffer: " name)
                          :buffer name
                          :path file :mode major-mode
                          :text (buffer-substring-no-properties (point-min) (point-max))))))
    (push entry herdr-core-context-entries)
    (herdr-core--context-notify-changed)
    (message "hc: staged %s" (plist-get entry :label))
    entry))

(defun herdr-core-context-add-file (file)
  "Stage FILE as context, inlined or as a path on the agent machine.
The file is inlined as text unless its path maps to the agent machine, in
which case only the path is sent."
  (interactive "fFile: ")
  (let* ((path (expand-file-name file))
         (remote (herdr-core--context-resolve path))
         (label (concat "file: " (abbreviate-file-name path)))
         (entry nil))
    (if remote
        (setq entry (list :type 'file :label label :path path :remote-path remote))
      (let ((text (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))))
        (when (string-match-p "\0" text)
          (user-error "Binary file; use herdr-core-put-file"))
        (let ((mode (with-temp-buffer
                      (insert-file-contents path)
                      (let ((buffer-file-name path))
                        (set-auto-mode))
                      major-mode)))
          (setq entry (list :type 'file :label label :path path
                            :mode mode :text text)))))
    (push entry herdr-core-context-entries)
    (herdr-core--context-notify-changed)
    (message "hc: staged %s" label)
    entry))

(defun herdr-core--context-region-label (start end)
  "Return the label for the region between START and END."
  (let ((name (or (buffer-file-name) (buffer-name)))
        (first (line-number-at-pos start))
        (last (line-number-at-pos (if (> end start) (1- end) end))))
    (format "%s (lines %d-%d)" name first last)))

(defun herdr-core-context-add-region (start end)
  "Stage the region between START and END as context."
  (interactive "r")
  (let* ((path (buffer-file-name))
         (label (herdr-core--context-region-label start end))
         (entry (list :type 'region
                      :label (concat (if path "file: " "buffer: ") label)
                      :buffer (buffer-name)
                      :path path
                      :mode major-mode
                      :range (cons (line-number-at-pos start)
                                   (line-number-at-pos (if (> end start) (1- end) end)))
                      :text (buffer-substring-no-properties start end))))
    (push entry herdr-core-context-entries)
    (herdr-core--context-notify-changed)
    (message "hc: staged %s" (plist-get entry :label))
    entry))

(defun herdr-core--context-project-root ()
  "Return the current project root, or `default-directory'."
  (let ((p (and (fboundp 'project-current)
                (project-current nil (expand-file-name default-directory)))))
    (or (and p (fboundp 'project-root) (project-root p))
        (and p (cdr p))
        (expand-file-name default-directory))))

(defun herdr-core-context-add-project-file ()
  "Stage a file from the current project as context."
  (interactive)
  (let ((file (read-file-name "Project file: " (herdr-core--context-project-root))))
    (herdr-core-context-add-file file)))

(defun herdr-core-context-add ()
  "Stage context to send with messages.
Prompts for what to stage: a buffer, a file, a region or a project file.
Staged context is prepended to every message until it is cleared."
  (interactive)
  (let ((kind (completing-read "Add context: "
                               '("buffer" "file" "region" "project file")
                               nil t)))
    (cond
     ((equal kind "buffer") (herdr-core-context-add-buffer))
     ((equal kind "file") (call-interactively #'herdr-core-context-add-file))
     ((equal kind "region") (call-interactively #'herdr-core-context-add-region))
     ((equal kind "project file") (herdr-core-context-add-project-file)))))

(defun herdr-core-context-remove (entry)
  "Remove ENTRY from the staged context.
Interactively, removes the entry at point in the context list."
  (interactive
   (let ((id (tabulated-list-get-id)))
     (unless id (user-error "No context entry at point"))
     (list (nth id herdr-core-context-entries))))
  (setq herdr-core-context-entries (delete entry herdr-core-context-entries))
  (herdr-core--context-notify-changed)
  (message "hc: context entry removed"))

(defun herdr-core-context-clear ()
  "Remove every staged context entry."
  (interactive)
  (setq herdr-core-context-entries nil)
  (herdr-core--context-notify-changed)
  (message "hc: context cleared"))

;;; The context list buffer

(defvar herdr-core-context-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "d" #'herdr-core-context-remove)
    (define-key map "k" #'herdr-core-context-remove)
    (define-key map (kbd "RET") #'herdr-core-context-open)
    (define-key map "g" #'herdr-core-context-list-revert)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `herdr-core-context-list-mode'.")

(defun herdr-core--context-list-entries ()
  "Return the tabulated list rows for the staged context."
  (let ((index 0)
        (rows nil))
    (dolist (entry herdr-core-context-entries)
      (push (list index
                  (vector (symbol-name (or (plist-get entry :type) 'buffer))
                          (or (plist-get entry :label) "")
                          (number-to-string (length (or (plist-get entry :text) "")))
                          (if (plist-get entry :remote-path) "yes" "")))
            rows)
      (setq index (1+ index)))
    (nreverse rows)))

(define-derived-mode herdr-core-context-list-mode tabulated-list-mode "hc-context"
  "Major mode for the staged herdr-core context.

Each row is one entry: its type, label, size and whether only a path on the
agent machine is sent.  RET visits the source of the entry.

\\{herdr-core-context-list-mode-map}"
  (setq tabulated-list-format [("Type" 10 nil) ("Label" 60 nil)
                               ("Size" 10 nil) ("Remote" 8 nil)])
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-entries #'herdr-core--context-list-entries)
  (tabulated-list-init-header))

(defun herdr-core-context-list-revert ()
  "Redraw the context list from the staged entries."
  (interactive)
  (tabulated-list-print t))

(defun herdr-core-context-open ()
  "Visit the source of the context entry at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (entry (and id (nth id herdr-core-context-entries))))
    (unless entry (user-error "No context entry at point"))
    (cond
     ((and (plist-get entry :remote-path) herdr-core-context-open-function)
      (funcall herdr-core-context-open-function (plist-get entry :remote-path)))
     ((plist-get entry :path)
      (find-file (plist-get entry :path)))
     ((plist-get entry :buffer)
      (switch-to-buffer (plist-get entry :buffer)))
     (t (user-error "Nothing to visit")))))

(defun herdr-core-context-show ()
  "Show the staged context in a list buffer.
The buffer is displayed without selecting it; see
`herdr-core-context-list-mode' for its keys."
  (interactive)
  (let ((buf (get-buffer-create "*hc context*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'herdr-core-context-list-mode)
        (herdr-core-context-list-mode))
      (setq tabulated-list-entries #'herdr-core--context-list-entries)
      (tabulated-list-print t))
    (display-buffer buf)
    buf))

(provide 'herdr-core-context)

;;; herdr-core-context.el ends here
