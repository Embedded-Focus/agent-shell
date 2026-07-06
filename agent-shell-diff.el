;;; agent-shell-diff.el --- A quick way to query/display a diff. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'diff)
(require 'diff-mode)

(defvar-local agent-shell-diff--on-exit nil
  "Function to call when the diff buffer is killed.

This variable is automatically set by :on-exit from `agent-shell-diff'
and can be temporarily let-bound to nil to prevent the
on-exit callback from running when the buffer is killed.")

(defvar-local agent-shell-diff--file nil
  "Buffer-local file path associated with the diff.")

(defvar-local agent-shell-diff--accept-all-command nil
  "Buffer-local command to accept all changes in the diff.")

(defvar-local agent-shell-diff--reject-all-command nil
  "Buffer-local command to reject all changes in the diff.")

(defvar agent-shell-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'diff-hunk-next)
    (define-key map (kbd "p") #'diff-hunk-prev)
    (define-key map (kbd "y") #'agent-shell-diff-accept-all)
    (define-key map (kbd "C-c C-c") #'agent-shell-diff-reject-all)
    (define-key map (kbd "f") #'agent-shell-diff-open-file)
    (define-key map (kbd "q") #'kill-current-buffer)
    map)
  "Keymap for `agent-shell-diff-mode'.")

(define-derived-mode agent-shell-diff-mode diff-mode "Agent-Shell-Diff"
  "Major mode for `agent-shell' diff buffers.
Derives from `diff-mode'.  Provides `agent-shell-diff-accept-all'
and `agent-shell-diff-reject-all' commands that can be rebound
via `agent-shell-diff-mode-map'."
  :group 'agent-shell
  ;; Don't inherit diff-mode-map (some bindings can be destructive).
  (set-keymap-parent agent-shell-diff-mode-map nil)
  (setq buffer-read-only t))

(defun agent-shell-diff-kill-buffer (buffer)
  "Kill diff BUFFER, suppressing any `agent-shell-diff--on-exit' callback.
If BUFFER is not live, do nothing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq agent-shell-diff--on-exit nil))
    (kill-buffer buffer)))

(defun agent-shell-diff-accept-all ()
  "Accept all changes in the current diff buffer."
  (interactive)
  (if agent-shell-diff--accept-all-command
      (let ((buf (current-buffer)))
        (funcall agent-shell-diff--accept-all-command)
        (when (buffer-live-p buf)
          (let ((agent-shell-diff--on-exit nil))
            (kill-buffer buf))))
    (user-error "No accept command available in this buffer")))

(defun agent-shell-diff-reject-all ()
  "Reject all changes in the current diff buffer."
  (interactive)
  (if agent-shell-diff--reject-all-command
      (let ((buf (current-buffer)))
        (when (funcall agent-shell-diff--reject-all-command)
          (when (buffer-live-p buf)
            (let ((agent-shell-diff--on-exit nil))
              (kill-buffer buf)))))
    (user-error "No reject command available in this buffer")))

(cl-defun agent-shell-diff (&key diffs on-exit on-accept on-reject title)
  "Display one or more diffs in a buffer.

Creates a new buffer showing the differences using
`agent-shell-diff-mode'.  The buffer is read-only.

DIFFS is a list of alists, each with :old, :new and :file keys, as
returned by `agent-shell--make-diff-infos'.  A single diff is passed as
a one-element list.  When DIFFS holds more than one file, each is shown
as its own section preceded by a header naming the file.

When the buffer is killed, calls ON-EXIT with no arguments.

Returns the newly created diff buffer.

Arguments:
  :DIFFS     - List of ((:old . _) (:new . _) (:file . _)) alists
  :ON-EXIT   - Function called with no arguments when buffer is killed
  :ON-ACCEPT - Command to accept all changes
  :ON-REJECT - Command to reject all changes
  :TITLE     - Optional title to display in header line"
  (let* ((first-file (map-elt (car diffs) :file))
         (title (or title
                    (when (and first-file (not (cdr diffs)))
                      (file-name-nondirectory first-file))))
         (diff-buffer (generate-new-buffer "*agent-shell-diff*"))
         (calling-window (selected-window))
         (calling-buffer (current-buffer))
         (interrupt-key (where-is-internal 'agent-shell-interrupt
                                           (current-local-map) t)))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (let ((inhibit-read-only t)
                  (diff-mode-read-only nil))
              (erase-buffer)
              ;; Set mode before inserting diff so diff-no-select
              ;; doesn't reset font-lock (see #316).
              (agent-shell-diff-mode)
              (agent-shell-diff--insert-diffs diffs diff-buffer)
              ;; Add overlays to hide scary text.
              (save-excursion
                (goto-char (point-min))
                ;; Hide --- and +++ lines
                (while (re-search-forward "^\\(---\\|\\+\\+\\+\\).*\n" nil t)
                  (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
                    (overlay-put overlay 'category 'diff-header)
                    (overlay-put overlay 'display "")
                    (overlay-put overlay 'evaporate t)))
                ;; Replace @@ lines with "Changes"
                (goto-char (point-min))
                (while (re-search-forward "^@@.*@@.*\n" nil t)
                  (let ((overlay (make-overlay (match-beginning 0) (match-end 0)))
                        (face 'diff-hunk-header))  ; or any face you prefer
                    (overlay-put overlay 'category 'diff-header)
                    ;; Intended display is:
                    ;; ╭─────────╮
                    ;; │ changes │
                    ;; ╰─────────╯
                    ;; Using before-string so diff-hunk-next
                    ;; lands on "│" instead of "╭".
                    (overlay-put overlay 'before-string
                                 (propertize "\n╭─────────╮\n" 'face face))
                    (overlay-put overlay 'display
                                 (propertize "│ changes │\n╰─────────╯\n\n" 'face face))
                    (overlay-put overlay 'evaporate t)))))
            (goto-char (point-min))
            (ignore-errors (diff-hunk-next))
            (setq agent-shell-diff--file first-file
                  agent-shell-diff--accept-all-command on-accept
                  agent-shell-diff--reject-all-command on-reject)
            (when on-exit
              (setq agent-shell-diff--on-exit on-exit)
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (when (and agent-shell-diff--on-exit
                                     (buffer-live-p calling-buffer))
                            (with-current-buffer calling-buffer
                              (funcall on-exit)))
                          ;; Give focus back to calling buffer.
                          (when (buffer-live-p calling-buffer)
                            (ignore-errors
                              (when (window-live-p calling-window)
                                (unless (eq (window-buffer calling-window) calling-buffer)
                                  (set-window-buffer calling-window calling-buffer))
                                (select-window calling-window)))))
                        nil t))
            (let ((map (copy-keymap agent-shell-diff-mode-map)))
              (when (and interrupt-key
                         (not (lookup-key map interrupt-key)))
                (define-key map interrupt-key #'agent-shell-diff-reject-all))
              (use-local-map map))
            (setq header-line-format
                  (substitute-command-keys
                   (concat
                    "  "
                    (when title
                      (concat (propertize title 'face 'mode-line-emphasis) " "))
                    "\\[diff-hunk-next] next hunk  "
                    "\\[diff-hunk-prev] previous hunk  "
                    "\\[agent-shell-diff-accept-all] accept  "
                    "\\[agent-shell-diff-reject-all] reject  "
                    "\\[agent-shell-diff-open-file] open  "
                    "\\[kill-current-buffer] quit"))))
          diff-buffer)
      (pop-to-buffer diff-buffer '((display-buffer-use-some-window
                                    display-buffer-same-window))))))

(defun agent-shell-diff-open-file ()
  "Open the file associated with the diff section under point.

Falls back to the buffer's first file when point is not within a
tagged section."
  (interactive)
  (if-let* ((file (or (get-text-property (point) 'agent-shell-diff-file)
                      agent-shell-diff--file)))
      (find-file file)
    (user-error "No file associated with this diff buffer")))

(defun agent-shell-diff--insert-diffs (diffs buf)
  "Insert DIFFS into buffer BUF, one file section each.

DIFFS is a list of alists with :old, :new and :file keys.  When DIFFS
holds more than one entry, each section is preceded by a header naming
the file.  Each section is tagged with an `agent-shell-diff-file' text
property so `agent-shell-diff-open-file' can open the file under point."
  (let ((multiple (cdr diffs)))
    (with-current-buffer buf
      (dolist (diff diffs)
        (unless (bobp)
          (insert "\n"))
        (let ((section-start (point))
              (file (map-elt diff :file)))
          (when multiple
            (insert (propertize (concat (or file "changes") "\n")
                                'face 'diff-file-header)))
          (insert (agent-shell-diff--diff-section-string
                   (or (map-elt diff :old) "")
                   (or (map-elt diff :new) "")
                   file))
          (when file
            (put-text-property section-start (point)
                               'agent-shell-diff-file file)))))))

(defun agent-shell-diff--diff-section-string (old new file)
  "Return a cleaned diff between OLD and NEW for FILE.

FILE is only used to derive a temp-file suffix so `diff' picks a
sensible mode; it may be nil.  The leading command line and trailing
\"Diff finished.\" line that `diff-no-select' adds are removed."
  (let* ((extension (and file (file-name-extension file)))
         (suffix (and extension (format ".%s" extension)))
         (old-file (make-temp-file "old" nil suffix))
         (new-file (make-temp-file "new" nil suffix)))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old))
          (with-temp-file new-file (insert new))
          (with-temp-buffer
            (diff-no-select old-file new-file "-U3" t (current-buffer))
            (let ((inhibit-read-only t))
              ;; Remove command added by diff-no-select
              (goto-char (point-min))
              (delete-region (point) (progn (forward-line 1) (point)))
              ;; Remove "Diff finished." added by diff-no-select
              (delete-region (progn (goto-char (point-max))
                                    (forward-line -1)
                                    (forward-line 0)
                                    (point))
                             (point-max)))
            (buffer-string)))
      (delete-file old-file)
      (delete-file new-file))))

(provide 'agent-shell-diff)

;;; agent-shell-diff.el ends here
