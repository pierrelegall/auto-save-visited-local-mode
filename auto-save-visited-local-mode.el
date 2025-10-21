;;; auto-save-visited-local-mode.el --- Buffer-local auto-save for visited files -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pierre <pierre@legall.im>

;; Author: Pierre Le Gall <pierre@legall.im>
;; Maintainer: Pierre <pierre@legall.im>
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience files autosave
;; URL: https://github.com/pierrelegall/auto-save-visited-local-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; A buffer-local alternative to auto-save-visited-mode that saves
;; the current buffer periodically without affecting other buffers.
;; Feature-aligned with the global auto-save-visited-mode.

;;; Code:

(defvar auto-save-visited-local-mode)

(defgroup auto-save-visited-local nil
  "Buffer-local automatic saving of visited files."
  :group 'auto-save)

(defcustom auto-save-visited-local-interval 5
  "Number of seconds of idle time before auto-saving the buffer.
This is the buffer-local equivalent of `auto-save-visited-interval'.
Used by `auto-save-visited-local-mode'."
  :type 'number
  :group 'auto-save-visited-local)

(defcustom auto-save-visited-local-silent nil
  "If non-nil, do not display auto-save messages.
This is the buffer-local equivalent of `auto-save-visited-silent'."
  :type 'boolean
  :group 'auto-save-visited-local)

(defcustom auto-save-visited-local-predicate nil
  "Predicate to determine whether to auto-save a buffer.
If non-nil, this should be a function that takes no arguments and
returns non-nil if the current buffer should be auto-saved.
This is the buffer-local equivalent of `auto-save-visited-predicate'.
If nil, all file-visiting buffers will be auto-saved (subject to
other conditions)."
  :type '(choice (const :tag "Auto-save all buffers" nil)
                 (function :tag "Custom predicate"))
  :group 'auto-save-visited-local)

(defvar-local auto-save-visited-local--timer nil
  "Timer for buffer-local auto-saving.
This is buffer-local so each buffer can have its own timer.")

(defun auto-save-visited-local--should-save-p ()
  "Return non-nil if current buffer should be auto-saved.
Mimics the logic from `auto-save-visited-mode'."
  (and (buffer-live-p (current-buffer))
       buffer-file-name                    ; Must be visiting a file
       (buffer-modified-p)                 ; Must be modified
       (not (buffer-base-buffer))          ; Not an indirect buffer
       (file-writable-p buffer-file-name)  ; File must be writable
       (not (file-remote-p buffer-file-name)) ; Skip remote files by default
       ;; Apply custom predicate if set
       (or (null auto-save-visited-local-predicate)
           (funcall auto-save-visited-local-predicate))))

(defun auto-save-visited-local--save-buffer ()
  "Save the current buffer if appropriate.
Aligned with `auto-save-visited-mode' behavior."
  (when (auto-save-visited-local--should-save-p)
    (cond
     ;; Silent mode: suppress all messages
     (auto-save-visited-local-silent
      (let ((inhibit-message t)
            (message-log-max nil))
        (basic-save-buffer)))
     ;; Non-silent mode: show temporary message like auto-save-visited-mode
     ('else
      (let ((message-log-max nil))
        (with-temp-message (format "Auto-saving %s..." (buffer-name))
          (basic-save-buffer)))))))

(defun auto-save-visited-local--start-timer ()
  "Start the auto-save timer for the current buffer."
  (auto-save-visited-local--stop-timer) ; Cancel existing timer if any
  (setq auto-save-visited-local--timer
        (run-with-idle-timer auto-save-visited-local-interval
                             t ; Repeat
                             #'auto-save-visited-local--save-buffer-wrapper
                             (current-buffer))))

(defun auto-save-visited-local--save-buffer-wrapper (buffer)
  "Wrapper to save BUFFER if it still exists and the mode is active."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when auto-save-visited-local-mode
        (condition-case err
            (auto-save-visited-local--save-buffer)
          (error
           (when (not auto-save-visited-local-silent)
             (message "Auto-save-visited-local error: %s" (error-message-string err)))))))))

(defun auto-save-visited-local--stop-timer ()
  "Stop the auto-save timer for the current buffer."
  (when auto-save-visited-local--timer
    (cancel-timer auto-save-visited-local--timer)
    (setq auto-save-visited-local--timer nil)))

(defun auto-save-visited-local--update-timer ()
  "Update the timer when the interval change.
Restart the timer with the new interval if mode is active."
  (when auto-save-visited-local-mode
    (auto-save-visited-local--start-timer)))

;;;###autoload
(define-minor-mode auto-save-visited-local-mode
  "Toggle automatic saving of the current buffer after idle time.

Unlike `auto-save-visited-mode', this is a buffer-local minor mode
that only affects the current buffer.  After `auto-save-visited-local-interval'
seconds of idle time, the buffer will be saved if it has been modified.

This mode only works for buffers visiting files.  Remote files are
skipped by default for performance reasons.

The behavior is aligned with `auto-save-visited-mode':
- Respects `auto-save-visited-local-silent' for message suppression
- Respects `auto-save-visited-local-predicate' for custom filtering
- Only saves writable, non-remote files
- Skips indirect buffers
- Uses idle timers to avoid interrupting work

See also `auto-save-visited-mode' for the global equivalent."
  :lighter " AutoSave"
  :group 'auto-save-visited-local
  (cond
   (auto-save-visited-local-mode
    (when (not (buffer-file-name))
      (setq auto-save-visited-local-mode nil)
      (user-error "Buffer must be visiting a file to use auto-save-visited-local"))
    (auto-save-visited-local--start-timer)
    (when (not auto-save-visited-local-silent)
      (message "Auto-Save-Visited-Local mode enabled for %s (interval: %ds)"
               (buffer-name) auto-save-visited-local-interval))
    (add-hook 'kill-buffer-hook #'auto-save-visited-local--stop-timer nil t))
   ('else
    (auto-save-visited-local--stop-timer)
    (when (not auto-save-visited-local-silent)
      (message "Auto-Save-Visited-Local mode disabled for %s" (buffer-name)))
    (remove-hook 'kill-buffer-hook #'auto-save-visited-local--stop-timer t))))

;; Watch for interval changes (buffer-local)
(defun auto-save-visited-local--interval-watcher (_symbol _newval operation where)
  "Watch for change to `auto-save-visited-local-interval'.
OPERATION and WHERE specify the variable change context.
Update the timer if the mode is active in the current buffer."
  (when (and (eq operation 'set)
             (eq where (current-buffer))
             (bound-and-true-p auto-save-visited-local-mode))
    (auto-save-visited-local--update-timer)))

(add-variable-watcher 'auto-save-visited-local-interval
                      #'auto-save-visited-local--interval-watcher)

;;;###autoload
(defun auto-save-visited-local-mode-turn-on ()
  "Turn on `auto-save-visited-local-mode' if appropriate.
Helper function for use in hooks or directory-local settings."
  (when (and (buffer-file-name)
             (not (buffer-base-buffer)))
    (auto-save-visited-local-mode 1)))

(provide 'auto-save-visited-local-mode)

;;; auto-save-visited-local-mode.el ends here
