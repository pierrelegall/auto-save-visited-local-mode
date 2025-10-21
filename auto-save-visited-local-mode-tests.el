;;; auto-save-visited-local-mode-tests.el --- Tests for auto-save-visited-local-mode

;;; Commentary:
;; Test suite for auto-save-visited-local-mode using ERT.

;;; Code:

(require 'ert)
(require 'auto-save-visited-local-mode)

;;; Test Helpers

(defvar asvlm-test-dir nil
  "Temporary directory for test files.")

(defun asvlm-test-setup ()
  "Set up test environment."
  (setq asvlm-test-dir (make-temp-file "asvlm-test-" t))
  ;; Ensure clean state
  (setq auto-save-visited-local-interval 5)
  (setq auto-save-visited-local-silent nil)
  (setq auto-save-visited-local-predicate nil))

(defun asvlm-test-teardown ()
  "Clean up test environment."
  (when asvlm-test-dir
    (delete-directory asvlm-test-dir t)
    (setq asvlm-test-dir nil)))

(defun asvlm-test-create-file (name &optional content)
  "Create a test file NAME with optional CONTENT."
  (let ((file-path (expand-file-name name asvlm-test-dir)))
    (with-temp-file file-path
      (when content
        (insert content)))
    file-path))

(defun asvlm-test-with-file-buffer (file content body-fn)
  "Execute BODY-FN in a buffer visiting FILE with CONTENT."
  (let ((file-path (asvlm-test-create-file file content)))
    (with-current-buffer (find-file-noselect file-path)
      (unwind-protect
          (funcall body-fn)
        (when (buffer-live-p (current-buffer))
          (set-buffer-modified-p nil)
          (kill-buffer))))))

(defmacro asvlm-test-with-temp-buffer (file content &rest body)
  "Execute BODY in a buffer visiting FILE with CONTENT."
  (declare (indent 2))
  `(asvlm-test-with-file-buffer ,file ,content (lambda () ,@body)))

;;; Tests for Save Predicate

(ert-deftest auto-save-visited-local-mode/should-save-p-basic ()
  "Test basic conditions for auto-saving."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "initial content"
        ;; Should not save if buffer is not modified
        (should-not (auto-save-visited-local--should-save-p))

        ;; Should save if buffer is modified
        (insert "new content")
        (should (auto-save-visited-local--should-save-p)))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/should-save-p-no-file ()
  "Test that buffers not visiting files are not saved."
  (with-temp-buffer
    (setq buffer-file-name nil)
    (insert "some content")
    (set-buffer-modified-p t)
    (should-not (auto-save-visited-local--should-save-p))))

(ert-deftest auto-save-visited-local-mode/should-save-p-read-only ()
  "Test that read-only files are not saved."
  (asvlm-test-setup)
  (unwind-protect
      (let ((file-path (asvlm-test-create-file "readonly.txt" "content")))
        ;; Make file read-only
        (set-file-modes file-path #o444)
        (with-current-buffer (find-file-noselect file-path)
          (unwind-protect
              (progn
                (setq buffer-read-only nil) ; Allow modification in buffer
                (insert "new content")
                (should-not (auto-save-visited-local--should-save-p)))
            (set-buffer-modified-p nil)
            (kill-buffer))))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/should-save-p-custom-predicate ()
  "Test custom predicate filtering."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (insert "modified")

        ;; Predicate returns nil - should not save
        (setq-local auto-save-visited-local-predicate (lambda () nil))
        (should-not (auto-save-visited-local--should-save-p))

        ;; Predicate returns t - should save
        (setq-local auto-save-visited-local-predicate (lambda () t))
        (should (auto-save-visited-local--should-save-p)))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/should-save-p-indirect-buffer ()
  "Test that indirect buffers are not saved."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (let ((indirect-buf (make-indirect-buffer (current-buffer) "indirect" t)))
          (with-current-buffer indirect-buf
            (insert "modified")
            (should-not (auto-save-visited-local--should-save-p))
            (kill-buffer indirect-buf))))
    (asvlm-test-teardown)))

;;; Tests for Timer Management

(ert-deftest auto-save-visited-local-mode/timer-creation ()
  "Test that enabling mode creates a timer."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (should-not auto-save-visited-local--timer)
        (auto-save-visited-local-mode 1)
        (should auto-save-visited-local--timer)
        (should (timerp auto-save-visited-local--timer))
        (auto-save-visited-local-mode -1))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/timer-cleanup ()
  "Test that disabling mode cleans up timer."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (auto-save-visited-local-mode 1)
        (let ((timer auto-save-visited-local--timer))
          (should timer)
          (auto-save-visited-local-mode -1)
          (should-not auto-save-visited-local--timer)
          ;; Timer should be canceled
          (should-not (memq timer timer-idle-list))))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/timer-interval ()
  "Test that timer is created with custom interval."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (setq-local auto-save-visited-local-interval 10)
        (auto-save-visited-local-mode 1)
        ;; Timer should exist and be repeating
        (should auto-save-visited-local--timer)
        (should (timerp auto-save-visited-local--timer))
        (should (memq auto-save-visited-local--timer timer-idle-list))
        (auto-save-visited-local-mode -1))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/timer-restart-on-interval-change ()
  "Test that timer restarts when interval changes."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (setq-local auto-save-visited-local-interval 5)
        (auto-save-visited-local-mode 1)
        (let ((old-timer auto-save-visited-local--timer))
          (should old-timer)
          ;; Change interval
          (setq-local auto-save-visited-local-interval 15)
          ;; Timer should be different (new timer created)
          (should auto-save-visited-local--timer)
          (should-not (eq old-timer auto-save-visited-local--timer))
          ;; Old timer should be canceled
          (should-not (memq old-timer timer-idle-list)))
        (auto-save-visited-local-mode -1))
    (asvlm-test-teardown)))

;;; Tests for Mode Enable/Disable

(ert-deftest auto-save-visited-local-mode/mode-enable-with-file ()
  "Test enabling mode in buffer visiting a file."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (should-not auto-save-visited-local-mode)
        (auto-save-visited-local-mode 1)
        (should auto-save-visited-local-mode)
        (auto-save-visited-local-mode -1))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/mode-enable-without-file ()
  "Test that mode cannot be enabled in non-file buffer."
  (with-temp-buffer
    (should-error (auto-save-visited-local-mode 1)
                  :type 'user-error)
    (should-not auto-save-visited-local-mode)))

(ert-deftest auto-save-visited-local-mode/mode-turn-on-helper ()
  "Test the turn-on helper function."
  (asvlm-test-setup)
  (unwind-protect
      (progn
        ;; Should enable in file buffer
        (asvlm-test-with-temp-buffer "test.txt" "content"
          (auto-save-visited-local-mode-turn-on)
          (should auto-save-visited-local-mode)
          (auto-save-visited-local-mode -1))

        ;; Should not enable in non-file buffer
        (with-temp-buffer
          (auto-save-visited-local-mode-turn-on)
          (should-not auto-save-visited-local-mode)))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/mode-turn-on-indirect-buffer ()
  "Test that turn-on helper skips indirect buffers."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (let ((indirect-buf (make-indirect-buffer (current-buffer) "indirect" t)))
          (with-current-buffer indirect-buf
            (auto-save-visited-local-mode-turn-on)
            (should-not auto-save-visited-local-mode)
            (kill-buffer indirect-buf))))
    (asvlm-test-teardown)))

;;; Tests for Save Behavior

(ert-deftest auto-save-visited-local-mode/save-buffer-writes-file ()
  "Test that auto-save actually writes the file."
  (asvlm-test-setup)
  (unwind-protect
      (let* ((file-path (asvlm-test-create-file "test.txt" "initial"))
             (buf (find-file-noselect file-path)))
        (with-current-buffer buf
          (unwind-protect
              (progn
                (goto-char (point-max))
                (insert " modified")
                (should (buffer-modified-p))

                ;; Manually trigger save
                (auto-save-visited-local--save-buffer)

                ;; Buffer should no longer be modified
                (should-not (buffer-modified-p))

                ;; File should contain new content (with trailing newline from save)
                (with-temp-buffer
                  (insert-file-contents file-path)
                  (should (string= (buffer-string) "initial modified\n"))))
            (set-buffer-modified-p nil)
            (kill-buffer buf))))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/save-buffer-silent-mode ()
  "Test that silent mode suppresses messages."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (insert "modified")
        (setq-local auto-save-visited-local-silent t)

        ;; Capture messages
        (let ((messages nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (auto-save-visited-local--save-buffer)
            ;; Should not have any messages in silent mode
            (should-not messages))))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/save-buffer-non-silent-mode ()
  "Test that non-silent mode shows messages."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (insert "modified")
        (setq-local auto-save-visited-local-silent nil)

        ;; This test verifies the message is shown temporarily
        ;; We can't easily test temp messages, but we verify the call path
        (should (auto-save-visited-local--should-save-p))
        (auto-save-visited-local--save-buffer)
        (should-not (buffer-modified-p)))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/save-buffer-wrapper-dead-buffer ()
  "Test that wrapper handles dead buffers gracefully."
  (asvlm-test-setup)
  (unwind-protect
      (let* ((file-path (asvlm-test-create-file "test.txt" "content"))
             (buf (find-file-noselect file-path)))
        (kill-buffer buf)
        ;; Should not error
        (should-not (auto-save-visited-local--save-buffer-wrapper buf)))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/save-buffer-wrapper-mode-disabled ()
  "Test that wrapper does not save when mode is disabled."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (insert "modified")
        (auto-save-visited-local-mode -1)
        (let ((modified-before (buffer-modified-p)))
          (auto-save-visited-local--save-buffer-wrapper (current-buffer))
          ;; Should still be modified (no save occurred)
          (should (eq modified-before (buffer-modified-p)))))
    (asvlm-test-teardown)))

;;; Tests for Error Handling

(ert-deftest auto-save-visited-local-mode/save-error-handling ()
  "Test that errors during save are caught and reported."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (auto-save-visited-local-mode 1)
        (insert "modified")

        ;; Mock basic-save-buffer to throw error
        (cl-letf (((symbol-function 'basic-save-buffer)
                   (lambda () (error "Mock save error")))
                  (messages nil)
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))

          ;; Should catch error and show message
          (auto-save-visited-local--save-buffer-wrapper (current-buffer))
          (should messages)
          (should (cl-some (lambda (msg) (string-match-p "error" msg)) messages)))

        (auto-save-visited-local-mode -1))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/save-error-silent-mode ()
  "Test that errors in silent mode are suppressed."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        (setq-local auto-save-visited-local-silent t)
        (auto-save-visited-local-mode 1)
        (insert "modified")

        ;; Mock basic-save-buffer to throw error
        (cl-letf (((symbol-function 'basic-save-buffer)
                   (lambda () (error "Mock save error")))
                  (messages nil)
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))

          ;; Should catch error but not show message in silent mode
          (auto-save-visited-local--save-buffer-wrapper (current-buffer))
          (should-not messages))

        (auto-save-visited-local-mode -1))
    (asvlm-test-teardown)))

;;; Tests for Buffer Cleanup

(ert-deftest auto-save-visited-local-mode/kill-buffer-cleanup ()
  "Test that timer is cleaned up when buffer is killed."
  (asvlm-test-setup)
  (unwind-protect
      (let* ((file-path (asvlm-test-create-file "test.txt" "content"))
             (buf (find-file-noselect file-path)))
        (with-current-buffer buf
          (auto-save-visited-local-mode 1)
          (let ((timer auto-save-visited-local--timer))
            (should timer)
            (should (memq timer timer-idle-list))

            ;; Kill buffer
            (kill-buffer buf)

            ;; Timer should be removed from timer list
            (should-not (memq timer timer-idle-list)))))
    (asvlm-test-teardown)))

;;; Tests for Hook Management

(ert-deftest auto-save-visited-local-mode/hook-not-set-initially ()
  "Test that kill-buffer-hook is NOT set before mode is enabled."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        ;; Hook should not be present in a fresh buffer
        (should-not (memq #'auto-save-visited-local--stop-timer kill-buffer-hook))
        ;; kill-buffer-hook should not be buffer-local yet
        (should-not (local-variable-p 'kill-buffer-hook)))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/hook-added-on-enable ()
  "Test that kill-buffer-hook is added when mode is enabled."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        ;; Hook should not be present initially
        (should-not (memq #'auto-save-visited-local--stop-timer kill-buffer-hook))

        ;; Enable mode
        (auto-save-visited-local-mode 1)

        ;; Hook should now be present (buffer-local)
        (should (local-variable-p 'kill-buffer-hook))
        (should (memq #'auto-save-visited-local--stop-timer kill-buffer-hook))

        ;; Clean up
        (auto-save-visited-local-mode -1))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/hook-removed-on-disable ()
  "Test that kill-buffer-hook is removed when mode is disabled."
  (asvlm-test-setup)
  (unwind-protect
      (asvlm-test-with-temp-buffer "test.txt" "content"
        ;; Enable mode
        (auto-save-visited-local-mode 1)
        (should (memq #'auto-save-visited-local--stop-timer kill-buffer-hook))

        ;; Disable mode
        (auto-save-visited-local-mode -1)

        ;; Hook should be removed
        (should-not (memq #'auto-save-visited-local--stop-timer kill-buffer-hook)))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/hook-is-buffer-local ()
  "Test that the kill-buffer-hook is buffer-local, not global."
  (asvlm-test-setup)
  (unwind-protect
      (let* ((file1 (asvlm-test-create-file "test1.txt" "content1"))
             (file2 (asvlm-test-create-file "test2.txt" "content2"))
             (buf1 (find-file-noselect file1))
             (buf2 (find-file-noselect file2)))
        (unwind-protect
            (progn
              ;; Enable mode only in buf1
              (with-current-buffer buf1
                (auto-save-visited-local-mode 1)
                (should (memq #'auto-save-visited-local--stop-timer kill-buffer-hook)))

              ;; buf2 should NOT have the hook
              (with-current-buffer buf2
                (should-not (memq #'auto-save-visited-local--stop-timer kill-buffer-hook))))

          (with-current-buffer buf1
            (auto-save-visited-local-mode -1)
            (kill-buffer))
          (kill-buffer buf2)))
    (asvlm-test-teardown)))

;;; Tests for Configuration

(ert-deftest auto-save-visited-local-mode/buffer-local-interval ()
  "Test that interval can be set buffer-locally."
  (asvlm-test-setup)
  (unwind-protect
      (let* ((file1 (asvlm-test-create-file "test1.txt" "content1"))
             (file2 (asvlm-test-create-file "test2.txt" "content2"))
             (buf1 (find-file-noselect file1))
             (buf2 (find-file-noselect file2)))
        (unwind-protect
            (progn
              ;; Set different intervals
              (with-current-buffer buf1
                (setq-local auto-save-visited-local-interval 5)
                (auto-save-visited-local-mode 1))

              (with-current-buffer buf2
                (setq-local auto-save-visited-local-interval 20)
                (auto-save-visited-local-mode 1))

              ;; Verify each buffer has its own timer
              (with-current-buffer buf1
                (should auto-save-visited-local--timer)
                (should (timerp auto-save-visited-local--timer)))

              (with-current-buffer buf2
                (should auto-save-visited-local--timer)
                (should (timerp auto-save-visited-local--timer)))

              ;; Timers should be different objects
              (let ((timer1 (with-current-buffer buf1 auto-save-visited-local--timer))
                    (timer2 (with-current-buffer buf2 auto-save-visited-local--timer)))
                (should-not (eq timer1 timer2))))

          (with-current-buffer buf1
            (auto-save-visited-local-mode -1)
            (kill-buffer))
          (with-current-buffer buf2
            (auto-save-visited-local-mode -1)
            (kill-buffer))))
    (asvlm-test-teardown)))

(ert-deftest auto-save-visited-local-mode/buffer-local-silent ()
  "Test that silent setting can be buffer-local."
  (asvlm-test-setup)
  (unwind-protect
      (let* ((file1 (asvlm-test-create-file "test1.txt" "content1"))
             (file2 (asvlm-test-create-file "test2.txt" "content2"))
             (buf1 (find-file-noselect file1))
             (buf2 (find-file-noselect file2)))
        (unwind-protect
            (progn
              (with-current-buffer buf1
                (setq-local auto-save-visited-local-silent t)
                (should auto-save-visited-local-silent))

              (with-current-buffer buf2
                (setq-local auto-save-visited-local-silent nil)
                (should-not auto-save-visited-local-silent)))

          (kill-buffer buf1)
          (kill-buffer buf2)))
    (asvlm-test-teardown)))

(provide 'auto-save-visited-local-mode-tests)
;;; auto-save-visited-local-mode-tests.el ends here
