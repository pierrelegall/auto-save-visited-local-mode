;;; auto-save-visited-local-mode.el --- Buffer-local auto-save for visited files -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pierre <your-email@example.com>

;; Author: Pierre Le Gall <pierre@legall.im>
;; Maintainer: Pierre <pierre@legall.im>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience files autosave
;; URL: https://github.com/pierrelegall/auto-save-visited-local-mode

;;; Commentary:
;; A buffer-local alternative to auto-save-visited-mode that saves
;; the current buffer periodically without affecting other buffers.
;; Feature-aligned with the global auto-save-visited-mode.

;;; Code:

(defvar auto-save-visited-local-mode)

(provide 'auto-save-visited-local-mode)

;;; auto-save-visited-local-mode.el ends here
