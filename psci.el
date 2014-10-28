;;; psci.el --- Major mode for purescript repl psci

;; Copyright (C) 2014 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((purescript-mode "13.10"))
;; Keywords: purescript psci repl major mode
;; URL: https://github.com/ardumont/emacs-psci

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Major mode for purescript repl psci
;;
;; More informations: https://ardumont/emacs-psci
;; Issue tracker: https://github.com/ardumont/emacs-psci/issues

;;; Code:

(require 'comint)
(require 'purescript-mode)

(defvar psci/buffer-name "Psci"
  "Buffer name of the psci buffer.")

(defun psci/process-name (buffer-name)
  "Compute the buffer's process name based on BUFFER-NAME."
  (format "*%s*" buffer-name))

(defvar psci/file-path "psci"
  "Path to the program used by `psci/run' function.")

(defvar psci/arguments '()
  "Commandline arguments to pass to `psci/run' function.")

(defvar psci-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `psci'.")

(defvar psci/prompt-regexp "^>+ *"
  "Prompt for `psci'.")

(defun psci/run ()
  "Run an inferior instance of `psci' inside Emacs."
  (interactive)
  (let* ((psci-program psci/file-path)
         (buffer (comint-check-proc psci/buffer-name)))
    ;; pop to the "*Psci*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'psci-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer (psci/process-name psci/buffer-name)))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer psci/buffer-name buffer
             psci-program psci/arguments)
      (psci-mode))))

(defun psci/--initialize ()
  "Helper function to initialize psci."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

;;;###autoload
(define-derived-mode psci-mode comint-mode "Psci"
  "Major mode for `run-psci'.

\\<psci-mode-map>"
  nil "Psci"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp psci/prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(purescript-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) psci/prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'psci-mode-hook 'psci/--initialize)

(defun psci/run-psci-string! (command)
  "Run purescript code COMMAND as string."
  (let ((process (get-buffer-process (psci/process-name psci/buffer-name))))
    (comint-send-string process (format "%s\n" command))
    (process-send-eof process)))

(defun psci/run-psci-region! (region-start region-end)
  "Run purescript code between REGION-START and REGION-END."
  (let ((process (get-buffer-process (psci/process-name psci/buffer-name))))
    (comint-send-region process region-start region-end)
    (process-send-eof process)))

;;;###autoload
(defun psci/load-file! ()
  "Load the current file in the session."
  (interactive)
  (psci/run-psci-string! (format ":m %s" buffer-file-name)))

(defun psci/compute-module-name! ()
  "Compute the current file's module name."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp "^module \\\([a-zA-Z0-9\\\.]+\\\) "))
      (search-forward-regexp regexp)
      (match-string 1))))

;;;###autoload
(defun psci/load-module! ()
  "Load the module inside the repl session."
  (interactive)
  (let ((module-name (psci/compute-module-name!)))
    (psci/run-psci-string! (format ":i %s" module-name))))
(provide 'psci)
;;; psci.el ends here
