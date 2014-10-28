;;; psci.el --- Major mode for purescript repl psci

;; Copyright (C) 2014 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((purescript-mode "13.10") (dash "2.9.0") (projectile "0.11.0") (s "1.9.0") (f 0.17.1))
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

;; Provides a nice interface to evaluating Purescript expression.
;; Input is handled by the comint package, and output is passed
;; through the pretty-printer.

;; To start: M-x psci/run.  Type C-h m in the *Psci* buffer for more info.

;;
;; More informations: https://ardumont/emacs-psci
;; Issue tracker: https://github.com/ardumont/emacs-psci/issues

;;; Code:

(require 'comint)
(require 'dash)
(require 'purescript-mode)
(require 'projectile)
(require 's)

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
  (setq comint-eol-on-send t)
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

(defun psci/load-file! (filename)
  "Load the purescript FILENAME inside the current running session."
  (psci/run-psci-string! (format ":m %s" filename)))

;;;###autoload
(defun psci/load-current-file! ()
  "Load the current file in the session."
  (interactive)
  (psci/load-file! buffer-file-name))

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
  (-when-let (module-name (psci/compute-module-name!))
    (psci/run-psci-string! (format ":i %s" module-name))))

(defvar psci/project-module-file ".psci"
  "The default file referencing the purescript modules to load at psci startup.")

(defun psci/--file-content (filename)
  "Load the FILENAME's content as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun psci/--symbol (sym n)
  "Compute the repetition of a symbol SYM N times as a string."
  (--> n
    (-repeat it sym)
    (s-join "" it)))

(defun psci/--compute-relative-path (directory file-name)
  "Compute the relative path between the DIRECTORY and the FILE-NAME."
  (->> directory
    (f-relative file-name)
    f-split
    length
    1-
    (psci/--symbol "../")))

(defun psci/--project-module-files! ()
  "Compulse the list of modules for the current project."
  (let* ((parent-root-folder (projectile-project-root))
         (relative-path      (psci/--compute-relative-path (projectile-project-root) buffer-file-name)))
    (-when-let (psci-module-file (expand-file-name psci/project-module-file parent-root-folder))
      (when (file-exists-p psci-module-file)
        (->> psci-module-file
          psci/--file-content
          (s-split "\n")
          (--map (s-concat relative-path (cadr (s-split ":m " it))))
          (-filter 'file-exists-p)
          nreverse)))))

;;;###autoload
(defun psci/load-project-modules! ()
  "Load the modules needed for the repl session.
We chose to load the .psci file's content (the purescript doc proposes its use)."
  (interactive)
  (-when-let (modules (psci/--project-module-files!))
    (call-interactively 'psci/reset!)
    (mapc #'psci/load-file! modules)))

;;;###autoload
(defun psci/reset! ()
  "Reset the current status of the repl session."
  (interactive)
  (psci/run-psci-string! ":r"))

;; Add some default bindings
(add-hook 'purescript-mode-hook (lambda ()
                                  (define-key purescript-mode-map (kbd "C-c C-l") 'psci/load-current-file!)
                                  (define-key purescript-mode-map (kbd "C-c C-r") 'psci/load-project-modules!)
                                  (define-key purescript-mode-map (kbd "C-c M-n") 'psci/load-module!)))

(provide 'psci)
;;; psci.el ends here
