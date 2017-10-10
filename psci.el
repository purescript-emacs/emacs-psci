;;; psci.el --- Major mode for purescript repl psci

;; Copyright (C) 2014 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.0.6
;; Package-Requires: ((purescript-mode "13.10") (dash "2.9.0") (s "1.9.0") (f "0.17.1"))
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

;; Provides a simple interface to evaluate Purescript expression.
;; Input is handled by the comint package.

;; To start psci repl:
;; M-x psci.  Type C-h m in the *psci* buffer for more info.

;; To activate some basic bindings, you can add the following hook
;; to purescript-mode:
;; (add-hook 'purescript-mode-hook 'inferior-psci-mode)

;; To come back and forth between a purescript-mode buffer and
;; repl, you could use repl-toggle (available on melpa):
;; (require 'repl-toggle)
;; (add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci))

;; More informations: https://ardumont/emacs-psci
;; Issue tracker: https://github.com/ardumont/emacs-psci/issues

;;; Code:

(require 'comint)
(require 'dash)
(require 'purescript-mode)
(require 's)
(require 'f)

;; constants or variables

(defvar psci/buffer-name "psci"
  "Buffer name of the psci buffer.")

(defcustom psci/purs-path "purs"
  "Path to the `purs' binary"
  :group 'psci
  :type 'string)

(defcustom psci/arguments '("src/**/*.purs" "bower_components/purescript-*/src/**/*.purs")
  "Commandline arguments to pass to `psci' function."
  :group 'psci
  :type '(repeat string))

(defvar psci/prompt "> "
  "The psci prompt.")

;; private functions

(defun psci/log (msg)
  "Log MSG for psci."
  (message (format "psci - %s" msg)))

(defun psci/--project-root! ()
  "Determine the project's root folder.
Beware, can return nil if no .psci file is found."
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (file-name-directory (buffer-file-name))))

(defun psci/--process-name (buffer-name)
  "Compute the buffer's process name based on BUFFER-NAME."
  (format "*%s*" buffer-name))

(defun psci/--file-content (filename)
  "Load the FILENAME's content as a string.
When FILENAME is nil or not a real file, returns nil."
  (when (and filename (file-exists-p filename))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun psci/--run-psci-command! (command)
  "Run psci COMMAND as string."
  (-when-let (process (get-buffer-process (psci/--process-name psci/buffer-name)))
    (comint-simple-send process command)))

(defun psci/--compute-module-name! ()
  "Compute the current file's module name."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp "^module\\s-+\\\([a-zA-Z0-9\\\.]+\\\)\\b"))
      (search-forward-regexp regexp)
      (match-string 1))))

(defun psci/--get-psc-package-sources! ()
  (when (file-exists-p "psc-package.json")
    (split-string (shell-command-to-string "psc-package sources"))))

;; public functions

;;;###autoload
(defun psci (project-root-folder)
  "Run an inferior instance of `psci' inside Emacs.
Relies on .psci file for determining the project's root folder."
  (interactive (list (read-directory-name "Project root? "
                                          (psci/--project-root!))))
  (let* ((psci-program psci/purs-path)
         (buffer (comint-check-proc psci/buffer-name)))
    ;; pop to the "*psci*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'psci-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer (psci/--process-name psci/buffer-name)))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (setq default-directory project-root-folder)
      (let ((full-arg-list (-if-let (psc-package-sources (psci/--get-psc-package-sources!))
                               (append psci/arguments psc-package-sources)
                             psci/arguments)))
        (apply 'make-comint-in-buffer psci/buffer-name buffer
               psci-program nil "repl" full-arg-list))
      (psci-mode))))

(defvar psci-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `psci'.")

;;;###autoload
(define-derived-mode psci-mode comint-mode "psci"
  "Major mode for `run-psci'.

\\<psci-mode-map>"
  (require 'purescript-font-lock)
  (set (make-local-variable 'comint-prompt-regexp) (concat "^" (regexp-quote psci/prompt)))
  (set (make-local-variable 'paragraph-separate) "\\'") ;; so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (set (make-local-variable 'comint-input-sender-no-newline) nil)
  (set (make-local-variable 'comint-input-sender) 'comint-simple-send)
  (set (make-local-variable 'comint-get-old-input) 'comint-get-old-input-default)
  (set (make-local-variable 'comint-process-echoes) nil)
  (set (make-local-variable 'comint-prompt-read-only) t) ;; read-only prompt
  (set (make-local-variable 'comint-eol-on-send) t)
  (set (make-local-variable 'comint-input-filter-functions) nil)
  (set (make-local-variable 'font-lock-defaults) '(purescript-font-lock-keywords t))
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-use-syntax) t))

;;;###autoload
(defun psci/load-current-file! ()
  "Load the current file in the psci repl."
  (interactive)
  (save-buffer)
  (call-interactively 'psci/reset!)
  (call-interactively 'psci/load-module!))

;;;###autoload
(defun psci/load-module! ()
  "Load the module inside the repl session."
  (interactive)
  (-when-let (module-name (psci/--compute-module-name!))
    (psci/--run-psci-command! (format "import %s" module-name))))

;;;###autoload
(defun psci/reset! ()
  "Reset the current status of the repl session."
  (interactive)
  (psci/--run-psci-command! ":reset"))

;;;###autoload
(defun psci/quit! ()
  "Quit the psci session."
  (interactive)
  (psci/--run-psci-command! ":quit"))

(defvar inferior-psci-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'psci/load-current-file!)
    (define-key map (kbd "C-c M-n") 'psci/load-module!)
    map)
  "Basic mode map for `inferior-psci-mode'.")

(defgroup psci nil "psci customisation group."
  :tag "psci"
  :version "0.0.4"
  :group 'purescript
  :prefix "psci/")

;;;###autoload
(define-minor-mode inferior-psci-mode "psci minor mode to define default bindings."
  :lighter " ip"
  :keymap inferior-psci-mode-map
  :group 'psci)

(provide 'psci)
;;; psci.el ends here
