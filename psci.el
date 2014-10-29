;;; psci.el --- Major mode for purescript repl psci

;; Copyright (C) 2014 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.0.4
;; Package-Requires: ((purescript-mode "13.10") (dash "2.9.0") (s "1.9.0") (f "0.17.1") (deferred "0.3.2"))
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

;; To activate psci directly from a purescript-mode buffer, you
;; could use repl-toggle (available on melpa):
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
(require 'deferred)

(defvar psci/buffer-name "psci"
  "Buffer name of the psci buffer.")

(defun psci/process-name (buffer-name)
  "Compute the buffer's process name based on BUFFER-NAME."
  (format "*%s*" buffer-name))

(defvar psci/file-path "psci"
  "Path to the program used by `psci' function.")

(defvar psci/arguments '()
  "Commandline arguments to pass to `psci' function.")

(defvar psci-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `psci'.")

(defvar psci/prompt "> "
  "The psci prompt.")

(defun psci ()
  "Run an inferior instance of `psci' inside Emacs."
  (interactive)
  (let* ((psci-program psci/file-path)
         (buffer (comint-check-proc psci/buffer-name)))
    ;; pop to the "*psci*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'psci-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer (psci/process-name psci/buffer-name)))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (set (make-local-variable 'default-directory) (psci/--project-root!))
      (apply 'make-comint-in-buffer psci/buffer-name buffer
             psci-program psci/arguments)
      (psci-mode))))

;;;###autoload
(define-derived-mode psci-mode comint-mode "psci"
  "Major mode for `run-psci'.

\\<psci-mode-map>"
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

(defun psci/--run-psci-command! (command)
  "Run psci COMMAND as string."
  (-when-let (process (get-buffer-process (psci/process-name psci/buffer-name)))
    (comint-simple-send process command)
    (process-send-eof process)))

;; (defun psci/load-region! (region-start region-end)
;;   "Run purescript code between REGION-START and REGION-END."
;;   (interactive "r")
;;   (-when-let (process (get-buffer-process (psci/process-name psci/buffer-name)))
;;     (comint-send-region process region-start region-end)
;;     (process-send-eof process)))

(defun psci/--load-file! (filename)
  "Load the purescript FILENAME inside the current running session."
  (psci/--run-psci-command! (format ":m %s" filename)))

;;;###autoload
(defun psci/load-current-file! ()
  "Load the current file in the psci repl."
  (interactive)
  (lexical-let ((archive-folder (psci/--compute-modules-folder (psci/--project-root!)))
                (module-name    (psci/--compute-module-name!)))
    (when module-name
      (deferred:$
        (deferred:process-shell (format "rm -rf %s/node_modules/%s" archive-folder module-name))
        (deferred:nextc it
          (lambda ()
            (call-interactively 'psci/reset!)))))))

(defun psci/--compute-module-name! ()
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
  (-when-let (module-name (psci/--compute-module-name!))
    (psci/--run-psci-command! (format ":i %s" module-name))))

(defvar psci/project-module-file ".psci"
  "The default file referencing the purescript modules to load at psci startup.")

(defun psci/--file-content (filename)
  "Load the FILENAME's content as a string.
When FILENAME is nil or not a real file, returns nil."
  (when (and filename (file-exists-p filename))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun psci/--symbol (sym n)
  "Compute the repetition of a symbol SYM N times as a string."
  (--> n
    (-repeat it sym)
    (s-join "" it)))

(defun psci/--project-psci-file (project-root-folder)
  "Compute the project's psci file from the PROJECT-ROOT-FOLDER.
Returns nil if no .psci file is found."
  (let ((psci-module-file (expand-file-name psci/project-module-file project-root-folder)))
    (when (file-exists-p psci-module-file)
      psci-module-file)))

(defun psci/--project-module-files! ()
  "Compulse the list of modules for the current project.
Assumes the location of the modules is the project root folder."
  (let* ((parent-root-folder (psci/--project-root!))
         (psci-module-file   (psci/--project-psci-file parent-root-folder)))
    (when psci-module-file
      (->> psci-module-file
        psci/--file-content
        (s-split "\n")
        (--map (s-concat "./" (cadr (s-split ":m " it))))
        (-filter 'file-exists-p)
        nreverse))))

(defvar psci/--modules-folder ".psci_modules"
  "The modules folder psci uses as cache.")

(defun psci/--compute-modules-folder (project-root-folder)
  "Compute the psci modules folder from PROJECT-ROOT-FOLDER."
  (concat project-root-folder psci/--modules-folder))

;;;###autoload
(defun psci/load-project-modules! ()
  "Load the modules needed for the repl session.
We chose to load the .psci file's content (the purescript doc proposes its use)."
  (interactive)
  (lexical-let ((archive-folder (psci/--compute-modules-folder (psci/--project-root!))))
    (deferred:$
      (deferred:process-shell "rm -rf " (shell-quote-argument (format "%s/node_modules/*" archive-folder))) ;; clean compiled version
      (deferred:nextc it (lambda () (call-interactively 'psci/reset!)))                                         ;; flush in-memory version
      (deferred:nextc it                                                                                   ;; at last reload all files
        (lambda ()
          (-when-let (modules (psci/--project-module-files!))
            (mapc #'psci/--load-file! modules)))))))

;;;###autoload
(defun psci/reset! ()
  "Reset the current status of the repl session."
  (interactive)
  (psci/--run-psci-command! ":r"))

(defun psci/--project-root! ()
  "Determine the project's root folder."
  (->> psci/project-module-file
    (locate-dominating-file default-directory)
    expand-file-name))

(defvar inferior-psci-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'psci/load-current-file!)
    (define-key map (kbd "C-c C-r") 'psci/load-project-modules!)
    (define-key map (kbd "C-c M-n") 'psci/load-module!)
    map)
  "Basic mode map for `inferior-psci-mode'.")

(defgroup psci nil " psci customisation group."
  :tag "psci"
  :version "0.0.4")

;;;###autoload
(define-minor-mode inferior-psci-mode "Extend the bindings ."
  :lighter " ip"
  :keymap inferior-psci-mode-map
  :group 'psci)

(provide 'psci)
;;; psci.el ends here
