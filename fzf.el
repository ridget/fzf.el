;;; fzf.el --- A front-end for fzf.
;;
;; Copyright (C) 2015 by Bailey Ling
;; Author: Bailey Ling
;; URL: https://github.com/bling/fzf.el
;; Filename: fzf.el
;; Description: A front-end for fzf
;; Created: 2015-09-18
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.4"))
;; Keywords: fzf fuzzy search
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Install:
;;
;; Autoloads will be set up automatically if you use package.el.
;;
;; Usage:
;;
;; M-x fzf
;; M-x fzf-directory
;; M-x fzf-git
;; M-x fzf-git-files
;; M-x fzf-hg
;; M-x fzf-projectile
;; M-x fzf-git-grep
;;
;;; Code:

(require 'subr-x)

(defgroup fzf nil
  "Configuration options for fzf.el"
  :group 'convenience)

(defcustom fzf/window-height 15
  "The window height of the fzf buffer"
  :type 'integer
  :group 'fzf)

(defcustom fzf/executable "fzf"
  "The path to the fzf executable."
  :type 'string
  :group 'fzf)

(defcustom fzf/args "-x --color bw --print-query"
  "Additional arguments to pass into fzf."
  :type 'string
  :group 'fzf)

(defcustom fzf/git-grep-args "-i --line-number %s"
  "Arguments to pass into git grep. %s is the search term placeholder"
  :type 'string
  :group 'fzf)

(defcustom fzf/position-bottom t
  "Set the position of the fzf window. Set to nil to position on top."
  :type 'bool
  :group 'fzf)

(defcustom fzf/directory-start nil
  "The path of the default start directory for fzf-directory."
  :type 'string
  :group 'fzf)

(defun fzf/grep-cmd (cmd args)
  (format (concat cmd " " args)
          (shell-quote-argument
           (if (region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (read-from-minibuffer (concat cmd ": "))))))

(defun fzf/after-term-handle-exit (process-name msg)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (lines (split-string text "\n" t "\s*>\s+"))
         (line (car (last (butlast lines 1))))
         (selected (split-string line ":"))
         (file (expand-file-name (pop selected)))
         (linenumber (pop selected)))
    (kill-buffer "*fzf*")
    (jump-to-register :fzf-windows)
    (when (file-exists-p file)
      (find-file file))
    (when linenumber
      (goto-char (point-min))
      (forward-line (- (string-to-number linenumber) 1))
      (back-to-indentation)))
  (advice-remove 'term-handle-exit #'fzf/after-term-handle-exit))

(defun fzf/start (directory &optional cmd-stream)
  (require 'term)
  (window-configuration-to-register :fzf-windows)
  (advice-add 'term-handle-exit :after #'fzf/after-term-handle-exit)
  (let* ((buf (get-buffer-create "*fzf*"))
         (min-height (min fzf/window-height (/ (window-height) 2)))
         (window-height (if fzf/position-bottom (- min-height) min-height))
         (window-system-args (when window-system " --margin=1,0"))
         (fzf-args (concat fzf/args window-system-args))
         (sh-cmd (if cmd-stream (concat cmd-stream " | " fzf/executable " " fzf-args)
                   (concat fzf/executable " " fzf-args))))
    (with-current-buffer buf
      (setq default-directory directory))
    (split-window-vertically window-height)
    (when fzf/position-bottom (other-window 1))
    (make-term "fzf" "sh" nil "-c" sh-cmd)
    (switch-to-buffer buf)
    (linum-mode 0)
    (visual-line-mode 0)

    ;; disable various settings known to cause artifacts, see #1 for more details
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t) ;for paths wider than the window
    (setq-local show-trailing-whitespace nil)
    (setq-local display-line-numbers nil)
    (face-remap-add-relative 'mode-line '(:box nil))

    (term-char-mode)
    (setq mode-line-format (format "   FZF  %s" directory))))

(defun fzf/vcs (match)
  (let ((path (locate-dominating-file default-directory match)))
    (if path
        (fzf/start path)
      (fzf-directory))))

(defun fzf/git-files ()
  (let ((process-environment
         (cons (concat "FZF_DEFAULT_COMMAND=git ls-files")
               process-environment))
        (path (locate-dominating-file default-directory ".git")))
    (if path
        (fzf/start path)
      (user-error "Not inside a Git repository"))))

;;;###autoload
(defun fzf ()
  "Starts a fzf session."
  (interactive)
  (if (fboundp #'projectile-project-root)
      (fzf/start (condition-case err
                     (projectile-project-root)
                   (error
                    default-directory)))
    (fzf/start default-directory)))

;;;###autoload
(defun fzf-directory ()
  "Starts a fzf session at the specified directory."
  (interactive)
  (fzf/start (ido-read-directory-name "Directory: " fzf/directory-start)))

;;;###autoload
(defun fzf-git ()
  "Starts a fzf session at the root of the current git."
  (interactive)
  (fzf/vcs ".git"))

;;;###autoload
(defun fzf-git-files ()
  "Starts a fzf session only searching for git tracked files."
  (interactive)
  (fzf/git-files))

;;;###autoload
(defun fzf-hg ()
  "Starts a fzf session at the root of the curreng hg."
  (interactive)
  (fzf/vcs ".hg"))

;;;###autoload
(defun fzf-projectile ()
  "Starts a fzf session at the root of the projectile project."
  (interactive)
  (require 'projectile)
  (fzf/start (projectile-project-root)))

;;;###autoload
(defun fzf-git-grep ()
  "Starts a fzf session based on git grep result. The input comes
   from the prompt or the selected region"
  (interactive)
  (fzf/start (locate-dominating-file default-directory ".git")
             (fzf/grep-cmd "git grep" fzf/git-grep-args)))

(defun fzf-with-command (command action &optional directory)
                                        ; Set FZF_DEFAULT_COMMAND and then call fzf/start. If command is nil, leave FZF_DEFAULT_COMMAND
                                        ; alone and use the users normal command
                                        ;
                                        ; For some inputs it would be much more efficient to directly pass the output to FZF rather than
                                        ; capture in emacs, then pass to FZF. This function takes a command and uses/abuses
                                        ; FZF_DEFAULT_COMMAND to run and pass the output to FZF
  (interactive)
  (if command
      (let
          ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=" command "") process-environment)))
        (fzf/start directory action))
    (fzf/start directory action)
    )
  )

(defun fzf-base (action &optional directory)
  "Run FZF without setting default command"
  (interactive)
  (fzf-with-command "" action directory)
  )

(defun fzf-with-entries (entries action &optional directory)
  "`entries' is a list of strings that is piped into `fzf' as a source."
                                        ; FZF will read from stdin only if it detects stdin is not a tty, which amounts to something being
                                        ; piped in. Unfortunately the emacs term-exec code runs /bin/sh -c exec "command", so it cannot
                                        ; take in a pipeline of shell commands. Like bling/fzf.el/pull/20, abuse the FZF_DEFAULT_COMMAND,
                                        ; environment var
  (interactive)
  (if entries
      (fzf-with-command (concat "echo \"" (mapconcat (lambda (x) x) entries "\n") "\"") action directory)
    (message "FZF not started because contents nil")
    )
  )

(defun fzf-base (action &optional directory)
  "Run FZF without setting default command"
  (interactive)
  (fzf-with-command "" action directory)
  )

(defun fzf/resolve-directory (&optional directory)
  ; An example function to resolve a directory in a user command, before passing it to fzf. Here if
  ; directory is undefined, attempt to use the projectile root. Users can define their own as
  ; desired
  ;
  ; Example usage:
  ; (defun fzf-example ()
  ;   (fzf
  ;    (lambda (x) (print x))
  ;    (fzf/resolve-directory directory)))
  (cond
   (directory directory)
   ((fboundp #'projectile-project-root) (condition-case err (projectile-project-root) (error default-directory)))
   (t "")
  )
)

;; Prebuilt user commands
(defun fzf-switch-buffer ()
  (interactive)
  (fzf-with-entries
   (seq-filter
    (lambda (x) (not (string-prefix-p " " x)))
    (mapcar (function buffer-name) (buffer-list))
   )
    (lambda (x) (set-window-buffer nil x))
  )
)

(defun fzf-find-file (&optional directory)
  (interactive)
  (let ((d (fzf/resolve-directory directory)))
    (fzf-base
    (lambda (x)
        (let ((f (expand-file-name x d)))
        (when (file-exists-p f)
            (find-file f))))
    d
    )
  )
)

(defun fzf-find-file-in-dir (directory)
  (interactive "sDirectory: ")
  (fzf-find-file directory)
)

(defun fzf-recentf ()
  (interactive)
  (fzf-with-entries recentf-list
    (lambda (f) (when (file-exists-p f) (find-file f))))
)

(defun fzf-grep (search &optional directory)
  (interactive "sGrep: ")
  (let ((d (fzf/resolve-directory directory)))
    (fzf-with-command
    (format "grep -rHn %s ." search)
    (lambda (x)
      (let* ((parts (split-string x ":"))
             (f (expand-file-name (nth 0 parts) d)))
        (when (file-exists-p f)
          (find-file f)
          (goto-line (string-to-number (nth 1 parts))))))
    d)))

(defun fzf-test ()
  (fzf-with-entries
   (list "a" "b" "c")
   (lambda (x) (print x))))

(provide 'fzf)
;;; fzf.el ends here
