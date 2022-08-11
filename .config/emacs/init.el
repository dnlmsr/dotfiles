;; Setup straight.el
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Packages
(straight-use-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(straight-use-package 'evil)
(straight-use-package 'helm)
(straight-use-package 'magit)
(straight-use-package 'evil-magit)
(straight-use-package 'rustic)
(straight-use-package 'tramp)
(setenv "SHELL" "/bin/bash")
(require 'tramp)
(add-to-list 'tramp-methods
	     '("yadm"
	       (tramp-login-program "yadm")
	       (tramp-login-args (("enter")))
	       (tramp-login-env (("SHELL") ("/bin/sh")))
	       (tramp-remote-shell "/bin/sh")
	       (tramp-remote-shell-args ("-c"))))
(defun yadm ()
  (interactive)
  (magit-status "/yadm::"))

;; Startup screen
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'fundamental-mode)

;; Turn off UI elements
; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; Load theme
(load-theme 'wheatgrass t)

;; Load evil mode
(require 'evil)
(evil-mode 1)

;; Load helm mode
(helm-mode 1)

(require 'evil-magit)

;; Put newline always at the end of file
(setq require-final-newline 'visit-save)
