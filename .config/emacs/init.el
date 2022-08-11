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
(straight-use-package 'evil)

;; Don't show the splash screen
(setq inhibit-startup-message t)

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
