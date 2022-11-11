;; Setup straight.el
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Packages

;; exec-path-from-shell
(straight-use-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Evil mode
(straight-use-package 'evil)
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; Undo tree
(straight-use-package 'undo-tree)
(require 'undo-tree)
(global-undo-tree-mode 1)
(evil-set-undo-system 'undo-tree)

;; Helm
(straight-use-package 'helm)
(helm-mode 1)

;; Magit
(straight-use-package 'magit)
(straight-use-package 'evil-magit)
(require 'evil-magit)

;; Kconfig mode
(straight-use-package 'kconfig-mode)

;; JSON mode
(straight-use-package 'json-mode)

;; Python mode
(straight-use-package 'python-mode)
(straight-use-package 'lsp-pyright)
(require 'lsp-pyright)

;; Tramp
(straight-use-package 'tramp)
(setenv "SHELL" "/bin/bash")
(require 'tramp)
;; Setup yadm command
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

;; Company mode
(straight-use-package 'company-mode)
(add-hook 'after-init-hook 'global-company-mode)

(straight-use-package '(company-box :hook (company-mode . company-box-mode)))

;; LSP mode
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(evil-define-key 'normal lsp-mode-map (kbd "\\") lsp-command-map)

;; Which key
(straight-use-package 'which-key)
(which-key-mode)

;; YAML mode
(straight-use-package 'yaml-mode)

;; Yasnippet
(straight-use-package 'yasnippet)
(yas-global-mode 1)
(straight-use-package 'yasnippet-snippets)

;; Projectile
(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
(straight-use-package 'kaolin-themes)
(load-theme 'kaolin-aurora t)
(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro-10"))

;; Put newline always at the end of file
(setq require-final-newline 'visit-save)

;; Electric pair mode
(electric-pair-mode 1)
