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

;; Setup use-package
(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t)
  )

;; Packages

;; exec-path-from-shell
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize)
  )

;; Evil mode
(use-package evil
  :custom (evil-want-C-u-scroll t)
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1) (evil-set-undo-system 'undo-tree) (evil-set-leader 'normal (kbd "SPC"))(evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)(evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status)
  )
(use-package evil-collection
  :after evil
  :config (evil-collection-init)
  )

;; Undo tree
(use-package undo-tree
  :config (global-undo-tree-mode 1)
  )

;; Helm
(use-package helm
  :config (helm-mode 1)
  )

;; Magit
(use-package magit
  :hook turn-on-magit-gitflow)
(use-package magit-gitflow)

;; Rust setup
(use-package rustic
  :custom (rustic-format-trigger 'on-save)
  )

;; Tramp
(setenv "SHELL" "/bin/bash")
(use-package tramp
  ;; Setup yadm method
  :config (add-to-list 'tramp-methods
		       '("yadm"
			 (tramp-login-program "yadm")
			 (tramp-login-args (("enter")))
			 (tramp-login-env (("SHELL") ("/bin/sh")))
			 (tramp-remote-shell "/bin/sh")
			 (tramp-remote-shell-args ("-c"))
			 )
		       )
  )

;; Setup yadm command
(defun yadm ()
  (interactive)
  (magit-status "/yadm::")
  )

;; Ledger mode
(use-package ledger-mode
  :config (add-to-list 'ledger-reports
		       '("net" "%(binary) -f %(ledger-file) cleared ^Assets ^Liabilities")
		       )
  )

;; Company mode
(use-package company
  :init (global-company-mode)
  )
(use-package company-box
  :hook (company-mode . company-box-mode)
  )

;; LSP mode
(use-package flycheck)
(use-package lsp-mode)
(use-package lsp-ui)
(evil-define-key 'normal lsp-mode-map (kbd "\\") lsp-command-map)

;; Which key
(use-package which-key
  :config (which-key-mode)
  )

;; Auctex
(use-package auctex
  :defer t
  )

;; Yasnippet
(use-package yasnippet
  :config (yas-global-mode 1)
  )
(use-package yasnippet-snippets)

;; Projectile
(use-package projectile
  :config (projectile-mode +1)
  )
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
(use-package kaolin-themes)
(load-theme 'kaolin-aurora t)
(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro-10"))

;; Put newline always at the end of file
(setq require-final-newline 'visit-save)

;; Electric pair mode
(electric-pair-mode 1)
