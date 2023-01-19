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
  :config (evil-mode 1) (evil-set-undo-system 'undo-tree)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global
    (kbd "<leader>fs") 'save-buffer
    (kbd "<leader>ff") 'helm-find-files
    (kbd "<leader>fr") 'helm-recentf
    (kbd "<leader>gs") 'magit-status
    (kbd "<leader>gy") 'yadm
    (kbd "<leader>qQ") 'kill-emacs
    (kbd "<leader>qq") 'save-buffers-kill-terminal
    )
  (evil-define-key 'normal lsp-mode-map (kbd ",") lsp-command-map)
  (evil-define-key 'normal projectile-mode-map (kbd "<leader>p") 'projectile-command-map)
  )

(use-package evil-collection
  :after evil
  :config (evil-collection-init)
  )

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1)
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
  :custom
  (magit-revision-show-gravatars t)
  (magit-diff-refine-hunk 'all)
  )

;; Rust setup
(use-package rustic
  :custom (rustic-format-trigger 'on-save)
  )

;; Kconfig mode
(use-package kconfig-mode)

;; JSON mode
(use-package json-mode)

;; Tramp
;; Setup yadm tramp method
(setenv "SHELL" "/bin/bash")
(add-to-list 'tramp-methods
	     '("yadm"
	       (tramp-login-program "yadm")
	       (tramp-login-args (("enter")))
	       (tramp-login-env (("SHELL") ("/bin/sh")))
	       (tramp-remote-shell "/bin/sh")
	       (tramp-remote-shell-args ("-c"))))

;; Setup yadm command
(defun yadm ()
  (interactive)
  (magit-status "/yadm::")
  )


;; Company mode
(use-package company
  :init (global-company-mode)
  )
(use-package company-box
  :hook (company-mode . company-box-mode)
  )

;; Python mode
(use-package python-mode)

;; Pyvenv
(use-package pyvenv)
(use-package pyvenv-auto
  :hook (python-mode . pyvenv-auto-run)
  )

;; LSP mode
(use-package flycheck)
(use-package lsp-mode
  :custom (lsp-pylsp-plugins-pylint-enabled t)
  :hook (c-mode python-mode)
  )
(use-package lsp-ui)

;; Python mode
(use-package lua-mode)

;; Which key
(use-package which-key
  :config (which-key-mode)
  )

;; YAML mode
(use-package yaml-mode)

;; Yasnippet
(use-package yasnippet
  :config (yas-global-mode 1)
  )
(use-package yasnippet-snippets)

;; Projectile
(use-package projectile
  :config (projectile-mode +1)
  )

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  )

;; All the icons
(use-package all-the-icons)

;; Format all the code
(use-package format-all
  :hook (
	 c-mode
	 emacs-lisp-mode
	 json-mode
	 markdown-mode
	 python-mode
	 sql-mode
	 ) (format-all-mode . format-all-ensure-formatter)
  )

;; Docker
(use-package docker)
(use-package dockerfile-mode)

;; Startup screen
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'fundamental-mode)

;; Turn off UI elements
(menu-bar-mode -1)
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
