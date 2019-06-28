;;; init.el --- emacs init.el

;;; Commentary:
;;; Emacs init file

;;; Code:

(eval-and-compile
  (setq load-path
        (append (delete-dups load-path)
                '("~/.emacs.d/elpa"))))

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

; list the packages you want
(setq package-list '(intero cargo flycheck lsp-mode lsp-treemacs helm-lsp toml-mode dap-mode lsp-ui rustic magit magithub irony irony-eldoc flycheck-irony flycheck-rust company-lsp company-irony groovy-mode company auto-complete iedit utop tuareg merlin merlin-eldoc ocp-indent protobuf-mode yasnippet yasnippet-snippets))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(auto-save-mode -1)
;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

(column-number-mode t)
(line-number-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp)
(require 'lsp-clients)
(require 'lsp-ui)
(require 'toml-mode)
(add-hook 'lsp-mode-hook   #'lsp-ui-mode)

(require 'company-lsp)
(push 'company-lsp company-backends)

(when (>= emacs-major-version 26)
  (require 'rustic)
  (require 'rustic-compile)
  (setq rustic-lsp-server 'rls)
  (add-hook 'rustic-mode-hook  #'lsp))

;; set c-basic-offset
; (setq-default c-basic-offset 4)

(global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)

(when (and (display-graphic-p) (eq system-type 'darwin))
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-setenv "SHELL" "/bin/bash")))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(add-hook 'haskell-mode-hook 'intero-mode)

;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'rust-mode-hook 'cargo-minor-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;; (add-hook 'racer-mode-hook #'cargo-minor-mode)
;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; (add-hook 'rust-mode-hook #'lsp-rust)
;; (add-hook 'rust-mode-hook #'flycheck-mode)

(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'objc-mode-hook #'lsp)

; irony mode
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; (add-hook 'irony-mode-hook #'irony-eldoc)
;; (eval-after-load 'flycheck
;;  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;;(eval-after-load 'company
;;  '(add-to-list 'company-backends 'company-irony))

(if (file-exists-p "/usr/local/share/emacs/site-lisp/cask/cask.el")
    ( progn
      (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
      (cask-initialize))
  ( when (file-exists-p "~/.cask/cask.el")
    (require 'cask "~/.cask/cask.el")
    (cask-initialize))
)

(provide 'init)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; OCaml code
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
