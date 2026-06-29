;;; init.el --- emacs init.el

;;; Commentary:
;;; Emacs init file

;;; Code:

(eval-and-compile
  (setq load-path
        (append (delete-dups load-path)
                '("~/.emacs.d/lisp"))))

(require 'package)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

; list the packages you want
(setq package-list '(auctex company flycheck lsp-ui magit forge dap-mode which-key lsp-mode lsp-haskell rustic use-package))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

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

(when (and (display-graphic-p) (eq system-type 'darwin))
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-setenv "SHELL" "/bin/bash")))

(use-package flycheck)

(use-package forge :after magit)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(use-package lsp-mode
  :hook ((rustic-mode . lsp)
         (python-mode . lsp)
         (haskell-mode . lsp)
         (haskell-literate-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (objc-mode . lsp)
         (go-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-idle-delay 0.500)
  )

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)

(use-package which-key
  :config
  (which-key-mode))

(with-eval-after-load 'lsp-mode
  (setq lsp-diagnostics-modeline-scope :project)
  (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode))

;; Rustic, LSP
(require 'rustic)

;; (setq rustic-lsp-client nil)
(setq rustic-lsp-server 'rust-analyzer)
(setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))
(setq rustic-format-on-save t)
;; (push 'rustic-clippy flycheck-checkers)
;; (remove-hook 'rustic-mode-hook 'flycheck-mode)

;; Set up AUCTeX and connect it to lsp-mode
(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . lsp-deferred)
         (LaTeX-mode . turn-on-reftex))
  :config
  ;; Recommended AUCTeX settings for parsing documents
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; Set XeLaTeX as the default compilation engine
  (setq-default TeX-engine 'xetex)
  ;; Force AUCTeX to compile straight to PDF
  (setq-default TeX-PDF-mode t)
  ;; Ask for the master file when creating a new document
  (setq-default TeX-master nil))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
