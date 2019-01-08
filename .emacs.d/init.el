;;; init.el --- emacs init.el

;;; Commentary:
;;; Emacs init file

;;; Code:

(eval-and-compile
  (setq load-path
        (append (delete-dups load-path)
                '("~/.emacs.d/lisp"))))

(when (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(font . "Source Code Pro-12" ))
  (set-face-attribute 'default t :font "Source Code Pro-12" )
)

(require 'package)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

; list the packages you want
(setq package-list '(intero racer cargo flycheck lsp-rust magit magithub irony irony-eldoc flycheck-irony company-irony groovy-mode company auto-complete iedit utop tuareg merlin merlin-eldoc ocp-indent))

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

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'racer-mode-hook #'cargo-minor-mode)
;(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;(setq company-tooltip-align-annotations t)


(with-eval-after-load 'lsp-mode
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  (require 'lsp-rust))

;;(add-hook 'rust-mode-hook #'lsp-rust-enable)
;;(add-hook 'rust-mode-hook #'flycheck-mode)

; irony mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook #'irony-eldoc)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

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
