;;; init.el --- emacs init.el

;;; Commentary:
;;; Emacs init file

;;; Code:

(eval-and-compile
  (setq load-path
        (append (delete-dups load-path)
                '("~/.emacs.d/lisp"))))

(if (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(font . "Source Code Pro-12" ))
  (set-face-attribute 'default t :font "Source Code Pro-12" )
)

(require 'package)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

; list the packages you want
(setq package-list '(intero racer cargo flycheck lsp-rust magit magithub groovy-mode company auto-complete cl iedit utop tuareg merlin))

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

;; OCaml code
(add-hook
 'tuareg-mode-hook
 (lambda ()
   ;; Add opam emacs directory to the load-path
   (setq opam-share
     (substring
      (shell-command-to-string "opam config var share 2> /dev/null")
      0 -1))
   (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
   ;; Load merlin-mode
   (require 'merlin)
   ;; Start merlin on ocaml files
   (add-hook 'tuareg-mode-hook 'merlin-mode t)
   (add-hook 'caml-mode-hook 'merlin-mode t)
   ;; Enable auto-complete
   (setq merlin-use-auto-complete-mode 'easy)
   ;; Use opam switch to lookup ocamlmerlin binary
   (setq merlin-command 'opam)

   (setq utop-command "opam config exec -- utop -emacs")

   (company-mode)
   (require 'ocp-indent)
   (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
   (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
   (autoload 'merlin-mode "merlin" "Merlin mode" t)
   (utop-minor-mode)
   (company-quickhelp-mode)
   ;; Important to note that setq-local is a macro and it needs to be
   ;; separate calls, not like setq
   (setq-local merlin-completion-with-doc t)
   (setq-local indent-tabs-mode nil)
   (setq-local show-trailing-whitespace t)
   (setq-local indent-line-function 'ocp-indent-line)
   (setq-local indent-region-function 'ocp-indent-region)
   (merlin-mode)))

(add-hook 'utop-mode-hook (lambda ()
                (set-process-query-on-exit-flag
                 (get-process "utop") nil)))

(provide 'init)
;;; init.el ends here
