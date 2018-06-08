;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t) 
(column-number-mode t)

; (setq-default c-basic-offset 4)

(if (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(font . "Source Code Pro-14" ))
  (set-face-attribute 'default t :font "Source Code Pro-14" )
)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; recursively add ~/.emacs.d/
(let ((default-directory "~/.emacs.d/elpa/"))
    (normal-top-level-add-subdirs-to-load-path))

;; disable backup files
(setq make-backup-files nil)
;;
;; ;; disable auto save
(auto-save-mode -1)

;; If you don't have MELPA in your package archives:
(require 'package)
(add-to-list
  'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Install Intero
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'racer-mode-hook #'cargo-minor-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(setq company-tooltip-align-annotations t)


(with-eval-after-load 'lsp-mode
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  (require 'lsp-rust))

(add-hook 'rust-mode-hook #'lsp-rust-enable)
(add-hook 'rust-mode-hook #'flycheck-mode)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (groovy-mode magithub lsp-rust markdown-mode racer w3 utop tuareg pos-tip ocp-indent intero graphviz-dot-mode flycheck-rust csharp-mode cargo))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
