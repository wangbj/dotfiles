(setq inhibit-splash-screen t)
(setq inhibit-startup-message t) 
(column-number-mode t)

(if (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(font . "Source Code Pro-10" ))
  (set-face-attribute 'default t :font "Source Code Pro-10" )
)

(cua-mode 1)

;; recursively add ~/.emacs.d/
(let ((default-directory "~/.emacs.d/elpa/"))
    (normal-top-level-add-subdirs-to-load-path))

;; disable backup files
(setq make-backup-files nil)
;;
;; ;; disable auto save
(auto-save-mode -1)
;;

;; line numbers
(if (>= emacs-major-version 23) (require 'linum))

(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   (quote
    (("Okular"
      ("Command")
      "okular --unique %o#src:%n%b"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open"))))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")))))

;;(require 'cl-lib)

;(require 'haskell-mode)

(add-to-list 'package-archives
         '("melpa" . "https://stable.melpa.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;(add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode)
;(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;(custom-set-variables '(haskell-process-type 'stack-ghci))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(if (eq system-type 'darwin)
  (add-to-list 'load-path "~/Library/Haskell/share/ghc-7.10.3-x86_64/ghc-mod-5.5.0.0/elisp")
)
(if (eq system-type 'gnu/linux)
  (add-to-list 'load-path "~/.cabal/share/x86_64-linux-ghc-7.10.3/ghc-mod-5.5.0.0")
)

(require 'haskell-mode)
(define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
;(setq haskell-hoogle-command "hoogle")

;(autoload 'ghc-init "ghc" nil t)
;(autoload 'ghc-debug "ghc" nil t)

;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;        LaTeX         ;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ##### Run emacs in server mode in order to be able to use
;; ##### emacsclient in Okular. Don't forget to configure
;; ##### Okular to use emacs in
;; ##### "Configuration/Configure Okular/Editor"
;; ##### => Editor => Emacsclient. (you should see
;; ##### emacsclient -a emacs --no-wait +%l %f
;; ##### in the field "Command".
(server-start) 

(setq TeX-PDF-mode t) ;; use pdflatex instead of latex

(setq-default TeX-master nil)
;; ##### Enable synctex correlation. From Okular just press
;; ##### Shift + Left click to go to the good line.
(setq TeX-source-correlate-method 'synctex)
;; ##### Enable synctex generation. Even though the command shows
;; ##### as "latex" pdflatex is actually called
(custom-set-variables '(LaTeX-command "latex -synctex=1") )

;; ##### Use Okular to open your document at the good
;; ##### point. It can detect the master file.
(add-hook 'LaTeX-mode-hook '(lambda ()
                  (add-to-list 'TeX-expand-list
                       '("%u" Okular-make-url))))

(defun Okular-make-url () (concat
               "file://"
               (expand-file-name (funcall file (TeX-output-extension) t)
                         (file-name-directory (TeX-master-file)))
               "#src:"
               (TeX-current-line)
               (expand-file-name (TeX-master-directory))
               "./"
               (TeX-current-file-name-master-relative)))

;; ## Use these lines if you want a confirmation of the
;; ## command line to run...
;; (setq TeX-view-program-selection '((output-pdf "Okular")))
;; (setq TeX-view-program-list '(("Okular" "okular --unique %u")))
;; ## And theses if you don't want any confirmation.
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list 
		'("View" "okular --unique %u" TeX-run-discard-or-function nil t :help "View file")))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
