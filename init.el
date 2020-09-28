;;;;
;; Packages
;;;;

;; Mac only - swap Alt and Cmd
(setq mac-command-modifier 'meta)
(setq Mac-option-modifier 'super)

;; Define package repositories
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"     . 5)
        ("melpa"        . 0)))

;; manually downloaded packages
(add-to-list 'load-path "~/.emacs.d/packages/")


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Use libressl installed with brew for MELPA certificate management
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(

    ;; use-package - macro for loading packages: https://github.com/jwiegley/use-package
    use-package
    
    ;; Smart navigation with Helm
    ;; helm
    ;; helm-ag
    ;; helm-swoop ;; for effective search (https://github.com/emacsorphanage/helm-swoop)

    ;; ;; project navigation
    ;; helm-projectile
    ;; projectile

    ;; ;; git integration
    ;; magit

    ;; ;; company mode
    ;; company

    ))


;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))
(setenv "PATH" (concat (expand-file-name "~/.local/bin:") (getenv "PATH")))

;; install or update my-packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Turn on use-package
(require 'use-package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(exec-path-from-shell use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
