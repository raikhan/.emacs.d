;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
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


;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; Smart navigation with Helm
    helm
    helm-projectile
    helm-dash  ;; Dash documentation
   
    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; git integration
    magit

    ;; ;; auto complete
    ;; auto-complete

    ;; company mode
    company

    ;; markdown major mode 
    markdown-mode

    ;; flycheck syntax checker
    flycheck

    ;; Minor navigation/UI packages
    multiple-cursors
    expand-region
    key-chord
    golden-ratio
    ace-jump-mode

    ;; R
    ;; ESS loaded automatically in modified Emacs 
    polymode     ;; for R markdown


    ;; Python
    elpy
    jedi

    ;; HTML/CSS
    web-mode
    emmet-mode
    impatient-mode
    company-web
    ac-html-csswatcher
    ac-html-bootstrap

    ;; JavaScript
    js2-mode
    js2-refactor
    skewer-mode
    indium
    company-tern

    ;; Emacs browser - w3m
    w3m
    helm-w3m

    ;; ;; Clojure
    ;; clojure-mode ;; https://github.com/clojure-emacs/clojure-mode
    ;; clojure-mode-extra-font-locking     ;; extra syntax highlighting for clojure
    ;; cider ;; integration with a Clojure REPL

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


;; install or update my-packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;;;
;; Customization
;;;;


;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; ;; Sets up exec-path-from-shell so that Emacs will use the correct
;; ;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;;
;; Customizations for individual languages
;;
(load "Rmarkdown.el")
(load "setup-R.el")
(load "setup-python.el")
(load "setup-js.el")   ;; Javascript
(load "setup-web.el")  ;; HTML/CSS
;; c/c++
;; clojure
;; scala



;;
;; General packages setup
;;

;; use company-mode for autocompletion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)     ; use globally

;; company-mode settings
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
(setq company-idle-delay .2)                         ; decrease delay before autocompletion popup shows
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(setq company-minimum-prefix-length 1)               ; show options after second character
(global-set-key (kbd "C-c /") 'company-files)        ; Force complete file names on "C-c /" key

;; ;; auto-complete setup
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-delay 0.1)
;; (setq ac-auto-show-menu 0.2)
;; (setq ac-quick-help-delay 0.2)
;; (setq ac-quick-help-height 10)
;; (setq ac-candidate-limit 100)

;; Magit - set shortcut for magit-status screen
(global-set-key (kbd "C-x g") 'magit-status)

;;
;; Org mode key bindings
;;
(setq org-log-done 'time)    ;; leave timestamp when set to DONE

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;;
;; Browsing in Emacs with w3m
;;
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)


;; automatically refresh files that changed on disk
(global-auto-revert-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-w3m helm-emmet js3-mode helm-dash ac-html-csswatcher ac-html-bootstrap ac-html ac-html-angular tern-django tern-auto-complete company-tern company-web helm-company company-jedi indium impatient-mode web-mode emmet-mode js2-refactor js2-highlight-vars jedi elpy golden-ratio helm-projectile helm rainbow-delimiters projectile polymode paredit multiple-cursors markdown-mode magit key-chord flycheck expand-region exec-path-from-shell clojure-mode-extra-font-locking cider auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

