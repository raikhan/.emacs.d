;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; manually downloaded packages
(add-to-list 'load-path "~/.emacs.d/packages/")

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))


;; load exec-path-from-shell package
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    ;; multiple cursors
    multiple-cursors

    ;; ace jump mode (jump to specific character)
    ace-jump-mode

    ;; expand region
    expand-region

    ;; buffer move
    buffer-move

    ;; key chords
    key-chord

    ;; poly mode (for R markdown)
    polymode

    ;; auto complete
    auto-complete

    ;; markdown major mode 
    markdown-mode

    ;; ELPY python mode
    elpy

    ;; emacs jupyter notebook
    ein

    ;; flycheck syntax checker
    flycheck))

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

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
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

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-R.el") ;; Milan
(load "Rmarkdown.el") ;; Milan
(load "setup-python.el") ;; Milan
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (indium markdown-mode auto-complete polymode key-chord buffer-move expand-region ace-jump-mode multiple-cursors magit tagedit rainbow-delimiters projectile smex ido-ubiquitous cider clojure-mode-extra-font-locking clojure-mode paredit exec-path-from-shell)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; Milan
;;

;; set new font size
(set-face-attribute 'default nil :height 200)

;; Mac only - swap Alt and Cmd
(setq mac-command-modifier 'meta)
(setq Mac-option-modifier 'super)

;; Start with maximized window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; auto-complete setup
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
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
;; Scala
;;

;; Scala mode ENSIME
(require 'use-package)
(use-package ensime
  :ensure t
  :pin melpa-stable)

;; add sbt to path
(add-to-list 'exec-path "/usr/local/bin")

;; remove welcome screen
(setq ensime-startup-notification nil)

;; hook to start ensime-mode when scala-mode starts
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
