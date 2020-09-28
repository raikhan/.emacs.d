;;;;
;; Packages
;;;;

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


