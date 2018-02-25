;;
;; javascript
;;

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; ;; setup skewer-mode
;; (require 'skewer-mode)
;; (add-hook 'js2-mode-hook 'skewer-mode)

;; use indium interactively
(require 'indium)
(add-hook 'js2-mode-hook #'indium-interaction-mode)

;; ;; refactoring
;; (require 'js2-refactor)
;; (add-hook 'js2-mode-hook #'js2-refactor-mode)
;; (js2r-add-keybindings-with-prefix "C-c C-m")  ;; prefix
;; (setq js2-skip-preprocessor-directives t)

;; ;; for highlighting variable names
;; NOTE - nice mode but breaks company-tern
;; (eval-after-load "js2-highlight-vars-autoloads"
;;   '(add-hook 'js2-mode-hook (lambda () (js2-highlight-vars-mode))))

;; company mode setup to use Tern
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

;; dash docsets
(defun js-dash-hook () 
  (interactive)
  (setq-local helm-dash-docsets '("Javascript" "NodeJS" "UnderscoreJS")))
(add-hook 'js2-mode-hook 'js-dash-hook)


