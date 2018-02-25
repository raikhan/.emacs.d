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

;; for highlighting variable names
(eval-after-load "js2-highlight-vars-autoloads"
  '(add-hook 'js2-mode-hook (lambda () (js2-highlight-vars-mode))))

;; company mode setup to use Tern
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

                          
;;
;; HTML / CSS tools
;;

;; web mode for editing templates
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;; Load company mode for HTML
(require 'company-web-html)                  
;; (add-to-list 'company-backends 'company-web-html)        
(add-hook 'web-mode-hook (lambda ()
                           (set (make-local-variable 'company-backends) '(company-web-html
                                                                          company-css
                                                                          company-dabbrev-code
                                                                          company-dabbrev))
                           (company-mode t)))

(require 'ac-html-csswatcher)
(company-web-csswatcher-setup)

;; for live showing HTML in browser
(require 'impatient-mode)

;; Emmet - web dev shortcuts: https://emmet.io/
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)


;;
;; Other
;;

