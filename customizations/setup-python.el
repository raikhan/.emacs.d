;;
;; Python mode setup
;;

;;
;; Don't forget to install jedi server (M-X jedi:install-server) 
;; when reinstalling this setup
;;

;; Using elpy for now (switch to anaconda-mode maybe?)
(elpy-enable)

;; setup personal python mode
(defun personal-python-mode-defaults ()
  "Personal defaults for Python programming."

  ;; Enable elpy mode
  (elpy-mode)

  ;; Don't use auto-complete
  (setq elpy-default-minor-modes (delete 'auto-complete elpy-default-minor-modes))

  ;; Jedi backend                                                                                      
  (jedi:setup)
  (setq jedi:complete-on-dot t) ;optional                                                               

  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'my/python-mode-hook)

  ;; needed to use ipython as python shell in Emacs
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")

  (setq elpy-rpc-backend "jedi")

  )

(setq personal-python-mode-hook 'personal-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'personal-python-mode-hook)))


;; ;; EIN
;; (package-initialize)
;; (require 'ein)
;; (require 'ein-loaddefs)
;; (require 'ein-notebook)
;; (require 'ein-subpackages)
;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)


