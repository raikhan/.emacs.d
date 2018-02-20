;;
;; Python mode setup
;;

;; Using elpy for now (switch to anaconda-mode maybe?)
(elpy-enable)

;; setup personal python mode
(defun personal-python-mode-defaults ()
  "Personal defaults for Python programming."

  ;; Enable elpy mode
  (elpy-mode)

  ;; Jedi backend                                                                                      
  (jedi:setup)
  (setq jedi:complete-on-dot t) ;optional                                                               
  ;; needed to use ipython as python shell in Emacs
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")

  ;; (setq elpy-rpc-backend "jedi")

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


