;;
;; Python mode setup
;;

;;
;; Elpy
;;

;; set PATH inside Emacs (should be done before loading elpy
(exec-path-from-shell-copy-env "PATH")

;; enable elpy mode for Python
(elpy-enable)

;; needed to use ipython as python shell in Emacs
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(elpy-use-ipython)

(setq elpy-rpc-backend "jedi")

;; setup personal python mode
(defun personal-python-mode-defaults ()
  "Personal defaults for Python programming."
  ;; Enable elpy mode
  (elpy-mode)
  ;; Jedi backend                                                                                       
  (jedi:setup)
  (setq jedi:complete-on-dot t) ;optional                                                               
  (auto-complete-mode)
  (jedi:ac-setup)

  (flycheck-mode)

  (setq elpy-rpc-python-command "python3") 
  (python-shell-interpreter "ipython3")
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


