;;
;; Python mode setup
;; 
;; 6/10/2020 - I tried some of the LSP packages below, but default Elpy with Jedi is still the best in terms of completion on my Mac. Did not really change the setup at all
;;

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred

;; (use-package lsp-jedi
;;   :ensure t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     (add-to-list 'lsp-enabled-clients 'jedi))
;;   :hook (python-mode . (lambda () (lsp)))
;; )


;; Make sure packages are installed
(use-package elpy
  :ensure t)

(use-package blacken
  :ensure t)

;; setup personal python mode
(defun personal-python-mode-defaults ()
  "Personal defaults for Python programming."
  (interactive)

  ;; Using elpy for now (switch to anaconda-mode maybe?)
  (elpy-enable)

  ;; Enable elpy mode
  (elpy-mode)


  ;; needed to use ipython as python shell in Emacs
  ;; NOTE: fixing rubish generated in ipython elpy terminal on MacOS
  (setq elpy-shell-echo-output nil
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i")

)

(setq personal-python-mode-hook 'personal-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'personal-python-mode-hook)))

;; Use black for code formatting automatically on save
(add-hook 'python-mode-hook 'blacken-mode)

;; disable company in python shell
(add-hook 'inferior-python-mode-hook
          (lambda () (company-mode -1)))


;; fix for elpy native-completion problem
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))


;; Enable flycheck in elpy
(with-eval-after-load 'elpy
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    ;; use same prev/next shortcut as flymake
    (define-key elpy-mode-map (kbd "C-c C-n") 'flycheck-next-error)
    (define-key elpy-mode-map (kbd "C-c C-p") 'flycheck-previous-error))
  ;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
)

