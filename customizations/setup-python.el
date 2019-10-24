;;
;; Python mode setup
;;

;;
;; Don't forget to install jedi server (M-X jedi:install-server) 
;; when reinstalling this setup
;;

;; setup personal python mode
(defun personal-python-mode-defaults ()
  "Personal defaults for Python programming."
  (interactive)

  ;; Using elpy for now (switch to anaconda-mode maybe?)
  (elpy-enable)

  ;; Enable elpy mode
  (elpy-mode)

  (setq-local helm-dash-docsets '("Python_3" "Django"))

  ;; needed to use ipython as python shell in Emacs
  ;; NOTE: fixing rubish generated in ipython elpy terminal on MacOS
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i")  
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "-i --simple-prompt")

  ;; Enable navigating autocompletion menu from company-mode with C-n and C-p
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

  )

(setq personal-python-mode-hook 'personal-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'personal-python-mode-hook)))

;; Use black for code formatting automatically on save
(add-hook 'python-mode-hook 'blacken-mode)

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
;; (with-eval-after-load 'elpy
;;   ;; (when (load "flycheck" t t)
;;   ;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   ;;   (add-hook 'elpy-mode-hook 'flycheck-mode)
;;   ;;   ;; use same prev/next shortcut as flymake
;;   ;;   (define-key elpy-mode-map (kbd "C-c C-n") 'flycheck-next-error)
;;   ;;   (define-key elpy-mode-map (kbd "C-c C-p") 'flycheck-previous-error))
;;   (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

