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
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")

  )

(setq personal-python-mode-hook 'personal-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'personal-python-mode-hook)))

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

