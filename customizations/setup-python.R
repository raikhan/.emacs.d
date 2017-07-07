;;
;; Python mode setup
;;

;;
;; Elpy
;;

;; set PATH inside Emacs (should be done before loading elpy
(exec-path-from-shell-copy-env "PATH")

;; enable elpy mode for Python
(package-initialize)
(elpy-enable)

;; ;; needed to use ipython as python shell in Emacs
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(elpy-use-ipython)
