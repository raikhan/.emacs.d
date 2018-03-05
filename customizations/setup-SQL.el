;;
;; Setup SQLi for accessing SQL databases
;;

;; truncate lines
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; automatically capitalize SQL commands
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)

