;;
;; Settings useful to all programing modes
;; 

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
)


(use-package company
  :ensure t
  :init (global-company-mode)
  :bind(:map company-active-map
             ("C-n" . company-select-next-or-abort)
             ("C-p" . company-select-previous-or-abort)
             )
  :config
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
  (setq company-idle-delay .2)                         ; decrease delay before autocompletion popup shows
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (setq company-minimum-prefix-length 1)               ; show options after second character
)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
)


(use-package lsp-mode
  :ensure t
)


(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-mode)
)


