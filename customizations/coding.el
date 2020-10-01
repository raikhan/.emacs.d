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


