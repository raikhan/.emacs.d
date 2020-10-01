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


