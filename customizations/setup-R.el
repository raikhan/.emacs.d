
;; magrittr pipe - type ">" twice quickly
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global ">>" " %>% ")
(key-chord-define-global "<<" " %<>% ")

;;
;; ESS setup
;;

;; ESS - Emacs speaks statistics (installed in coding/libraries)
(require 'ess-site)

;; Control and up/down arrow keys to search history with matching what you've already typed:
(define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
(define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)


;; polymode for R markdown
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))


;; switch <- symbol to M-- key
(ess-toggle-underscore nil)
(ess-toggle-S-assign nil)
(ess-toggle-S-assign nil)

(defun ess-S-assign-custom ()
  (interactive)
  (insert " <- "))

(global-unset-key (kbd "M--"))
(define-key ess-mode-map (kbd "M--") `ess-S-assign-custom)
(define-key inferior-ess-mode-map (kbd "M--") 'ess-S-assign-custom)


