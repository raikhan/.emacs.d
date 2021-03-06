;; magrittr pipe - type ">" twice quickly
(key-chord-define-global ">>" " %>% ")
(key-chord-define-global "<<" " %<>% ")

;;
;; ESS setup
;;

(defun personal-R-mode-defaults ()

  ;; Control and up/down arrow keys to search history with matching what you've already typed:
  (define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
  (define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)


  ;; Enable navigating autocompletion menu from company-mode with C-n and C-p
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)


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

  ;;
  ;; Rmarkdown shortcuts
  ;;
  (define-key polymode-mode-map (kbd  "M-RET") 'rmd-evaluate-r-chunk)
  (define-key polymode-mode-map (kbd  "M-n M-RET") 'rmd-evaluate-r-chunk-and-goto-next)
  (define-key polymode-mode-map (kbd  "M-n RET") 'rmd-evaluate-all-r-chunks)

  (define-key polymode-mode-map (kbd  "<M-up>") 'rmd-goto-previous-r-chunk)
  (define-key polymode-mode-map (kbd  "<M-down>") 'rmd-goto-next-r-chunk)
  (define-key polymode-mode-map (kbd  "<M-left>") 'rmd-goto-beginning-of-r-chunk)
  (define-key polymode-mode-map (kbd  "<M-right>") 'rmd-goto-end-of-r-chunk)

  ;; Use electric pair mode (automatically closes brackets and quotes)
  (electric-pair-mode))

;; ESS - Emacs speaks statistics (installed in coding/libraries)
(require 'ess-site)

;; polymode for R markdown
(require 'polymode)
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(setq personal-R-mode-hook 'personal-R-mode-defaults)

(add-hook 'ess-mode-hook (lambda ()
                           (run-hooks 'personal-R-mode-hook)))
