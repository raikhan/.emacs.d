;; Customizations relating to editing a buffer.

;;;;
;; Selection
;;;;

;; Turn on cua-mode for advanced rectangle selection
(setq cua-rectangle-mark-key (kbd "C-x SPC"))
(cua-selection-mode t)
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; Overwrite selection with new typing
(delete-selection-mode 1)

;;;;
;; Tabs to spaces
;;;;

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; function to replace tabs in a buffer with 4 spaces
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 4)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))


;;;;
;; Comments
;;;;

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)


;;;;
;; Indentation
;;;;

;; switch off electric-indent-mode, a minor mode to automatically indent code on newline
(setq electric-indent-mode nil)

;; use special tool for indented blocks syntax (Python, YAML)
(use-package indent-tools
  :ensure t
  :bind
  ("C-c i" .  indent-tools-hydra/body)
)

;;;;
;; Brackets and quotes
;;;;

;; Enclose selection in parenthesis/quotes
;; standard parenthesis already mapped to M-(
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-'") 'insert-pair)

;; Use electric pair mode (automatically closes brackets and quotes)
(electric-pair-mode)

;;;;
;; Other
;;;;

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :config
  (setq save-place-file (concat user-emacs-directory "places"))  ; keep track of saved places in ~/.emacs.d/places
  (setq save-place-forget-unreadable-files nil)                  ; make sure unreadable files are not recorded, makes emacs closing much faster
  (save-place-mode 1)
)

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))


;; Enable log4j-mode for Java log formatting
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))


