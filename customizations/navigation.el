;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

;;
;; Helm setup
;;
(use-package helm
  :ensure t
  :demand t
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-mini)
	 :map helm-map
	 ("<tab>" . 'helm-execute-persistent-action) ; rebind tab to run persistent action
	 ("C-i" . 'helm-execute-persistent-action)   ; make TAB work in terminal
	 ("C-z" . 'helm-select-action)               ; list actions using C-z
	 ("C-c SPC" . 'helm-all-mark-rings)          ; helm menu for mark ring
	 ("C-c h g" . 'helm-google-suggest )         ; search web in helm using google
	 )
  :config
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t
	helm-echo-input-in-header-line        t
	helm-autoresize-max-height            0
	helm-autoresize-min-height           30
	helm-M-x-fuzzy-match                  t ; fuzzy match all the things
	helm-buffers-fuzzy-matching           t
	helm-recentf-fuzzy-match              t
	helm-semantic-fuzzy-match             t
        helm-imenu-fuzzy-match                t
	mark-ring-max                         3 ; use a smaller size of the mark ring so it is more easy to manage with helm
	)

  (helm-autoresize-mode 1)
  (helm-mode 1)
)

(use-package helm-config
  :demand t
  :after helm
  :init
  (custom-set-variables '(helm-command-prefix-key "C-c h"))
  :config
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  ;; (global-set-key (kbd "C-c h") 'helm-command-prefix)  
  (global-unset-key (kbd "C-x c"))  
)

(use-package helm-eshell
  ;; use helm to show command history in eshell
  :after helm
  :hook (eshell-mode . (lambda ()
			 (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
  :bind
  (:map shell-mode-map
   ("C-c C-l" . helm-coming-input-ring))
)


;;
;; Projectile
;; 
(use-package projectile
  :ensure t
  :after helm
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'helm)
  (projectile-global-mode)
)

(use-package helm-projectile
  ;; All projectile functions use a helm interface
  :ensure t
  :after projectile
  :config
  (setq helm-projectile-fuzzy-match nil)
  (helm-projectile-on)
)


;;
;; Multiple cursors
;;
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-S-c C->" . mc/mark-all-like-this)
)


;;
;; expand-region
;;
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
)

;;
;; Avy - an advanced ace-jump mode
;;
(use-package avy
  :ensure t
  :bind
  ("C-z" . avy-goto-char-timer)
  ;;("C-z" . avy-goto-char)
  :config
  (avy-setup-default)
)

;;
;; Other packages
;;

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(use-package recentf
  :config
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (setq recentf-max-menu-items 40)
  (recentf-mode 1)
)


;;
;; Other functions and keyboard shortcuts
;;

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; comment / uncomment code
(global-set-key (kbd "C-S-d") 'comment-or-uncomment-region)

;; ;; set just-one-space to M-\ (replacing delete-horizontal-space)
;; (global-set-key (kbd "M-\\") 'just-one-space)

;; ;; move cursor to minibuffer
;; (defun switch-to-minibuffer ()
;;   "Switch to minibuffer window."
;;   (interactive)
;;   (if (active-minibuffer-window)
;;       (select-window (active-minibuffer-window))
;;     (error "Minibuffer is not active")))
;; (global-set-key "\C-c o" 'switch-to-minibuffer) ;; Bind to `C-c o'




;; ;; quick window switching (https://github.com/magnars/.emacs.d/blob/master/defuns/buffer-defuns.el)
;; (defun toggle-window-split ()
;;   (interactive)
;;   (if (= (count-windows) 2)
;;       (let* ((this-win-buffer (window-buffer))
;;              (next-win-buffer (window-buffer (next-window)))
;;              (this-win-edges (window-edges (selected-window)))
;;              (next-win-edges (window-edges (next-window)))
;;              (this-win-2nd (not (and (<= (car this-win-edges)
;;                                          (car next-win-edges))
;;                                      (<= (cadr this-win-edges)
;;                                          (cadr next-win-edges)))))
;;              (splitter
;;               (if (= (car this-win-edges)
;;                      (car (window-edges (next-window))))
;;                   'split-window-horizontally
;;                 'split-window-vertically)))
;;         (delete-other-windows)
;;         (let ((first-win (selected-window)))
;;           (funcall splitter)
;;           (if this-win-2nd (other-window 1))
;;           (set-window-buffer (selected-window) this-win-buffer)
;;           (set-window-buffer (next-window) next-win-buffer)
;;           (select-window first-win)
;;           (if this-win-2nd (other-window 1))))))

;; (defun rotate-windows ()
;;   "Rotate your windows"
;;   (interactive)
;;   (cond ((not (> (count-windows)1))
;;          (message "You can't rotate a single window!"))
;;         (t
;;          (setq i 1)
;;          (setq numWindows (count-windows))
;;          (while  (< i numWindows)
;;            (let* (
;;                   (w1 (elt (window-list) i))
;;                   (w2 (elt (window-list) (+ (% i numWindows) 1)))

;;                   (b1 (window-buffer w1))
;;                   (b2 (window-buffer w2))

;;                   (s1 (window-start w1))
;;                   (s2 (window-start w2))
;;                   )
;;              (set-window-buffer w1  b2)
;;              (set-window-buffer w2 b1)
;;              (set-window-start w1 s2)
;;              (set-window-start w2 s1)
;;              (setq i (1+ i)))))))

;; (windmove-default-keybindings) ;; Shift+direction
;; (global-set-key (kbd "C-x -") 'toggle-window-split)
;; (global-set-key (kbd "C-x C--") 'rotate-windows)


;; ;; for Dired mode: use 'a' instead of RET when chaning directories to prevent opening new buffers
;; (put 'dired-find-alternate-file 'disabled nil)


;; ;; package to swap buffers between windows
;; (require `buffer-move)





;; ;;
;; ;; eval-and-replace function (evaluate an elist expresion and replace it with its result)
;; ;;
;; (defun eval-and-replace ()
;;   "Replace the preceding sexp with its value."
;;   (interactive)
;;   (backward-kill-sexp)
;;   (condition-case nil
;;       (prin1 (eval (read (current-kill 0)))
;;              (current-buffer))
;;     (error (message "Invalid expression")
;;            (insert (current-kill 0)))))
;; (global-set-key (kbd "C-c e") 'eval-and-replace)

;; ;; start key-chord mode
;; (require 'key-chord)
;; (key-chord-mode 1)







;; ;; NeoTree setup
;; (require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)
;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))


;; ;; Automatically search the web using s from: https://github.com/zquestz/s
;; (defun web-search-using-s (searchq &optional provider)
;;   "Do a web search using s command. Function modelled on the example from http://ergoemacs.org/emacs/elisp_universal_argument.html"
;;   (interactive
;;    (cond
;;     ((equal current-prefix-arg nil) ; no C-u
;;      (list (read-string "Enter query: ") ""))
;;     (t ; any other combination of C-u
;;      (list
;;       (read-string "Enter query: " )
;;       (concat " -p " (read-string "Enter provider: ")) 
;;       ))))
;;   ;; call s
;;   (shell-command (concat "s " searchq provider))
;;   )

;; (defun web-search-current-region (beg end)
;;   "Search the web for the string in the selected region"
;;   (interactive (if (use-region-p)
;;                    (list (region-beginning) (region-end))
;;                  (list nil nil)))
;;   (if (and beg end)
;;       (web-search-using-s (buffer-substring beg end))             
;;     (message "Select region first!")))

;; (global-set-key (kbd "C-c s") 'web-search-using-s)
;; (global-set-key (kbd "C-S-c s") 'web-search-current-region)


;;;;
;; Not sure about what this does from old version of the file
;;;;

;; (defun spacemacs//helm-hide-minibuffer-maybe ()
;;   "Hide minibuffer in Helm session if we use the header line as input field."
;;   (when (with-helm-buffer helm-echo-input-in-header-line)
;;     (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;       (overlay-put ov 'window (selected-window))
;;       (overlay-put ov 'face
;;                    (let ((bg-color (face-background 'default nil)))
;;                      `(:background ,bg-color :foreground ,bg-color)))
;;       (setq-local cursor-type nil))))


;; (add-hook 'helm-minibuffer-set-up-hook
;;           'spacemacs//helm-hide-minibuffer-maybe)


;; ;; imenu / semantic mode for extracting coding tags from text (find functions/ variables etc)
;; ;; Opens with C-c h i   (C-c h is the Helm prefix)
;; (semantic-mode 1)
