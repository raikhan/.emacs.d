;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; projectile everywhere + using helm-projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; define projectile mode map shortcut
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; My navigation additions


;; comment / uncomment code
(global-set-key (kbd "C-S-d") 'comment-or-uncomment-region)

;; ;; set just-one-space to M-\ (replacing delete-horizontal-space)
;; (global-set-key (kbd "M-\\") 'just-one-space) 

;; move cursor to minibuffer
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
(global-set-key "\C-c o" 'switch-to-minibuffer) ;; Bind to `C-c o'

;; Setting and moving to register shortcut
(global-set-key (kbd "C-c r") (lambda () (interactive) (print "Register set!") (point-to-register 'r)))
(global-set-key (kbd "C-c f") (lambda () (interactive) (jump-to-register 'r)))


;; quick window switching (https://github.com/magnars/.emacs.d/blob/master/defuns/buffer-defuns.el)
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)


;; for Dired mode: use 'a' instead of RET when chaning directories to prevent opening new buffers
(put 'dired-find-alternate-file 'disabled nil)


;;
;; Multiple cursors setup
;;
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C->") 'mc/mark-all-like-this)


;;
;; expand-region setup
;;
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; ;; package to swap buffers between windows
;; (require `buffer-move)


;;
;; eval-and-replace function (evaluate an elist expresion and replace it with its result)
;;
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; start key-chord mode
(require 'key-chord)
(key-chord-mode 1)


;; avy - an advanced ace-jump mode
(avy-setup-default)
(global-set-key (kbd "C-'") 'avy-goto-char-timer)
(global-set-key (kbd "C-z") 'avy-goto-char)


;;
;; Helm setup
;;
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 30)
(helm-autoresize-mode 1)

;;
;; Settings from http://tuhdo.github.io/helm-intro.html
;;
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Helm-mini
(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; imenu / semantic mode for extracting coding tags from text (find functions/ variables etc)
;; Opens with C-c h i   (C-c h is the Helm prefix)
(semantic-mode 1)

(setq helm-semantic-fuzzy-match t

      helm-imenu-fuzzy-match    t)

;; menu for mark ring
(global-set-key (kbd "C-c SPC") 'helm-all-mark-rings)

;; for google search in helm
(global-set-key (kbd "C-c h g") 'helm-google-suggest)

;; ;; helm eshell history
;; (require 'helm-eshell)

;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;; ;; regular shell history (M-x shell)
;; (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

(helm-mode 1)


;; NeoTree setup
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))


;; Automatically search the web using s from: https://github.com/zquestz/s
(defun web-search-using-s (searchq &optional provider)
  "Do a web search using s command. Function modelled on the example from http://ergoemacs.org/emacs/elisp_universal_argument.html"
  (interactive
   (cond
    ((equal current-prefix-arg nil) ; no C-u
     (list (read-string "Enter query: ") ""))
    (t ; any other combination of C-u
     (list
      (read-string "Enter query: " )
      (concat " -p " (read-string "Enter provider: ")) 
      ))))
  ;; call s
  (shell-command (concat "s " searchq provider))
  )
(global-set-key (kbd "C-c s") 'web-search-using-s)

