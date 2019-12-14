;;
;; Setup tools for shell script editting
;;

(defun personal-shell-mode-defaults ()
  (interactive)
  (defun sh-send-line-or-region (&optional step)
    (interactive ())
    (let ((proc (get-process "shell"))
          pbuf min max command)
      (unless proc
        (let ((currbuff (current-buffer)))
          (shell)
          (switch-to-buffer currbuff)
          (setq proc (get-process "shell"))
          ))
      (setq pbuff (process-buffer proc))
      (if (use-region-p)
          (setq min (region-beginning)
                max (region-end))
        (setq min (point-at-bol)
              max (point-at-eol)))
      (setq command (concat (buffer-substring min max) "\n"))
      (with-current-buffer pbuff
        (goto-char (process-mark proc))
        (insert command)
        (move-marker (process-mark proc) (point))
        ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
      (process-send-string  proc command)
      (display-buffer (process-buffer proc) t)
      (when step 
        (goto-char max)
        (next-line))
      ))

  (defun sh-send-line-or-region-and-step ()
    (interactive)
    (sh-send-line-or-region t))
  (defun sh-switch-to-process-buffer ()
    (interactive)
    (pop-to-buffer (process-buffer (get-process "shell")) t))

  (define-key sh-mode-map (kbd  "C-<return>") 'sh-send-line-or-region-and-step)
  (define-key sh-mode-map (kbd  "C-c s") 'sh-switch-to-process-buffer)

  )

(setq personal-shell-mode-hook 'personal-shell-mode-defaults)
(add-hook 'sh-mode-hook (lambda ()
                          (run-hooks 'personal-shell-mode-hook)))

