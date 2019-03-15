;;
;; Set of functions to work with R chunk in R markdown files
;;
(require 'polymode)

;; helper functions
(defun rmd-search-backward-r-chunk (count)
  "Helper function to move cursor to beginning of previous R chunk"
  (if  (search-backward-regexp "```{[r].*}" nil t count)
      (progn
        (search-forward "}")
        (next-line)
        (beginning-of-line))
    (message "No more R chunks")))

(defun rmd-point-in-chunk ()
  "Check if the current point is in an R code chunk"
  (interactive)
  (save-excursion
    (let ((cur (point)))
      (and
       (< (let ((res (search-backward-regexp "```{[r].*}" nil t 1)))  ;; if there is nothing to find backward, return buffer-size so first position check fails
            (if res res (buffer-size))) cur)
       (> (search-forward-regexp "```\n") cur)))))



;; movement
(defun rmd-goto-next-r-chunk ()
  "Move the pointer to the first line of next R chunk"
  (interactive)
  (if  (search-forward-regexp "```{[r].*}" nil t 1)
      (progn
        (next-line)
        (beginning-of-line))
    (progn
      (message "No more R chunks")
      nil)))

(defun rmd-goto-previous-r-chunk ()
  "Move the pointer to the first line of previous R chunk"
  (interactive)
  (if (rmd-point-in-chunk)
      (rmd-search-backward-r-chunk 2)
    (rmd-search-backward-r-chunk 1)))

(defun rmd-goto-beginning-of-r-chunk ()
  "If currently in an R chunk, move cursor to the first line"
  (interactive)
  (if (rmd-point-in-chunk)
      (rmd-search-backward-r-chunk 1)
    (message "Not in R chunk")))

(defun rmd-goto-end-of-r-chunk ()
  "If currently in an R chunk, move cursor to the first line"
  (interactive)
  (if (rmd-point-in-chunk)
      (progn
        (search-forward-regexp "```\n")
        (previous-line))
    (message "Not in R chunk")))



;; selection
(defun rmd-select-r-chunk ()
  "Select all text in an R chunk"
  (interactive)
  (if (rmd-point-in-chunk)
      (progn
        (rmd-search-backward-r-chunk 1)
        (set-mark-command nil)
        (search-forward-regexp "```")
        (beginning-of-line)
        (setq deactivate-mark nil)
        t)
    (progn
      (message "Not in R chunk")
      nil)))


;; Evaluation
(defun rmd-evaluate-r-chunk ()
  "Select and evaluate all the code in the current R chunk"
  (interactive)
  (save-excursion
    (rmd-select-r-chunk)
    (ess-eval-region (region-beginning) (region-end) nil)))

(defun rmd-evaluate-r-chunk-and-goto-next ()
  "Select and evaluate all the code in the current R chunk, then jump to next R chunk"
  (interactive)
  (if (rmd-point-in-chunk)
      (progn
        (rmd-evaluate-r-chunk)
        (rmd-goto-next-r-chunk))
    (message "Not in R chunk")))

(defun rmd-evaluate-all-r-chunks ()
  "Move the pointer to the beginning of the buffer and run all R chunks top to bottom"
  (interactive)
  (beginning-of-buffer)
  (while (search-forward-regexp "```{[r].*}" nil t 1)
    (rmd-evaluate-r-chunk))
  (rmd-goto-end-of-r-chunk)
  (next-line))   ;; in the end, the pointer is right after the last chunk



;; function to compile an R markdown document
(defun ess-rmarkdown ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
  ;; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (buf-coding (symbol-name buffer-file-coding-system))
             (R-cmd
              (format "library(rmarkdown); rmarkdown::render(\"%s\")"
                      buffer-file-name)))
        (message "Running rmarkdown on %s" buffer-file-name)
        (ess-execute R-cmd 'buffer nil nil)
        (switch-to-buffer rmd-buf)
        (ess-show-buffer (buffer-name sbuffer) nil)))))
(define-key polymode-mode-map (kbd  "M-n s") 'ess-rmarkdown)


;; polymode is missing insert-chunk for Rmd mode. Here is a custom function to do it
(defun tws-insert-r-chunk (header) 
  "Insert an r-chunk in markdown mode. Necessary due to interactions between polymode and yas snippet" 
  (interactive "sHeader: ") 
  (insert (concat "```{r " header ", warning=FALSE, message=FALSE, echo=FALSE}\n\n```")) 
  (forward-line -1))

(define-key polymode-mode-map (kbd "M-n i") 'tws-insert-r-chunk)
(define-key polymode-mode-map (kbd "M-n n") 'polymode-next-chunk-same-type)
(define-key polymode-mode-map (kbd "M-n p") 'polymode-previous-chunk-same-type)

