;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(eval-when-compile
  (require 'calc))

(require 'thingatpt)
(require 'seq)

(defun quick-calc-dwim (&optional arg)
  "Call `calc-eval-region' if there is active region, otherwise
call `quick-calc'"
  (interactive "P")
  (call-interactively (if (use-region-p) #'my/calc-eval-region #'quick-calc)))

(defun my/calc-eval-region (arg beg end)
  "Calculate the region and show the result in the echo area.
With one prefix ARG (\\[universal-argument]), replace the region with
the result.  With two, insert the result after the region."
  (interactive "p\nr")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (when (consp result)                ; calc-eval returns (POS MSG) on error
      (user-error "Calc: %s" (cadr result)))
    (pcase arg
      (4  (goto-char beg)
          (push-mark (point) 'nomsg)
          (delete-region beg end)
          (insert result))
      (16 (goto-char end)
          (push-mark (point) 'nomsg)
          (insert result))
      (_  (message "%s = %s" expr result)))))

(defun my/calc-push-number-at-point ()
  "Push the number at point onto the Calc stack and show Calc."
  (interactive)
  (let ((str (thing-at-point 'number t)))
    (if (null str)
        (user-error "No number at point")
      (calc-eval str 'push)
      (calc))))

(defun my/calc-grab-number-at-point ()
  "Push the number at point in the previously used buffer onto the Calc stack."
  (interactive)
  (let* ((buf (seq-find
               (lambda (b)
                 (and (not (string-prefix-p " " (buffer-name b)))
                      (not (minibufferp b))
                      (not (memq (buffer-local-value 'major-mode b)
                                 '(calc-mode calc-trail-mode)))))
               (buffer-list)))
         (win (and buf (get-buffer-window buf)))
         (num (and buf
                   (with-current-buffer buf
                     (save-excursion
                       ;; point of a buffer shown in another window lives in the window
                       (when win (goto-char (window-point win)))
                       (number-at-point)))))
         (val (and num (math-read-number (number-to-string num)))))
    (unless val (user-error "No number at point in %s" (if buf (buffer-name buf) "any buffer")))
    (calc-wrapper (calc-push val))))

(use-package calc
  :bind (("<f10>" . #'calc)
	 ("C-<f10>" . #'quick-calc-dwim)
	 :map calc-mode-map
	 ("M-n" . #'my/calc-grab-number-at-point)
	 :map calc-dispatch-map
	 ("N" . #'my/calc-push-number-at-point)
	 ("M-n" . #'my/calc-push-number-at-point)))

