(defun exec-r-fn-with-text (r_fn text)
  (let ((escaped-text (prin1-to-string text)))
    (ess-eval-linewise (format "%s(%s)" r_fn escaped-text))))

(defun text-around-cursor (&optional rows-around)
  (let ((rows-around (or rows-around 10))
        (current-line (line-number-at-pos)))
    (save-mark-and-excursion
      (goto-line (- current-line rows-around))
      (set-mark (point))
      (goto-line (+ current-line rows-around))
      (end-of-line)
      (buffer-substring-no-properties (mark) (point)))))



