(defun exec-r-fn-with-text (r_fn text)
  (let ((escaped-text (prin1-to-string text)))
    (ess-eval-linewise (format "%s(%s)" r_fn escaped-text))))

(defun text-around-cursor (&optional rows-around)
  (let ((rows-around (or rows-around 10))
        (current-line (line-number-at-pos))
        (initial-point (point)))
    (save-mark-and-excursion
      (goto-line (- current-line rows-around))
      (set-mark (point))
      (goto-line (+ current-line rows-around))
      (end-of-line)
      ;; Return a list of text, index
      (list (buffer-substring-no-properties (mark) (point))
            (- initial-point (mark))))))

(defun fnyes ()
  (let* ((input-context (text-around-cursor))
        (text (prin1-to-string (car input-context)))
        (index (cdr input-context)))
    (ess-eval-linewise (format "cat(fn_defn_from_cursor(%s, %s))" text index))))



