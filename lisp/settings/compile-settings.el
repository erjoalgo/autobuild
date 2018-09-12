(defun erjoalgo-compile-disable-query-on-proc-exit (proc)
  (set-process-query-on-exit-flag proc nil))

(add-hook 'compilation-start-hook 'erjoalgo-compile-disable-query-on-proc-exit)
