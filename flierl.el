;; flierl
;;
;; This is a hack and probably shouldnt be used, however it's a checker for erlang files
;; which uses an 'inferior-erlang' process to do the compilation. It requires a
;; complementary helper lib on the erlang side.
;;
;; One big advantage (and why I did this) is that this checker is tramp friendly, temp
;; files are created on the machine where the erlang is being compiled and, as the
;; erlang shell is being used for the compilation, all includes work properly.

(require 'flycheck)
(require 'flycheck-tip)

;;------------------------------------------------------------

(defun flierl-split-tramp-path (path)
  (let ((remote (file-remote-p path)))
    (if remote
        (list remote
              (substring path (length remote)))
      (list nil path))))

;;------------------------------------------------------------

(defun flierl-comint-run-in-process (process command)
  "Send COMMAND to PROCESS."
  (let ((output-buffer " *Comint Redirect Work Buffer*"))
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (comint-redirect-send-command-to-process command
					       output-buffer process nil t)
      ;; Wait for the process to complete
      (set-buffer (process-buffer process))
      (while (and (null comint-redirect-completed)
		  (accept-process-output process)))
      ;; Collect the output
      (set-buffer output-buffer)
      (goto-char (point-min))
      ;; Skip past the command, if it was echoed
      (and (looking-at command)
	   (forward-line))
      ;; Grab the rest of the buffer
      (buffer-substring-no-properties (point) (- (point-max) 1)))))

;;------------------------------------------------------------

(defun flierl-compile-file (path)
  (let ((inhibit-quit nil))
    (read
     (substring
      (flierl-comint-run-in-process
       (get-process "inferior-erlang")
       (format "flierl:compile(\"%s\")." path))
      0 -3))))

(defun erl-to-flyc-err (file kind message)
  (pcase-let ((`(,line ,message) message))
    (flycheck-error-new-at
     line
     nil
     kind
     message
     :checker 'flierl
     :filename file)))

(defun flycheck-flierl-start (checker callback)
  (condition-case err
      (let* ((buffer-path (buffer-file-name (current-buffer)))
             (tmp-path (flycheck-save-buffer-to-temp #'tramp-flycheck-temp-file-system))
             (split-path (flierl-split-tramp-path tmp-path)))
        (pcase-let ((`(,remote-root ,local-path) split-path))
          (pcase-let ((`(,errors ,warnings) (flierl-compile-file local-path)))
            (let ((results
                   (append (seq-map (lambda (e) (erl-to-flyc-err
                                                 buffer-path 'error e))
                                    errors)
                           (seq-map (lambda (e) (erl-to-flyc-err
                                                 buffer-path 'warning e))
                                    warnings))))
              (funcall callback 'finished results)
              (flycheck-safe-delete-temporaries)))))
    (error
     (progn
       (message "DEAD")
       (funcall callback 'errored (list (error-message-string err)))))))

;;------------------------------------------------------------

(defun flycheck-verify-flierl (_checker)
  ;; take from https://github.com/flycheck/flycheck-ocaml/blob/master/flycheck-ocaml.el
  (let* ((ie-process (get-process "inferior-erlang"))
         (ie-buffer (when ie-process
                      (process-buffer ie-process))))
    (list
     (flycheck-verification-result-new
      :label "Erlang shell"
      :message (if ie-buffer
                   (format "Found at %s" (buffer-name ie-buffer))
                 "Not found")
      :face (if ie-buffer 'success '(bold error))))))

;;------------------------------------------------------------

(flycheck-define-generic-checker 'flierl
  "Best of luck to us all"
  :start 'flycheck-flierl-start
  :verify 'flycheck-verify-flierl
  :modes '(erlang-mode)
  :predicate (lambda ()
               (get-process "inferior-erlang")))

;;------------------------------------------------------------
;; Replacements for two funcs in flycheck which sadly used #'make-temp-file which is
;; always local. If we make a new version based on #'temporary-file-directory we
;; could make this whole chain work on the remote machine.

(defun tramp-flycheck-temp-file-system (filename)
  "Create a temporary file named after FILENAME.

If FILENAME is non-nil, this function creates a temporary
directory with `flycheck-temp-dir-system', and creates a file
with the same name as FILENAME in this directory.

Otherwise this function creates a temporary file with
`flycheck-temp-prefix' and a random suffix.  The path of the file
is added to `flycheck-temporaries'.

Return the path of the file."
  (let ((tempfile (convert-standard-filename
                   (if filename
                       (expand-file-name (file-name-nondirectory filename)
                                         (tramp-flycheck-temp-dir-system))
                     (make-nearby-temp-file flycheck-temp-prefix)))))
    (push tempfile flycheck-temporaries)
    tempfile))

(defun tramp-flycheck-temp-dir-system ()
  "Create a unique temporary directory.

   Use `flycheck-temp-prefix' as prefix, and add the directory to
   `flycheck-temporaries'.

   Return the path of the directory"
  (let* ((tempdir (make-nearby-temp-file flycheck-temp-prefix 'directory)))
    (push tempdir flycheck-temporaries)
    tempdir))

;;------------------------------------------------------------

;;;###autoload
(defun flierl-setup ()
  "Setup Flycheck ierl.
   Add `flierl' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'flierl))

(provide 'flierl)

;;------------------------------------------------------------
