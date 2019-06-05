;;;; Edit *score* interactively --- a slight modification of slime-edit-value
;;;

(setq score-buffer-name "*echorepl edit score*")

(defun echorepl-edit-score ()
  "\\<echorepl-edit-score-mode-map>\
Edit the value of echorepl::*score*
The value is inserted into a temporary buffer for editing and then set
in Lisp when committed with \\[echorepl-edit-score-commit]."
  (interactive)
  (slime-eval-async `(swank:value-for-editing "*score*")
    #'echorepl-edit-score-callback
    "echorepl"))

(global-set-key (kbd "C-c e") 'echorepl-edit-score)
(global-set-key (kbd "C-c r") 'echorepl-rename-clip)

(define-minor-mode echorepl-edit-score-mode
  "Mode for editing echorepl::*score*"
  nil
  "Edit-Value"
  '(("\C-c\C-c" . echorepl-edit-score-commit)))

(defun echorepl-edit-score-callback (score)
  (let ((buffer 
	 (or (get-buffer score-buffer-name)
	     (slime-with-popup-buffer (score-buffer-name :package "echorepl"
							 :connection t
							 :select t
							 :mode 'lisp-mode)
	       (slime-popup-buffer-mode -1) ; don't want binding of 'q'
	       (slime-mode 1)
	       (echorepl-edit-score-mode 1)
	       (current-buffer)))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert score)
      (message "Type C-c C-c to save and compile the score."))
    (pop-to-buffer buffer)))

(defun echorepl-update-score-callback (score)
  (let ((buffer (get-buffer score-buffer-name)))
    (if buffer
	(with-current-buffer buffer
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert score)))))

(defslimefun echorepl-update-score (_)
  (slime-eval-async `(swank:value-for-editing "*score*")
		    #'echorepl-update-score-callback
		    "echorepl"))

(defun echorepl-edit-score-commit ()
  "Commit the edited score to the Lisp image.
\\(See `echorepl-edit-score'.)"
  (interactive)
  (let ((value (buffer-substring-no-properties (point-min) (point-max))))
    (lexical-let ((buffer (current-buffer)))
      (slime-eval-async `(swank:commit-edited-value "*score*"
						    ,value)
	(lambda (_)
	  (slime-eval-async `(echorepl::play-score)))))))

(defun echorepl-rename-clip ()
  (interactive)
  (let* ((name (or (slime-symbol-at-point)
		   (read-string "Clip to rename: ")))
	 (new-name (read-string (concat "Rename " name " to: "))))
    (slime-eval-async
	`(echorepl::rename-for-slime ,name ,new-name)
      #'echorepl-update-score)))
