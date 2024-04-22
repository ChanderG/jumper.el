(defvar jumper-buffers nil)

(defun jumper-msg ()
  "Retrieve message to prompt user with."
  ;; TODO: make this a color based selector
  (let ((menu (mapcar
                   (lambda (x) (format "%c %s" (car x) (cadr x)))
                   jumper-buffers))
        (footer "[.] Add current; [J] Clear all; [-x] to remove buffer x"))
    (format "Buffers: \n %s\n%s" menu footer)))

(defun jumper-add (buf)
  (let ((ch (downcase (string-to-char (string-trim-left (buffer-name buf) "*")))))
    ;; TODO: use a different char if existing char is already mapped
    ;; TODO: deal with duplicates
    (push (list ch buf) jumper-buffers)
    (message "Jumper: Mapped %c to %s" ch buf)))

(defun jumper-remove (inp)
  (let ((buf (cadr (assoc inp jumper-buffers))))
    (setf jumper-buffers (assoc-delete-all inp jumper-buffers))
    (message "Jumper: Removed buffer %s" buf)))

(defun jumper-jump (inp)
  (switch-to-buffer (cadr (assoc inp jumper-buffers))))

(defun jumper-trigger ()
  (interactive)
  (let* ((msg (jumper-msg))
         (inp (read-char msg)))
    (cond ((eq inp ?J) (setq jumper-buffers nil))
          ((eq inp ?.) (jumper-add (current-buffer)))
          ((eq inp ?-) (jumper-remove (read-char "Jumper to remove: ")))
          ((eq inp ?\e) nil)
          (t (jumper-jump inp)))))

(define-key evil-normal-state-map "j" 'jumper-trigger)
