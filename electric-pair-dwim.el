;; Electric pairing.

(defcustom electric-pair-pairs
  '((?\" . ?\"))
  "Alist of pairs that should be used regardless of major mode."
  :type '(repeat (cons character character)))

(defcustom electric-pair-skip-self t
  "If non-nil, skip char instead of inserting a second closing paren.
When inserting a closing paren character right before the same character,
just skip that character instead, so that hitting ( followed by ) results
in \"()\" rather than \"())\".
This can be convenient for people who find it easier to hit ) than C-f."
  :type 'boolean)

(defun electric-pair-post-self-insert-function ()
  (let* ((syntax (and (eq (char-before) last-command-event) ; Sanity check.
                      electric-pair-mode
                      (let ((x (assq last-command-event electric-pair-pairs)))
                        (cond
                         (x (if (eq (car x) (cdr x)) ?\" ?\())
                         ((rassq last-command-event electric-pair-pairs) ?\))
                         (t (char-syntax last-command-event))))))
         ;; FIXME: when inserting the closer, we should maybe use
         ;; self-insert-command, although it may prove tricky running
         ;; post-self-insert-hook recursively, and we wouldn't want to trigger
         ;; blink-matching-open.
         (closer (if (eq syntax ?\()
                     (cdr (or (assq last-command-event electric-pair-pairs)
                              (aref (syntax-table) last-command-event)))
                   last-command-event)))
    (cond
     ;; Wrap a pair around the active region.
     ((and (memq syntax '(?\( ?\" ?\$)) (use-region-p))
      (if (> (mark) (point))
          (goto-char (mark))
        ;; We already inserted the open-paren but at the end of the region,
        ;; so we have to remove it and start over.
        (delete-char -1)
        (save-excursion
          (goto-char (mark))
          (insert last-command-event)))
      (insert closer))
     ;; Backslash-escaped: no pairing, no skipping.
     ((save-excursion
        (goto-char (1- (point)))
        (not (zerop (% (skip-syntax-backward "\\") 2))))
      nil)
     ;; Skip self.
     ((and (memq syntax '(?\) ?\" ?\$))
           electric-pair-skip-self
           (eq (char-after) last-command-event))
      ;; This is too late: rather than insert&delete we'd want to only skip (or
      ;; insert in overwrite mode).  The difference is in what goes in the
      ;; undo-log and in the intermediate state which might be visible to other
      ;; post-self-insert-hook.  We'll just have to live with it for now.
      (delete-char 1))
     ;; Insert matching pair.
     ((not (or (not (memq syntax `(?\( ?\" ?\$)))
               overwrite-mode
               ;; I find it more often preferable not to pair when the
               ;; same char is next.
               (eq last-command-event (char-after))
               (eq last-command-event (char-before (1- (point))))
               ;; I also find it often preferable not to pair next to a word.
               (eq (char-syntax (following-char)) ?w)))
      (save-excursion (insert closer))))))

;; post-self-insert-hook is emacs 24 hook
;;;###autoload
(define-minor-mode electric-pair-mode
  "Toggle automatic parens pairing (Electric Pair mode).
With a prefix argument ARG, enable Electric Pair mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Electric Pair mode is a global minor mode.  When enabled, typing
an open parenthesis automatically inserts the corresponding
closing parenthesis.  \(Likewise for brackets, etc.)"
  :global t
  :group 'electricity
  (if electric-pair-mode
      ;; (add-hook 'post-self-insert-hook
      (add-hook 'post-command-hook
                #'electric-pair-post-self-insert-function)
    ;; (remove-hook 'post-self-insert-hook
    (remove-hook 'post-command-hook
                 #'electric-pair-post-self-insert-function)))

(defcustom electric-pair-dwim-pairs
  '((?\" . ?\"))
  "Alist of pairs that should be used regardless of major mode."
  :type '(repeat (cons character character)))

(defcustom electric-pair-dwim-skip-self t
  "If non-nil, skip char instead of inserting a second closing paren.
When inserting a closing paren character right before the same character,
just skip that character instead, so that hitting ( followed by ) results
in \"()\" rather than \"())\".
This can be convenient for people who find it easier to hit ) than C-f."
  :type 'boolean)

;; (progn (skeleton-insert
;;         (cons nil skeleton)
;;         (if markp -1))
;;        (cond ((and openp (< (mark) (point)))
;;               (exchange-point-and-mark))
;;              ((and closep (< (point) (mark)))
;;               (exchange-point-and-mark)))
;;        (message "n1"))

(defun electric-pair-dwim-wrap-region (beg end opener closer)
  (goto-char end)
  (insert closer)
  (save-excursion
    (goto-char beg)
    (insert opener)
    ))

;; Electric pairing.
;; (electric-pair-dwim-mode -1)
;; (electric-pair-dwim-mode 1)
;; (electric-pair-mode -1)
;; (electric-pair-mode 1)
(defun electric-pair-dwim-comment-or-stringp (&optional pos)
  (setq pos (or pos (point)))
  (memq (get-text-property pos 'face)
        '(font-lock-comment-face font-lock-doc-face
                                 font-lock-string-face))
  ;;(not (memq (char-syntax (following-char)) '(?\" ?\')))
  ;; (not (eq (previous-single-property-change (1+ (point)) 'face)
  ;;          (point)))
  )

(defun electric-pair-dwim-escapedp (&optional pos)
  (setq pos (or pos (point)))
  (save-excursion
    (goto-char pos)
    (not (zerop (% (skip-syntax-backward "\\") 2))))
  )

(defun electric-pair-dwim (syntax)
  ;; FIXME: when inserting the closer, we should maybe use
  ;; self-insert-command, although it may prove tricky running
  ;; post-self-insert-hook recursively, and we wouldn't want to trigger
  ;; blink-matching-open.
  (let
      ((closer (if (eq syntax ?\()
                   (cdr (or (assq last-command-event electric-pair-dwim-pairs)
                            (aref (syntax-table) last-command-event)))
                 last-command-event))
       (opener (if (eq syntax ?\))
                   (cdr (or (assq last-command-event electric-pair-dwim-pairs)
                            (aref (syntax-table) last-command-event)))
                 last-command-event))
       (pair (if (or (eq syntax ?\() (eq syntax ?\)))
                 ;; assq is ok?
                 (cdr (or (assq last-command-event electric-pair-dwim-pairs)
                          (aref (syntax-table) last-command-event)))
               last-command-event)))
    (delete-char -1)
    (cond
     ;; Wrap a pair around the active region.
     ((and (memq syntax '(?\( ?\" ?\$)) (use-region-p))
      (if (< (point) (mark)) (exchange-point-and-mark))
      (electric-pair-dwim-wrap-region (mark) (point) opener closer))
     ;; Backslash-escaped: no pairing, no skipping.
     ((electric-pair-dwim-escapedp)
      (call-interactively 'self-insert-command))
     ;; Skip self.
     ((and (memq syntax '(?\) ?\" ?\$))
           electric-pair-dwim-skip-self
           (eq (char-after) last-command-event))
      (forward-char 1)
      (message "skip")
      )
     ((and (memq syntax '(?\( ?\" ?\$)))
      (call-interactively 'self-insert-command)
      (insert closer))
     (t (call-interactively 'self-insert-command)
        ;; (insert closer)
        (message "t")
        )
     )
    ;; ;; Insert matching pair.
    ;;  ((not (or (not (memq syntax `(?\( ?\" ?\$)))
    ;;            overwrite-mode
    ;;            ;; I find it more often preferable not to pair when the
    ;;            ;; same char is next.
    ;;            (eq last-command-event (char-after))
    ;;            (eq last-command-event (char-before (1- (point))))
    ;;            ;; I also find it often preferable not to pair next to a word.
    ;;            (eq (char-syntax (following-char)) ?w)))
    ;;   (save-excursion (insert closer)))
    ;;  )
  ))

(defun electric-pair-dwim-post-command-function ()
  (interactive)
  (let* ((syntax (and (eq (char-before) last-command-event) ; Sanity check.
                      electric-pair-dwim-mode
                      (let ((x (assq last-command-event
                                     electric-pair-dwim-pairs)))
                        (cond
                         (x (if (eq (car x) (cdr x)) ?\" ?\())
                         ((rassq last-command-event electric-pair-dwim-pairs)
                          ?\))
                         (t (char-syntax last-command-event)))))))
    (if (memq syntax '(?\) ?\( ?\" ?\$))
        (electric-pair-dwim syntax))
    ))

;; post-self-insert-hook is emacs 24 hook
;;;###autoload
(define-minor-mode electric-pair-dwim-mode
  "Toggle automatic parens pairing (Electric Pair mode).
With a prefix argument ARG, enable Electric Pair mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Electric Pair mode is a global minor mode.  When enabled, typing
an open parenthesis automatically inserts the corresponding
closing parenthesis.  \(Likewise for brackets, etc.)"
  :global t
  :lighter " EP"
  :group 'electricity
  (if electric-pair-dwim-mode
      ;; (add-hook 'post-self-insert-hook
      (add-hook 'post-command-hook
                #'electric-pair-dwim-post-command-function)
    ;; (remove-hook 'post-self-insert-hook
    (remove-hook 'post-command-hook
                 #'electric-pair-dwim-post-command-function)))

(dont-compile
  (when(fboundp 'expectations)
    (expectations
      (desc "electric-pair-dwim")
      ;; (expect "(word)"
      (electric-pair-dwim-mode 1)
      (transient-mark-mode t)
      (expect "("
        (with-temp-buffer
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (buffer-string)
          ))
      (expect "(word)"
        (with-temp-buffer
          (set-mark (point))
          (insert "word")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          (buffer-string)
          ))
      (expect "word)"
        (with-temp-buffer
          (set-mark (point))
          (insert "word")
          (setq last-command-event ?\))
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          (buffer-string)
          ))
      (expect "\"word\""
        (with-temp-buffer
          (set-mark (point))
          (insert "word")
          (setq last-command-event ?\")
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          ;; (buffer-substring-no-properties (point-min) (point-max))
          (buffer-string)
          ))
      (expect "\\("
        (with-temp-buffer
          (insert "\\")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          ;; (buffer-substring-no-properties (point-min) (point-max))
          (buffer-string)
          ))
      (expect "\\\\()"
        (with-temp-buffer
          (insert "\\\\")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          ;; (buffer-substring-no-properties (point-min) (point-max))
          (buffer-string)
          ))
      (expect '("()" 3)
        (with-temp-buffer
          (insert "(")
          (save-excursion
            (insert ")"))
          (setq last-command-event ?\))
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          ;; (buffer-substring-no-properties (point-min) (point-max))
          (list (buffer-string) (point)))
          ))
      )))

(provide 'electric-pair-dwim)
;;; electric-pair-dwim.el ends here
