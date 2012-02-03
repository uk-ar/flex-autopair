;; Electric pairing.

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

(defun electric-pair-dwim-wrap-region (closer)
  (if (< (point) (mark))
      (goto-char (mark))
    ;; We already inserted the open-paren but at the end of the region,
    ;; so we have to remove it and start over.
    (delete-char -1)
    (save-excursion
      (goto-char (mark))
      (insert last-command-event)))
  (insert closer))

(defun electric-pair-dwim-wrap-region2 (beg end closer)
  (goto-char beg)

  (goto-char end)
  (insert closer)
  )

(defun electric-pair-dwim-post-self-insert-function ()
  (let* ((syntax (and (eq (char-before) last-command-event) ; Sanity check.
                      electric-pair-dwim-mode
                      (let ((x (assq last-command-event
                                     electric-pair-dwim-pairs)))
                        (cond
                         (x (if (eq (car x) (cdr x)) ?\" ?\())
                         ((rassq last-command-event electric-pair-dwim-pairs)
                          ?\))
                         (t (char-syntax last-command-event))))))
         ;; FIXME: when inserting the closer, we should maybe use
         ;; self-insert-command, although it may prove tricky running
         ;; post-self-insert-hook recursively, and we wouldn't want to trigger
         ;; blink-matching-open.
         (closer (if (eq syntax ?\()
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
    (cond
     Wrap a pair around the active region.
     ((and (memq syntax '(?\( ?\" ?\$)) (use-region-p))
      (electric-pair-dwim-wrap-region closer))
     ;; Backslash-escaped: no pairing, no skipping.
     ((save-excursion
        (goto-char (1- (point)))
        (not (zerop (% (skip-syntax-backward "\\") 2))))
      nil)
     ;; Skip self.
     ((and (memq syntax '(?\) ?\" ?\$))
           electric-pair-dwim-skip-self
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
      (save-excursion (insert closer)))
     )))

;; (primitive-undo (1+ (key-combo-count-boundary key-combo-undo-list))
;;                         key-combo-undo-list)

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
  :group 'electricity
  (if electric-pair-dwim-mode
      ;; (add-hook 'post-self-insert-hook
      (add-hook 'post-command-hook
                #'electric-pair-dwim-post-self-insert-function)
    ;; (remove-hook 'post-self-insert-hook
    (remove-hook 'post-command-hook
                 #'electric-pair-dwim-post-self-insert-function)))

(provide 'electric-pair-dwim)
;;; electric-pair-dwim.el ends here
