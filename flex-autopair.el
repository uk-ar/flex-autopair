;; Electric pairing.

(defcustom flex-autopair-pairs
  '((?\" . ?\"))
  ;; '(nil)
  "Alist of pairs that should be used regardless of major mode."
  :type '(repeat (cons character character)))

(defcustom flex-autopair-skip-self t
  "If non-nil, skip char instead of inserting a second closing paren.
When inserting a closing paren character right before the same character,
just skip that character instead, so that hitting ( followed by ) results
in \"()\" rather than \"())\".
This can be convenient for people who find it easier to hit ) than C-f."
  :type 'boolean)

(defun flex-autopair-wrap-region (beg end opener closer)
  (let ((marker (copy-marker end)))
    (goto-char beg)
    (insert opener)
    (save-excursion
      (goto-char (marker-position marker))
      (insert closer)
      (goto-char beg)
      (show-paren-function)
      )
    ))

(defun flex-autopair-comment-or-stringp (&optional pos)
  (setq pos (or pos (point)))
  (memq (get-text-property pos 'face)
        '(font-lock-comment-face font-lock-doc-face
                                 font-lock-string-face))
  ;;(not (memq (char-syntax (following-char)) '(?\" ?\')))
  )

(defun flex-autopair-escapedp (&optional pos)
  (setq pos (or pos (point)))
  (save-excursion
    (goto-char pos)
    (not (zerop (% (skip-syntax-backward "\\") 2))))
  )

(defun flex-autopair-get-bounds (symbol)
  (if (eq symbol 'region)
      (and (use-region-p)
           (if (< (point) (mark))
               `(,(point) . ,(mark))
             `(,(mark) . ,(point))))
    (let ((bounds
           (bounds-of-thing-at-point symbol)))
      (if (and (eq (car bounds) (point))
               ;; bug for bounds-of-thing-at-point
               (not (eq (point-min) (point-max))))
          bounds nil)
      )))

(defun flex-autopair-openp (syntax &optional pos)
  (setq pos (if (bobp) 1 (1- (or pos (point)))))
  (and (not (eq syntax ?\)))
       (or (eq syntax ?\();; '(?\( ?\" ?\$)
           ;; FIXME: bug with temp buffer
           (not (eq (get-text-property pos 'face) 'font-lock-string-face))
       )))

(defun flex-autopair-smart-insert-space ()
  (unless (or (eq (char-syntax (preceding-char)) ? )
              (eq (char-syntax (preceding-char)) ?\( )
              (eq (char-syntax (preceding-char)) ?')
              (bolp))
    (insert " " )))

(setq flex-autopair-lisp-mode
  '(lisp-mode emacs-lisp-mode lisp-interaction-mode
              inferior-gauche-mode scheme-mode)
  )

;; (defcustom flex-autopair-conditions
(setq flex-autopair-conditions
  '(((flex-autopair-escapedp) . self)
    (overwrite-mode . self)
    ;; Wrap a pair.
    ((and openp (flex-autopair-get-bounds 'region)) . bounds)
    ;; ((and openp (flex-autopair-get-url)) . region);; symbol works better
    ((and openp (flex-autopair-get-bounds 'symbol)) . bounds)
    ;; for lisp
    ((and openp
          (eq syntax ?\()
          (memq major-mode flex-autopair-lisp-mode)
          (flex-autopair-get-bounds 'sexp)) . bounds-and-space)
    ((and openp (flex-autopair-get-bounds 'sexp)) . bounds)
    ;; Skip self.
    ((and closep flex-autopair-skip-self
          (eq (char-after) last-command-event)) . skip)
    ((and closep) . self)
    ;; Insert matching pair.
    ((and openp
          (eq syntax ?\()
          (memq major-mode flex-autopair-lisp-mode)) . space-and-pair)
    (openp . pair)
    ;; self-insert-command is default
    (t . self)
    )
;;   "Alist of conditions"
  )

;; (defcustom flex-autopair-alias
(setq flex-autopair-alias
      '((self . (call-interactively 'self-insert-command))
        (bounds . (flex-autopair-wrap-region (car bounds)
                                                  (cdr bounds)
                                                  opener closer))
        (bounds-and-space . (progn
                              (flex-autopair-wrap-region (car bounds)
                                                              (cdr bounds)
                                                              opener closer)
                              (insert " ")
                              (backward-char 1)))
        (skip . (forward-char 1))
        (pair . (progn (call-interactively 'self-insert-command)
                       (save-excursion
                         (insert closer))))
        (space-and-pair . (progn (flex-autopair-smart-insert-space)
                                 (call-interactively 'self-insert-command)
                                 (save-excursion
                                   (insert closer)))))
;;   "Alist of function alias"
  )

(defun flex-autopair (syntax)
  (let*
      ((closer (if (eq syntax ?\()
                   (cdr (or (assq last-command-event flex-autopair-pairs)
                            (aref (syntax-table) last-command-event)))
                 last-command-event))
       (opener (if (eq syntax ?\))
                   (cdr (or (assq last-command-event flex-autopair-pairs)
                            (aref (syntax-table) last-command-event)))
                 last-command-event))
       (openp (flex-autopair-openp syntax))
       (closep (not openp))
       (bounds))
    (catch 'break
      (mapc (lambda (x)
              (when (setq bounds (eval (car x)))
                (message "%s" (cdr x))
                (eval (cdr (assq (cdr x) flex-autopair-alias)))
                (throw 'break t))
              ) flex-autopair-conditions)
      ))
  )

(defun flex-autopair-post-command-function ()
  (interactive)
  (let* ((syntax (and (eq (char-before) last-command-event) ; Sanity check.
                      flex-autopair-mode
                      (let ((x (assq last-command-event
                                     flex-autopair-pairs)))
                        (cond
                         (x (if (eq (car x) (cdr x)) ?\" ?\())
                         ((rassq last-command-event flex-autopair-pairs)
                          ?\))
                         (t (char-syntax last-command-event)))))))
    (cond ((memq syntax '(?\) ?\( ?\" ?\$))
           (undo-boundary)
           (delete-backward-char 1)
           (flex-autopair syntax)))
    ))

;; post-self-insert-hook is emacs 24 hook
;;;###autoload
(define-minor-mode flex-autopair-mode
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
  (if flex-autopair-mode
      ;; (add-hook 'post-self-insert-hook
      (add-hook 'post-command-hook
                #'flex-autopair-post-command-function)
    ;; (remove-hook 'post-self-insert-hook
    (remove-hook 'post-command-hook
                 #'flex-autopair-post-command-function)))

(dont-compile
  (when(fboundp 'expectations)
    (flex-autopair-mode 1)
    ;; (transient-mark-mode t)
    (expectations
      (desc "flex-autopair")
      (expect '("()" 2)
        (with-temp-buffer
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (call-interactively 'flex-autopair-post-command-function)
          (list (buffer-string) (point))
          ))
      (expect '("a\"\"" 3)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "a")
          (setq last-command-event ?\")
          (call-interactively 'self-insert-command)
          (call-interactively 'flex-autopair-post-command-function)
          (list (buffer-string) (point))
          ))
      (expect '("a ()" 4)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "a")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (call-interactively 'flex-autopair-post-command-function)
          (list (buffer-string) (point))
          ))
      (expect t
        (with-temp-buffer
          (emacs-lisp-mode)
          (font-lock-mode)
          (insert "a")
          (flex-autopair-openp ?\")
          ))
      ;; because of flex-autopair-openp bug
      ;; (expect nil
      ;;   (with-temp-buffer
      ;;     (emacs-lisp-mode)
      ;;     (font-lock-mode 1)
      ;;     (setq font-lock-support-mode 'jit-lock-mode)
      ;;     ;; (sit-for 1)
      ;;     (insert "\"a")
      ;;     ;; (get-text-property (1- (point)) 'face)
      ;;     (flex-autopair-openp ?\")
      ;;     ))
      ;; (expect nil
      ;;   (with-temp-buffer
      ;;     (emacs-lisp-mode)
      ;;     (insert "\"a ")
      ;;     (flex-autopair-openp ?\")
      ;;     ))
      ;; (expect '("\"a\"" 4)
      ;;   (with-temp-buffer
      ;;     (emacs-lisp-mode)
      ;;     (insert "\"a")
      ;;     (setq last-command-event ?\")
      ;;     (call-interactively 'self-insert-command)
      ;;     (call-interactively 'flex-autopair-post-command-function)
      ;;     (list (buffer-string) (point))
      ;;     ))
      (expect '("a()" 3)
        (with-temp-buffer
          (insert "a")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (call-interactively 'flex-autopair-post-command-function)
          (list (buffer-string) (point))
          ))
      (expect '("'()" 3)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "'")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (call-interactively 'flex-autopair-post-command-function)
          (list (buffer-string) (point))
          ))
      (expect '("(())" 3)
        ;; (expect '("()" 2)
        (with-temp-buffer
          (insert "(")
          (save-excursion
            (insert ")"))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (list (buffer-string) (point))
          ))
      (desc "region")
      (expect "(word)"
        (with-temp-buffer
          (set-mark (point))
          (insert "word")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string))
          )
      (expect "(word)"
        (with-temp-buffer
          (set-mark (point))
          (insert "word")
          (exchange-point-and-mark)
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "word)"
        (with-temp-buffer
          (set-mark (point))
          (insert "word")
          (setq last-command-event ?\))
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "((word))"
        (with-temp-buffer
          (save-excursion
            (insert "(word)"))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "( (word))"
        (with-temp-buffer
          (emacs-lisp-mode)
          (save-excursion
            (insert "(word)"))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "\"(word)\""
        (with-temp-buffer
          (save-excursion
            (insert "(word)"))
          (setq last-command-event ?\")
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "(http://example.com/index.html)"
        (with-temp-buffer
          (emacs-lisp-mode)
          (save-excursion
            (insert "http://example.com/index.html"))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "(\"http://example.com/index.html\")"
        (with-temp-buffer
      (emacs-lisp-mode)
          (save-excursion
            (insert "(http://example.com/index.html)"))
          (goto-char 2)
          (setq last-command-event ?\")
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "( \"http://example.com/index.html\")"
        (with-temp-buffer
          (emacs-lisp-mode)
          (save-excursion
            (insert "\"http://example.com/index.html\""))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "( \"word\")"
        (with-temp-buffer
          (emacs-lisp-mode)
          (save-excursion
            (insert "\"word\""))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "(\"word\")"
        (with-temp-buffer
          (save-excursion
            (insert "\"word\""))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "\"word\""
        (with-temp-buffer
          (set-mark (point))
          (insert "word")
          (setq last-command-event ?\")
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "\\("
        (with-temp-buffer
          (insert "\\")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "\\\\ ()"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\\\\")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect "\\\\()"
        (with-temp-buffer
          (insert "\\\\")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          (buffer-string)
          ))
      (expect '("()" 3)
        (with-temp-buffer
          (insert "(")
          (save-excursion
            (insert ")"))
          (setq last-command-event ?\))
          (call-interactively 'self-insert-command)
          (flex-autopair-post-command-function)
          ;; (buffer-substring-no-properties (point-min) (point-max))
          (list (buffer-string) (point)))
          ))
      ))

;; https://github.com/jixiuf/joseph-autopair/blob/master/joseph-autopair.el
;; auto pair with newline
;; after change functions hook
;; delete pair

;; http://code.google.com/p/autopair/
;; auto wrap region
;; blink
;; skip white space?
;; don't pair in comment
;; skip
;; escape quote

;; electric-pair-mode
;; standard for 24
;; pair from syntax table
;; auto wrap region

;; acp.el
;; http://d.hatena.ne.jp/buzztaiki/20061204/1165207521
;; http://d.hatena.ne.jp/kitokitoki/20090823/p1
;; custumizable
;; auto wrap symbol

;; pair like comment /**/

(provide 'flex-autopair)
;;; flex-autopair.el ends here
