;; Electric pairing.

(defcustom electric-pair-dwim-pairs
  '((?\" . ?\"))
  ;; '(nil)
  "Alist of pairs that should be used regardless of major mode."
  :type '(repeat (cons character character)))

(defcustom electric-pair-dwim-skip-self t
  "If non-nil, skip char instead of inserting a second closing paren.
When inserting a closing paren character right before the same character,
just skip that character instead, so that hitting ( followed by ) results
in \"()\" rather than \"())\".
This can be convenient for people who find it easier to hit ) than C-f."
  :type 'boolean)

(defun electric-pair-dwim-wrap-region (beg end opener closer)
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

(defun electric-pair-dwim-comment-or-stringp (&optional pos)
  (setq pos (or pos (point)))
  (memq (get-text-property pos 'face)
        '(font-lock-comment-face font-lock-doc-face
                                 font-lock-string-face))
  ;;(not (memq (char-syntax (following-char)) '(?\" ?\')))
  )

(defun electric-pair-dwim-escapedp (&optional pos)
  (setq pos (or pos (point)))
  (save-excursion
    (goto-char pos)
    (not (zerop (% (skip-syntax-backward "\\") 2))))
  )

(defun electric-pair-dwim-get-bounds (symbol)
  (if (eq symbol 'region)
      (and (use-region-p)
           (if (< (point) (mark))
               `(,(point) . ,(mark))
             `(,(mark) . ,(point))))
    (let ((bounds
           (bounds-of-thing-at-point symbol)))
      (if (eq (car bounds) (point)) bounds nil)
      )))

(defun electric-pair-dwim-openp (syntax &optional pos)
  (setq pos (or pos (point)))
  (and (not (eq syntax ?\)))
       (or (eq syntax ?\();; '(?\( ?\" ?\$)
           (not (eq (get-text-property pos 'face) font-lock-string-face))
       )))

(defun electric-pair-dwim-smart-insert-space ()
  (unless (or (eq (char-syntax (preceding-char)) ? )
              (eq (char-syntax (preceding-char)) ?\( )
              (eq (char-syntax (preceding-char)) ?')
              (bolp))
    (insert " " )))

(setq electric-pair-dwim-lisp-mode
  '(lisp-mode emacs-lisp-mode lisp-interaction-mode
              inferior-gauche-mode scheme-mode)
  )

;; (defcustom electric-pair-dwim-conditions
(setq electric-pair-dwim-conditions
  '(((electric-pair-dwim-escapedp) . self)
    (overwrite-mode . self)
    ;; Wrap a pair.
    ((and openp (electric-pair-dwim-get-bounds 'region)) . bounds)
    ;; ((and openp (electric-pair-dwim-get-url)) . region);; symbol works better
    ((and openp (electric-pair-dwim-get-bounds 'symbol)) . bounds)
    ;; for lisp
    ((and openp
          (eq syntax ?\()
          (memq major-mode electric-pair-dwim-lisp-mode)
          (electric-pair-dwim-get-bounds 'sexp)) . bounds-and-space)
    ((and openp (electric-pair-dwim-get-bounds 'sexp)) . bounds)
    ;; Skip self.
    ((and closep electric-pair-dwim-skip-self
          (eq (char-after) last-command-event)) . skip)
    ;; Insert matching pair.
    ((and openp
          (memq major-mode electric-pair-dwim-lisp-mode)) . space-and-pair)
    (openp . pair)
    ;; self-insert-command is default
    (t . self)
    )
;;   "Alist of conditions"
  )

;; (defcustom electric-pair-dwim-alias
(setq electric-pair-dwim-alias
      '((self . (call-interactively 'self-insert-command))
        (bounds . (electric-pair-dwim-wrap-region (car bounds)
                                                  (cdr bounds)
                                                  opener closer))
        (bounds-and-space . (progn
                              (electric-pair-dwim-wrap-region (car bounds)
                                                              (cdr bounds)
                                                              opener closer)
                              (insert " ")
                              (backward-char 1)))
        (skip . (forward-char 1))
        (pair . (progn (call-interactively 'self-insert-command)
                       (save-excursion
                         (insert closer))))
        (space-and-pair . (progn (electric-pair-dwim-smart-insert-space)
                                 (call-interactively 'self-insert-command)
                                 (save-excursion
                                   (insert closer)))))
;;   "Alist of function alias"
  )

(defun electric-pair-dwim (syntax)
  (let*
      ((closer (if (eq syntax ?\()
                   (cdr (or (assq last-command-event electric-pair-dwim-pairs)
                            (aref (syntax-table) last-command-event)))
                 last-command-event))
       (opener (if (eq syntax ?\))
                   (cdr (or (assq last-command-event electric-pair-dwim-pairs)
                            (aref (syntax-table) last-command-event)))
                 last-command-event))
       (openp (electric-pair-dwim-openp syntax))
       (closep (not openp))
       (bounds))
    (catch 'break
      (mapc (lambda (x)
              (when (setq bounds (eval (car x)))
                (message "%s" (cdr x))
                (eval (cdr (assq (cdr x) electric-pair-dwim-alias)))
                (throw 'break t))
              ) electric-pair-dwim-conditions)
      ))
  )

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
    (cond ((memq syntax '(?\) ?\( ?\" ?\$))
           (undo-boundary)
           (delete-backward-char 1)
           (electric-pair-dwim syntax)))
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
    (electric-pair-dwim-mode 1)
    ;; (transient-mark-mode t)
    (expectations
      (desc "electric-pair-dwim")
      ;; (expect '("()" 2) ;; ??
      ;; (with-temp-buffer
      ;;     (setq last-command-event ?\()
      ;;     (call-interactively 'self-insert-command)
      ;;     (call-interactively 'electric-pair-dwim-post-command-function)
      ;;     (list (buffer-string) (point))
      ;;     ))
      (expect '("a ()" 4)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "a")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (call-interactively 'electric-pair-dwim-post-command-function)
          (list (buffer-string) (point))
          ))
      (expect '("a()" 3)
        (with-temp-buffer
          (insert "a")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (call-interactively 'electric-pair-dwim-post-command-function)
          (list (buffer-string) (point))
          ))
      (expect '("'()" 3)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "'")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (call-interactively 'electric-pair-dwim-post-command-function)
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
          (electric-pair-dwim-post-command-function)
          (list (buffer-string) (point))
          ))
      (desc "region")
      (expect "(word)"
        (with-temp-buffer
          (set-mark (point))
          (insert "word")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          (buffer-string))
          )
      (expect "(word)"
        (with-temp-buffer
          (set-mark (point))
          (insert "word")
          (exchange-point-and-mark)
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
      (expect "((word))"
        (with-temp-buffer
          (save-excursion
            (insert "(word)"))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          (buffer-string)
          ))
      (expect "( (word))"
        (with-temp-buffer
          (emacs-lisp-mode)
          (save-excursion
            (insert "(word)"))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          (buffer-string)
          ))
      (expect "\"(word)\""
        (with-temp-buffer
          (save-excursion
            (insert "(word)"))
          (setq last-command-event ?\")
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          (buffer-string)
          ))
      (expect "(http://example.com/index.html)"
        (with-temp-buffer
          (emacs-lisp-mode)
          (save-excursion
            (insert "http://example.com/index.html"))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
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
          (electric-pair-dwim-post-command-function)
          (buffer-string)
          ))
      (expect "( \"http://example.com/index.html\")"
        (with-temp-buffer
          (emacs-lisp-mode)
          (save-excursion
            (insert "\"http://example.com/index.html\""))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          (buffer-string)
          ))
      (expect "( \"word\")"
        (with-temp-buffer
          (emacs-lisp-mode)
          (save-excursion
            (insert "\"word\""))
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          (buffer-string)
          ))
      (expect "(\"word\")"
        (with-temp-buffer
          (save-excursion
            (insert "\"word\""))
          (setq last-command-event ?\()
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
          (buffer-string)
          ))
      (expect "\\("
        (with-temp-buffer
          (insert "\\")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          (buffer-string)
          ))
      (expect "\\\\ ()"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\\\\")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
          (buffer-string)
          ))
      (expect "\\\\()"
        (with-temp-buffer
          (insert "\\\\")
          (setq last-command-event ?\()
          (call-interactively 'self-insert-command)
          (electric-pair-dwim-post-command-function)
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

(provide 'electric-pair-dwim)
;;; electric-pair-dwim.el ends here
