;; Not necessarily bug fixes per se, but 'patches' so things work the way I want.
;; A few massive redifinitions here are easier to keep track of than
;; small tweaks spread across the entire lisp directory.

(require 'ispell)

(defun ispell-command-loop (miss guess word start end)
  "Display possible corrections from list MISS.
GUESS lists possibly valid affix construction of WORD.
Returns nil to keep word.
Returns 0 to insert locally into buffer-local dictionary.
Returns string for new chosen word.
Returns list for new replacement word (will be rechecked).
  Query-replace when list length is 2.
  Automatic query-replace when second element is `query-replace'.
Highlights the word, which is assumed to run from START to END.
Global `ispell-pdict-modified-p' becomes a list where the only value
indicates whether the dictionary has been modified when option `a'
or `i' is used.
Global `ispell-quit' set to start location to continue spell session."
  (let ((count ?0)
    (line ispell-choices-win-default-height)
    ;; ensure 4 context lines.
    (max-lines (- (ispell-adjusted-window-height) 4))
    (choices miss)
    (window-min-height (min window-min-height
                ispell-choices-win-default-height))
    (command-characters '( ?  ?i ?a ?A ?r ?R ?? ?x ?X ?q ?l ?u ?m ))
    (dedicated (window-dedicated-p (selected-window)))
    (skipped 0)
    char num result textwin dedicated-win)

    ;; setup the *Choices* buffer with valid data.
    (with-current-buffer (get-buffer-create ispell-choices-buffer)
      (setq mode-line-format
        (concat
             "--  %b  --  word: " word
             "  --  dict: " (or ispell-current-dictionary "default")
             "  --  prog: " (file-name-nondirectory ispell-program-name)))
      ;; XEmacs: no need for horizontal scrollbar in choices window
      (ispell-with-no-warnings
       (and (fboundp 'set-specifier)
        (boundp 'horizontal-scrollbar-visible-p)
        (set-specifier horizontal-scrollbar-visible-p nil
               (cons (current-buffer) nil))))
      (erase-buffer)
      (if guess
      (progn
        (insert "Affix rules generate and capitalize "
            "this word as shown below:\n\t")
        (while guess
          (if (> (+ 4 (current-column) (length (car guess)))
             (window-width))
          (progn
            (insert "\n\t")
            (setq line (1+ line))))
          (insert (car guess) "    ")
          (setq guess (cdr guess)))
        (insert "\nUse option `i' to accept this spelling and put it in your private dictionary.\n")
        (setq line (+ line (if choices 3 2)))))
      (while (and choices
          (< (if (> (+ 7 (current-column) (length (car choices))
                   (if (> count ?~) 3 0))
                (window-width))
             (progn
               (insert "\n")
               (setq line (1+ line)))
               line)
             max-lines))
    ;; not so good if there are over 20 or 30 options, but then, if
    ;; there are that many you don't want to scan them all anyway...
    (while (memq count command-characters) ; skip command characters.
      (setq count (ispell-int-char (1+ count))
        skipped (1+ skipped)))
    (insert "(" count ") " (car choices) "  ")
    (setq choices (cdr choices)
          count (ispell-int-char (1+ count))))
      (setq count (ispell-int-char (- count ?0 skipped))))

    ;; ensure word is visible
    (if (not (pos-visible-in-window-p end))
    (sit-for 0))

    ;; allow temporary split of dedicated windows...
    (if dedicated
    (progn
      (setq dedicated-win (selected-window))
      (set-window-dedicated-p dedicated-win nil)))

    ;; Display choices for misspelled word.
    (ispell-show-choices line end)
    (select-window (setq textwin (next-window)))

    ;; highlight word, protecting current buffer status
    (unwind-protect
    (progn
      (and ispell-highlight-p
           (ispell-highlight-spelling-error start end t))
      ;; Loop until a valid choice is made.
      (while
          (eq
           t
           (setq
        result
        (progn
          (undo-boundary)
          (let (message-log-max)
            (message (concat "C-h or ? for more options; SPC to leave "
                     "unchanged, Character to replace word")))
          (let ((inhibit-quit t)
            (input-valid t))
            (setq char nil skipped 0)
            ;; If the user types C-g, or generates some other
            ;; non-character event (such as a frame switch
            ;; event), stop ispell.  As a special exception,
            ;; ignore mouse events occurring in the same frame.
            (while (and input-valid (not (characterp char)))
              (setq char (read-key))
              (setq input-valid
                (or (characterp char)
                (and (mouse-event-p char)
                     (eq (selected-frame)
                     (window-frame
                      (posn-window (event-start char))))))))
            (when (or quit-flag (not input-valid) (= char ?\C-g))
              (setq char ?X quit-flag nil)))
          ;; Adjust num to array offset skipping command characters.
          (let ((com-chars command-characters))
            (while com-chars
              (if (and (> (car com-chars) ?0) (< (car com-chars) char))
              (setq skipped (1+ skipped)))
              (setq com-chars (cdr com-chars)))
            (setq num (- char ?0 skipped)))

          (cond
           ((= char ? ) nil)    ; accept word this time only
           ((= char ?i)        ; accept and insert word into pers dict
            (ispell-send-string (concat "*" word "\n"))
            (setq ispell-pdict-modified-p '(t)) ; dictionary modified!
            nil)
           ((or (= char ?a) (= char ?A)) ; accept word without insert
            (ispell-send-string (concat "@" word "\n"))
            (add-to-list 'ispell-buffer-session-localwords word)
            (or ispell-buffer-local-name ; session localwords might conflict
            (setq ispell-buffer-local-name (buffer-name)))
            (if (null ispell-pdict-modified-p)
            (setq ispell-pdict-modified-p
                  (list ispell-pdict-modified-p)))
            (if (= char ?A) 0))    ; return 0 for ispell-add buffer-local
           ((or (= char ?r) (= char ?R)) ; type in replacement
            (and (eq 'block ispell-highlight-p) ; refresh tty's
             (ispell-highlight-spelling-error start end nil t))
            (let ((result
               (if (or (= char ?R) ispell-query-replace-choices)
                   (list (read-string
                      (format "Query-replacement for %s: " word)
                      ;; word)
                      ;; Gordon gordon chose to use this line instead of the previous one:
                      )
                     t)
                 ;(cons (read-string "Replacement for: " word)
                 ;; Gordon gordon chose to use this line instead of the previous one:
                 (cons (read-string "Replacement for: ")
                   nil))))
              (and (eq 'block ispell-highlight-p)
               (ispell-highlight-spelling-error start end nil
                                'block))
              result))
           ((or (= char ??) (= char help-char) (= char ?\C-h))
            (and (eq 'block ispell-highlight-p)
             (ispell-highlight-spelling-error start end nil t))
            (ispell-help)
            (and (eq 'block ispell-highlight-p)
             (ispell-highlight-spelling-error start end nil
                              'block))
            t)
           ;; Quit and move point back.
           ((= char ?x)
            (ispell-pdict-save ispell-silently-savep)
            (message "Exited spell-checking")
            (setq ispell-quit t)
            nil)
           ;; Quit and preserve point.
           ((= char ?X)
            (ispell-pdict-save ispell-silently-savep)
            (message "%s"
             (substitute-command-keys
              (concat "Spell-checking suspended;"
                  " use C-u \\[ispell-word] to resume")))
            (setq ispell-quit start)
            nil)
           ((= char ?q)
            (if (y-or-n-p "Really kill Ispell process? ")
            (progn
              (ispell-kill-ispell t) ; terminate process.
              (setq ispell-quit (or (not ispell-checking-message)
                        (point))
                ispell-pdict-modified-p nil))
              t))        ; continue if they don't quit.
           ((= char ?l)
            (and (eq 'block ispell-highlight-p) ; refresh tty displays
             (ispell-highlight-spelling-error start end nil t))
            (let ((new-word (read-string
                     "Lookup string (`*' is wildcard): "
                     word)))
              (if new-word
              (progn
                (with-current-buffer (get-buffer-create
                                                  ispell-choices-buffer)
                  (erase-buffer)
                  (setq count ?0
                    skipped 0
                    mode-line-format ;; setup the *Choices* buffer with valid data.
                    (concat "--  %b  --  word: " new-word
                        "  --  word-list: "
                        (or ispell-complete-word-dict
                        ispell-alternate-dictionary))
                    miss (lookup-words new-word)
                    choices miss
                    line ispell-choices-win-default-height)
                  (while (and choices ; adjust choices window.
                      (< (if (> (+ 7 (current-column)
                               (length (car choices))
                               (if (> count ?~) 3 0))
                            (window-width))
                         (progn
                           (insert "\n")
                           (setq line (1+ line)))
                           line)
                         max-lines))
                (while (memq count command-characters)
                  (setq count (ispell-int-char (1+ count))
                    skipped (1+ skipped)))
                (insert "(" count ") " (car choices) "  ")
                (setq choices (cdr choices)
                      count (ispell-int-char (1+ count))))
                  (setq count (ispell-int-char
                       (- count ?0 skipped))))
                (ispell-show-choices line end)
                (select-window (next-window)))))
            (and (eq 'block ispell-highlight-p)
             (ispell-highlight-spelling-error start end nil
                              'block))
            t)            ; reselect from new choices
           ((= char ?u)        ; insert lowercase into dictionary
            (ispell-send-string (concat "*" (downcase word) "\n"))
            (setq ispell-pdict-modified-p '(t)) ; dictionary modified!
            nil)
           ((= char ?m)        ; type in what to insert
            (ispell-send-string
             (concat "*" (read-string "Insert: " word) "\n"))
            (setq ispell-pdict-modified-p '(t))
            (cons word nil))
           ((and (>= num 0) (< num count))
            (if ispell-query-replace-choices ; Query replace flag
            (list (nth num miss) 'query-replace)
              (nth num miss)))
           ((= char ?\C-l)
            (redraw-display) t)
           ((= char ?\C-r)
            ;; This may have alignment errors if current line is edited
            (if (marker-position ispell-recursive-edit-marker)
            (progn
              (message "Only one recursive edit session supported")
              (beep)
              (sit-for 2))
              (set-marker ispell-recursive-edit-marker start)
              ;;(set-marker ispell-region-end reg-end)
              (and ispell-highlight-p        ; unhighlight
               (ispell-highlight-spelling-error start end))
              (unwind-protect
              (progn
                (message
                 "%s"
                 (substitute-command-keys
                  (concat "Exit recursive edit with"
                      " \\[exit-recursive-edit]")))
                (save-window-excursion (save-excursion (recursive-edit))))
            ;; protected
            (goto-char ispell-recursive-edit-marker)
            (if (not (equal (marker-buffer
                     ispell-recursive-edit-marker)
                    (current-buffer)))
                (progn
                  (set-marker ispell-recursive-edit-marker nil)
                  (error
                   "Cannot continue ispell from this buffer.")))
            (set-marker ispell-recursive-edit-marker nil)))
           (list word nil))
           ((= char ?\C-z)
            (funcall (key-binding "\C-z"))
            t)
           (t (ding) t))))))
      result)
      ;; protected
      (and ispell-highlight-p        ; unhighlight
       (save-window-excursion
         (select-window textwin)
         (ispell-highlight-spelling-error start end)))
      (if dedicated
      (set-window-dedicated-p dedicated-win t)))))









(require 'calc)


(defun calc (&optional arg full-display interactive)
  "The Emacs Calculator.  Full documentation is listed under \"calc-mode\"."
  (interactive "P\ni\np")
  (if arg
      (unless (eq arg 0)
    (require 'calc-ext)
    (if (= (prefix-numeric-value arg) -1)
        (calc-grab-region (region-beginning) (region-end) nil)
      (when (= (prefix-numeric-value arg) -2)
        (calc-keypad))))
    (when (get-buffer-window "*Calc Keypad*")
      (calc-keypad)
      (set-buffer (window-buffer (selected-window))))
    (if (eq major-mode 'calc-mode)
    (calc-quit)
      (let ((oldbuf (current-buffer)))
    (calc-create-buffer)
    (setq calc-was-keypad-mode nil)
    (if (or (eq full-display t)
        (and (null full-display) calc-full-mode))
        (switch-to-buffer (current-buffer) t)
      (if (get-buffer-window (current-buffer))
          (select-window (get-buffer-window (current-buffer)))
            (if calc-window-hook
                (run-hooks 'calc-window-hook)
              (let ((w (get-largest-window)))
                (if (and pop-up-windows
                         (> (window-height w)
                            (+ window-min-height calc-window-height 2)))
                    (progn
                      (setq w (split-window w
                                            (- (window-height w)
                                               calc-window-height 2)
                                            nil))
                      (set-window-buffer w (current-buffer))
                      (select-window w))
                  (pop-to-buffer (current-buffer)))))))
    (with-current-buffer (calc-trail-buffer)
      (and calc-display-trail
           (= (window-width) (frame-width))
           (calc-trail-display 1 t)))
    (message "Welcome to the GNU Emacs Calculator!  Press `?' or `h' for help, `q' to quit")
    (run-hooks 'calc-start-hook)
    (and (windowp full-display)
         (window-point full-display)
         (select-window full-display))
    (calc-check-defines)
    ;;Gordon gordon
    ;; (when (and calc-said-hello interactive)
    ;;  (sit-for 2)
    ;;  (message ""))
    (setq calc-said-hello t)))))













(require 'calc-embed)


(defun calc-embedded-make-info (point cbuf fresh &optional
                      calc-embed-top calc-embed-bot
                                      calc-embed-outer-top calc-embed-outer-bot)
  (let* ((bufentry (assq (current-buffer) calc-embedded-active))
     (found bufentry)
     (force (and fresh calc-embed-top (null (equal calc-embed-top '(t)))))
     (fixed calc-embed-top)
     (new-info nil)
     info str)
    (or found
        (and
         (setq found (list (current-buffer))
               calc-embedded-active (cons found calc-embedded-active)
               calc-embedded-firsttime-buf t)
         (let ((newann (assoc major-mode calc-embedded-announce-formula-alist))
               (newform (assoc major-mode calc-embedded-open-close-formula-alist))
               (newword (assoc major-mode calc-embedded-word-regexp-alist))
               (newplain (assoc major-mode calc-embedded-open-close-plain-alist))
               (newnewform
                (assoc major-mode calc-embedded-open-close-new-formula-alist))
               (newmode (assoc major-mode calc-embedded-open-close-mode-alist)))
           (when newann
             (make-local-variable 'calc-embedded-announce-formula)
             (setq calc-embedded-announce-formula (cdr newann)))
           (when newform
             (make-local-variable 'calc-embedded-open-formula)
             (make-local-variable 'calc-embedded-close-formula)
             (setq calc-embedded-open-formula (nth 0 (cdr newform)))
             (setq calc-embedded-close-formula (nth 1 (cdr newform))))
           (when newword
             (make-local-variable 'calc-embedded-word-regexp)
             (setq calc-embedded-word-regexp (nth 1 newword)))
           (when newplain
             (make-local-variable 'calc-embedded-open-plain)
             (make-local-variable 'calc-embedded-close-plain)
             (setq calc-embedded-open-plain (nth 0 (cdr newplain)))
             (setq calc-embedded-close-plain (nth 1 (cdr newplain))))
           (when newnewform
             (make-local-variable 'calc-embedded-open-new-formula)
             (make-local-variable 'calc-embedded-close-new-formula)
             (setq calc-embedded-open-new-formula (nth 0 (cdr newnewform)))
             (setq calc-embedded-close-new-formula (nth 1 (cdr newnewform))))
           (when newmode
             (make-local-variable 'calc-embedded-open-mode)
             (make-local-variable 'calc-embedded-close-mode)
             (setq calc-embedded-open-mode (nth 0 (cdr newmode)))
             (setq calc-embedded-close-mode (nth 1 (cdr newmode)))))))
    (while (and (cdr found)
        (> point (aref (car (cdr found)) 3)))
      (setq found (cdr found)))
    (if (and (cdr found)
         (>= point (aref (nth 1 found) 2)))
        (setq info (nth 1 found))
      (setq calc-embedded-firsttime-formula t)
      (setq info (make-vector 16 nil)
        new-info t
        fresh t)
      (aset info 0 (current-buffer))
      (aset info 1 (or cbuf (save-excursion
                  (calc-create-buffer)
                  (current-buffer)))))
    (if (and
         (or (integerp calc-embed-top) (equal calc-embed-top '(4)))
         (not calc-embed-bot))
                                        ; started with a user-supplied argument
    (progn
          (if (equal calc-embed-top '(4))
              (progn
                (aset info 2 (copy-marker (line-beginning-position)))
                (aset info 3 (copy-marker (line-end-position))))
            (if (= (setq calc-embed-arg (prefix-numeric-value calc-embed-arg)) 0)
                (progn
                  (aset info 2 (copy-marker (region-beginning)))
                  (aset info 3 (copy-marker (region-end))))
              (aset info (if (> calc-embed-arg 0) 2 3) (point-marker))
              (if (> calc-embed-arg 0)
                  (progn
                    (forward-line (1- calc-embed-arg))
                    (end-of-line))
                (forward-line (1+ calc-embed-arg)))
              (aset info (if (> calc-embed-arg 0) 3 2) (point-marker))))
      (aset info 4 (copy-marker (aref info 2)))
      (aset info 5 (copy-marker (aref info 3))))
      (if (aref info 4)
      (setq calc-embed-top (aref info 2)
        fixed calc-embed-top)
    (if (consp calc-embed-top)
            (progn
              (require 'thingatpt)
              (if (thing-at-point-looking-at calc-embedded-word-regexp)
                  (progn
                    (setq calc-embed-top (copy-marker (match-beginning 0)))
                    (setq calc-embed-bot (copy-marker (match-end 0)))
                    (setq calc-embed-outer-top calc-embed-top)
                    (setq calc-embed-outer-bot calc-embed-bot))
                (setq calc-embed-top (point-marker))
                (setq calc-embed-bot (point-marker))
                (setq calc-embed-outer-top calc-embed-top)
                (setq calc-embed-outer-bot calc-embed-bot)))
      (or calc-embed-top
          (calc-embedded-find-bounds 'plain)))
    (aset info 2 (copy-marker (min calc-embed-top calc-embed-bot)))
    (aset info 3 (copy-marker (max calc-embed-top calc-embed-bot)))
    (aset info 4 (copy-marker (or calc-embed-outer-top (aref info 2))))
    (aset info 5 (copy-marker (or calc-embed-outer-bot (aref info 3))))))
    (goto-char (aref info 2))
    (if new-info
    (progn
      (or (bolp) (aset info 7 t))
      (goto-char (aref info 3))
      (or (bolp) (eolp) (aset info 7 t))))
    (if fresh
    (let ((modes (calc-embedded-find-modes)))
      (aset info 12 (car modes))
      (aset info 13 (nth 1 modes))
      (aset info 14 (nth 2 modes))))
    (aset info 15 calc-embedded-globals)
    (setq str (buffer-substring (aref info 2) (aref info 3)))
    (if (or force
        (not (equal str (aref info 6))))
    (if (and fixed (aref info 6))
        (progn
          (aset info 4 nil)
          ;;gordon Gordon
          (forward-char)
          ;;end Gordon

          (calc-embedded-make-info point cbuf nil)
          (setq new-info nil))
      (let* ((open-plain calc-embedded-open-plain)
         (close-plain calc-embedded-close-plain)
         (pref-len (length open-plain))
         (calc-embed-vars-used nil)
         suff-pos val temp)
        (with-current-buffer (aref info 1)
          (calc-embedded-set-modes (aref info 15)
                       (aref info 12) (aref info 14))
          (if (and (> (length str) pref-len)
               (equal (substring str 0 pref-len) open-plain)
               (setq suff-pos (string-match (regexp-quote close-plain)
                            str pref-len)))
          (setq val (math-read-plain-expr
                 (substring str pref-len suff-pos)))
        (if (string-match "[^ \t\n]" str)
            (setq pref-len 0
              val (condition-case nil
                                  (math-read-big-expr str)
                                (error (math-read-expr str))))
          (setq val nil))))
        (if (eq (car-safe val) 'error)
        (setq val (list 'error
                (+ (aref info 2) pref-len (nth 1 val))
                (nth 2 val))))
        (aset info 6 str)
        (aset info 8 val)
        (setq temp val)
        (if (eq (car-safe temp) 'calcFunc-evalto)
        (setq temp (nth 1 temp))
          (if (eq (car-safe temp) 'error)
          (if new-info
              (setq new-info nil)
            (setcdr found (delq info (cdr found)))
            (calc-embedded-active-state 'less))))
        (aset info 9 (and (eq (car-safe temp) 'calcFunc-assign)
                  (nth 1 temp)))
        (if (memq (car-safe val) '(calcFunc-evalto calcFunc-assign))
        (calc-embedded-find-vars val))
        (aset info 10 calc-embed-vars-used)
        (aset info 11 nil))))
    (if new-info
    (progn
      (setcdr found (cons info (cdr found)))
      (calc-embedded-active-state 'more)))
    info))
