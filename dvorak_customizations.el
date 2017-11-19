;; Gordon's customizations for using the dvorak keyboard layout with Emacs

(activate-input-method "english-dvorak")
(setq default-input-method "english-dvorak")
;; For some reason the minibuffer doesn't seem to use an input method unless
;; it's forced to like this.
;;
;; Recording macros with an input method active is broken at the *Emacs* level
;; (Evil has nothing to do with it). Evil uses the Emacs xxx-kbd-macro functions
;; to implement recording and playing macros with `q`, and playing macros with
;; `.`. If you use those functions directly with an input method active, you'll
;; see that keys inserted into the buffer that can be affected by the input
;; method are duplicated. For example, if you record a macro that inserts "fo"
;; in the buffer with an input method enabled, it will insert "ffoo" in the
;; buffer when played back (what is inserted will vary if the input method is
;; enabled when being played back, but the keys are duplicated either way).
;;
;; Since this has nothing to do with Evil, I'm not going to even try to fix it.
;; If I need to insert text in macros, I'll disable the input method before
;; recording the macro, and make sure it's disabled when I play back the macro.
;; `defining-kbd-macro` is true when defining a macro with `q`, and
;; `evil-in-single-undo` is true when playing back a macro with `@` or repeating
;; a command with `v` (evil-with-single-undo isn't used many other places
;; besides repeating).
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (set-input-method (if (or defining-kbd-macro evil-in-single-undo)
                                  nil
                                  "english-dvorak"))))

;; this runs whenever a new buffer is created in non-fundamental mode
(add-hook 'after-change-major-mode-hook (lambda ()
                                          (set-input-method "english-dvorak")))



;; EVIL DVORAK CUSTOMIZATIONS

(defun convert-second-element-to-integer (pair)
  (cons (nth 0 pair)
        (if (vectorp (nth 1 pair))
            (string-to-char (elt (nth 1 pair) 0))
          (nth 1 pair))))

(defun qwerty-to-dvorak (char)
  (let ((translation-map (mapcar `convert-second-element-to-integer (cdr (quail-map)))))
    (if (assoc char translation-map)
        (cdr (assoc char translation-map))
      char)))

(defadvice evil-replace (before use-current-input-method activate)
  (ad-set-arg 3 (qwerty-to-dvorak (ad-get-arg 3))))


(evil-define-motion evil-find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR."
  :jump t
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0)))
    (setq evil-last-find (list #'evil-find-char char fwd))
    (when fwd (forward-char))
    (let ((case-fold-search nil))
      (unless (prog1
                  (search-forward (char-to-string (qwerty-to-dvorak char))
                                  (unless evil-cross-lines
                                    (if fwd
                                        (line-end-position)
                                      (line-beginning-position)))
                                  t count)
                (when fwd (backward-char)))
        (error "Can't find %c" (qwerty-to-dvorak char))))))



;; CALC MODE DVORAK CUSTOMIZATIONS

(require 'calc)

(define-key calc-mode-map "}" 'calc-plus)
(define-key calc-mode-map "'" 'calc-minus)
(define-key calc-mode-map "[" 'calc-divide)
(define-key calc-mode-map "q" 'calc-algebraic-entry)
