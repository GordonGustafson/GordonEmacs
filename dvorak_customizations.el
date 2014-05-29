; Gordon's customizations for using the dvorak keyboard layout with Emacs

(activate-input-method "english-dvorak")
(setq default-input-method "english-dvorak")
(add-hook 'minibuffer-setup-hook (lambda () (set-input-method "english-dvorak")))

; this runs whenever a new buffer is created in non-fundamental mode
(add-hook 'after-change-major-mode-hook (lambda ()
                                          (set-input-method "english-dvorak")))



; EVIL DVORAK CUSTOMIZATIONS

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



; CALC MODE DVORAK CUSTOMIZATIONS

(require 'calc)

(define-key calc-mode-map "}" 'calc-plus)
(define-key calc-mode-map "'" 'calc-minus)
(define-key calc-mode-map "[" 'calc-divide)
(define-key calc-mode-map "q" 'calc-algebraic-entry)
