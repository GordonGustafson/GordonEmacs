
(activate-input-method "english-dvorak")
(setq default-input-method "english-dvorak")
(add-hook 'minibuffer-setup-hook (lambda () (set-input-method "english-dvorak")))

(defadvice switch-to-buffer (after activate-input-method-after-switch activate)
  "Activate the dvorak input-method after switching to any buffer"
  (activate-input-method "english-dvorak"))



; EVIL DVORAK CUSTOMIZATIONS

; use dvorak when you press R in command mode
(add-hook 'evil-replace-state-entry-hook (lambda () (activate-input-method "english-dvorak")))

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

(defadvice evil-search-forward (around use-dvorak-input-method activate)
  (evil-insert-state)
  (activate-input-method "english-dvorak")
  ad-do-it
  (evil-normal-state))

(defadvice evil-search-backward (around use-dvorak-input-method activate)
  (evil-insert-state)
  (activate-input-method "english-dvorak")
  ad-do-it
  (evil-normal-state))


(defun execute-kbd-macro-no-input-method (rep)
  (add-hook 'evil-insert-state-entry-hook 'deactivate-input-method)
  (deactivate-input-method)
  (let ((old-default-input-method default-input-method))
    (setq default-input-method nil)
    (unwind-protect
      (execute-kbd-macro rep)
      (progn
        (setq default-input-method old-default-input-method)
        (remove-hook 'evil-insert-state-entry-hook 'deactivate-input-method)))))

(require 'evil-repeat)

(defun evil-execute-repeat-info (repeat-info)
  "Executes a repeat-information REPEAT-INFO."
  (evil-save-repeat-info
    (dolist (rep repeat-info)
      (cond
       ((or (arrayp rep) (stringp rep))
        (execute-kbd-macro-no-input-method rep))
       ((consp rep)
        (apply (car rep) (cdr rep)))
       (t
        (error "Unexpected repeat-info: %S" rep))))))


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



; SHELL MODE DVORAK CUSTOMIZATIONS

(defadvice shell (after switch-to-dvorak activate)
  (activate-input-method "english-dvorak"))



; ESHELL DVORAK CUSTOMIZATIONS

(defadvice eshell (after switch-to-dvorak activate)
  (activate-input-method "english-dvorak"))



; CALC MODE DVORAK CUSTOMIZATIONS

(require 'calc)

(define-key calc-mode-map "}" 'calc-plus)
(define-key calc-mode-map "'" 'calc-minus)
(define-key calc-mode-map "[" 'calc-divide)
(define-key calc-mode-map "q" 'calc-algebraic-entry)
