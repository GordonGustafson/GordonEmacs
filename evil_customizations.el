;this file contains Gordon's customizations for EVIL

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(require 'cl)

(setq evil-cross-lines t) ;allow f and t commands to find characters beyond the current line

(evil-define-motion evil-insert-from-normal-mode (count)
  "inserts the last character typed from normal mode."
  (interactive "p") ;make count the numerical prefix argument
  (if (not (looking-at "\n"))
    (forward-char))
  (self-insert-command count)
  (backward-char))

;; (defadvice other-window (after switch-to-normal-state activate)
;;   (evil-normal-state))


(defadvice other-window (after switch-to-normal-state-in-non-calc-buffers activate)
  "Switch evil to normal state in the new buffer unless it is in calc-mode"
  (if (eq major-mode 'calc-mode)
     (evil-insert-state)
     (evil-normal-state))) 

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(evil-define-operator evil-yank-end-of-line (beg end type register)
  "Saves until end of line into the kill-ring."
  :motion nil
  :move-point nil
  (interactive "<R><x>")
  (evil-yank (point) (line-end-position) nil register))

(define-key evil-normal-state-map " " 'evil-insert-from-normal-mode)
;(define-key evil-normal-state-map "K" 'other-window)
;hack to stop ever calling evil-lookup (it still gets called sometimes even after K is remapped)
(substitute-key-definition 'evil-lookup 'other-window evil-motion-state-map)
 
(global-set-key (kbd "<f5>") 'evil-local-mode)
(define-key evil-normal-state-map (kbd "C-S-K") 'other-window-backward)
(define-key evil-normal-state-map "Y" 'evil-yank-end-of-line)
(define-key evil-normal-state-map "gm" 'evil-middle-of-visual-line)
(define-key evil-normal-state-map (kbd "<backspace>") 'evil-delete-backward-char)
(define-key evil-normal-state-map (kbd "<return>") (lambda (count) (interactive "p") (evil-open-below count) (evil-normal-state)))

; making a decision based on the major mode is kind of hacky, but calc mode
; should be the only time I'll want to modify the behavior of tab in insert mode
(define-key evil-insert-state-map (kbd "<tab>")
  (lambda (count)
    (interactive "p")
    (if (eq major-mode 'calc-mode)
      (calc-roll-down 2))
      (let ((bol-to-point (buffer-substring-no-properties (line-beginning-position) (point))))
        (if (string-match "^[ \t]*$" bol-to-point)
          (insert-char ?\s tab-width)
          (evil-complete-next)))))

(define-key evil-insert-state-map (kbd "C-p") 'previous-line) 
(define-key evil-insert-state-map (kbd "C-n") 'next-line) 
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line) 
(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)
 
(evil-ex-define-cmd "W[rite]" 'evil-write)   ; add CAPITAL versions of commonly used ex commands
(evil-ex-define-cmd "S[ubstitute]" 'evil-ex-substitute)

; Ensure that ESC will exit just about anything. Otherwise you need to press it three times.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)



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

(defadvice evil-find-char (before use-current-input-method activate)
  (ad-set-arg 1 (qwerty-to-dvorak (ad-get-arg 1))))

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

(defadvice self-insert-command (before foo)
  "This advice is activated by execute-kbd-macro-no-input-method in order to disable the use of any input methods."
  (deactivate-input-method))

(defun execute-kbd-macro-no-input-method (rep)
  (add-hook 'evil-insert-state-entry-hook 'deactivate-input-method)
  (ad-activate 'self-insert-command)
  (deactivate-input-method)
  (execute-kbd-macro rep)
  (remove-hook 'evil-insert-state-entry-hook 'deactivate-input-method)
  (ad-deactivate 'self-insert-command))


(evil-define-text-object evil-inner-dollar (count &optional beg end type)
  (evil-inner-object-range count beg end type
    (lambda (&optional arg) (re-search-forward "\\$" nil t 1) (backward-char))
    (lambda (&optional arg) (re-search-backward "\\$" nil t 1) (forward-char))))

(define-key evil-inner-text-objects-map "$" 'evil-inner-dollar)

(evil-define-text-object evil-outer-dollar (count &optional beg end type)
  (evil-inner-object-range count beg end type
    (lambda (&optional arg) (re-search-forward "\\$" nil t 1))
    (lambda (&optional arg) (re-search-backward "\\$" nil t 2))))

(define-key evil-outer-text-objects-map "$" 'evil-outer-dollar)
 
(defun distance-to-next-match (regex)
  "Returns the next position of regex in buffer without moving point.
   Returns nil if regex does not appear in buffer."
  (save-excursion
    (re-search-forward regex (point-max) t)))

(defconst gordon-matching-delimiters `( ("(" . ")")
                                      ("\\[" . "\\]")
                                        ("{" . "}")))

(defun get-next-delimiter (delims)
  "Returns the next delimeter in delims and its position in the buffer in a list"
  (let ((next-delim nil))
    (let ((final-position
          (reduce (lambda (old-closest-match delim-regex)
                  (let ((current-match (distance-to-next-match delim-regex)))
                    (if (null current-match)
                      (progn 
                        (delq delim-regex delims)
                        old-closest-match)
                      (if (< current-match old-closest-match)
                        (progn
                          (setq next-delim (match-string-no-properties 0))
                          current-match)
                        old-closest-match))))
                 delims :initial-value (point-max))))
  (cons next-delim final-position))))

(defun first-matching-predicate (predicate sequence)
  (let ((only-matching-items (remove-if-not predicate sequenc)))
    (car only-matching-items)))

(defun check-matching-delimiters ()
  (interactive)
  (goto-char (point-min))
  (let* ((delim-stack '())
        (open-delims (mapcar #'car gordon-matching-delimiters))
       (close-delims (mapcar #'cdr gordon-matching-delimiters))
        (all-delims (append open-delims close-delims))
        (current-delim)
        (current-delim-position)
        (last-position))
    (while t
      (destructuring-bind (current-delim . current-delim-position) (get-next-delimiter all-delims)
      (when (null current-delim) ;we've searched the whole buffer
        (if (null delim-stack)   ;if there are no unmatched delimeters
          (return)               
          (progn
            (message "File ended before all delimiters were matched")
            (goto-char last-position))))
      (if (member current-delim open-delims)
        (progn
          (setq delim-stack (cons current-delim delim-stack))
          (setq last-position current-delim-position))
        (if (member current-delim close-delims)
          (if (equal (position (car delim-stack) open-delims) (position current-delim close-delims))
            (setq delim-stack (cdr delim-stack))
            (progn 
              (message (format "Unmatched delimiters %s and %s" (car delim-stack) (current-delim)))
              (goto-char current-delim-position)))
          (message "Error is check-matching-delimiters: got a delimeter that does not match any known opening or closing delimeters"))))
    (message "All delimiters match. YAY!"))))
          

  ;;   (Let ((next-delim-location (point-max))
  ;;         (next-delim))
  ;;   (save-excursion 
  ;;     (loop for (start-delim . end-delim) in gordon-matching-delimiters do
  ;;       (let ((next-open-delim-location (re-search-forward start-delim))
  ;;            (next-close-delim-location (re-search-forward end-delim)))

  ;;         (when (< (next-open-delim-location) (next-delim-location))
  ;;           (setq next-delim-location next-open-delim-location)
  ;;           (setq next-delim-location next-open-delim-location)
            
  ;;       )
  ;; )))))
  ;;       minimizing (min (re-search-forward start-delim) (re-search-forward end-delim)) into next-delim
  ;;       finally (progn
  ;;                 (goto-char (- next-delim 1))
  ;;                 (message (match-string 0))

(define-key evil-normal-state-map "U" 'check-matching-delimiters)



(defun newline-at-end-of-sentences (from to)
  (interactive (progn
    (barf-if-buffer-read-only)
    (if (use-region-p)
      (list (region-beginning) (region-end))
      (list (line-beginning-position) (line-end-position)))))
  (save-excursion
    (goto-char from)
    (while (< (point) to)
      (forward-sentence)
      (unless (= (char-after (1+ (point))) ?\n)
        (forward-char)
        (insert "\n")))))

(define-key evil-normal-state-map "Q" 'newline-at-end-of-sentences)


(evil-define-operator evil-delete-into-null-register (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Do not save it in any register."
  (interactive "<R><x><y>")
  (evil-delete beg end type ?_ yank-handler))

(define-key evil-normal-state-map "X" 'evil-delete-into-null-register)



(evil-define-operator gordon-evil-calc-evaluate (beg end type)
  "Use calc package to evaluate current region as a formula"
    (calc-embedded 0)
    (calc-embedded nil)) ;quit calc-embedded mode

(define-key evil-visual-state-map "e" 'gordon-evil-calc-evaluate)
