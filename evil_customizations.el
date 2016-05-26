;; Gordon's customizations for evil, a Vim emulation layer for Emacs

(setq evil-want-C-u-scroll t) ; can usually use "4 X" instead of "C-u X"

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(require 'evil-anzu)
(evil-mode 1)

;; C-u in insert-state behaves like in bash (delete to beginning of line)
(define-key evil-insert-state-map (kbd "C-u") (lambda () (interactive)
                                                (kill-line 0)))
(define-key minibuffer-local-map (kbd "C-u") (lambda () (interactive)
                                                (kill-line 0)))

(define-key gordon-global-mode-map (kbd "C-w") evil-window-map)
(define-key evil-window-map (kbd "q")   'evil-window-delete)
(define-key evil-window-map (kbd "C-q") 'evil-window-delete)
(define-key evil-window-map (kbd "C-h") 'evil-window-left)
(define-key evil-window-map (kbd "C-j") 'evil-window-down)
(define-key evil-window-map (kbd "C-k") 'evil-window-up)
(define-key evil-window-map (kbd "C-l") 'evil-window-right)
(define-key evil-window-map (kbd "C-=") 'balance-windows)
(define-key evil-window-map (kbd "C--") 'evil-window-decrease-height)

(require 'org)
(require 'cl)

;; when point on foo in foo-bar, make */# search for foo-bar instead of just foo
(setq-default evil-symbol-word-search t)
(setq evil-cross-lines t)            ; f and t find characters in other lines
(setq evil-auto-balance-windows nil) ; C-w commands don't equalize window sizes

(defadvice other-window (after switch-to-normal-state-in-non-calc-buffers activate)
  "Switch evil to normal state in the new buffer unless it is in calc-mode"
  (if (eq major-mode 'calc-mode)
      (evil-insert-state)
    (evil-normal-state)))

(defun gordon-other-window (&optional n)
  "Select next window. Numeric prefix arg of 4 (C-u) selects previous window, but all other prefixes work normally."
  (interactive "P")
  (let ((numeric-prefix-arg (prefix-numeric-value n)))
    (if (equal numeric-prefix-arg 4)
        (other-window (- 1))
      (other-window numeric-prefix-arg))))

(evil-define-operator evil-yank-end-of-line (beg end type register)
  "Saves until end of line into the kill-ring."
  :motion nil
  :move-point nil
  (interactive "<R><x>")
  (evil-yank (point) (line-end-position) nil register))

(defmacro evil-without-repeat-prefix-arg (&rest body)
  "Defines an anonymous evil command that is not added to the repeat ring
and takes a numeric prefix argument COUNT."
  (let ((command-name (make-symbol "anonymous-command-name")))
    `(evil-define-command ,command-name (count)
       :repeat nil
       (interactive "p")
       ,@body)))

(evil-declare-ignore-repeat 'evil-delete-char)
(evil-declare-ignore-repeat 'evil-delete-backward-char)

(evil-declare-repeat 'evil-scroll-column-left)
(evil-declare-repeat 'evil-scroll-column-right)
(evil-declare-repeat 'evil-scroll-left)
(evil-declare-repeat 'evil-scroll-right)

;; (define-key evil-normal-state-map "K" 'other-window)
;; hack to stop ever calling evil-lookup (it still gets called sometimes even after K is remapped)
(substitute-key-definition 'evil-lookup 'gordon-other-window evil-motion-state-map)

(define-key gordon-global-mode-map  (kbd "<f5>") 'evil-local-mode)
(define-key evil-normal-state-map "Y" 'evil-yank-end-of-line)
(define-key evil-normal-state-map (kbd "<backspace>") 'evil-delete-backward-char)
(define-key evil-normal-state-map " "
  (evil-without-repeat-prefix-arg (when (not (looking-at "\n")) (forward-char))
                                  (self-insert-command count) (backward-char)))
(define-key evil-normal-state-map (kbd "S-<SPC>")
  (evil-without-repeat-prefix-arg (insert " ") (backward-char count)))
;; The choice of RET instead of <return> is significant here ("RET" gets overriden
;; by magit's text-property keymaps but <return> does not). I suspect this is because
;; there are many representations of the enter key (<return>, RET, \n, \r), and they
;; are looked up in a specific order.
(define-key evil-normal-state-map (kbd "RET")
  (evil-without-repeat-prefix-arg (loop repeat count do (evil-insert-newline-below))))
(define-key evil-normal-state-map (kbd "S-<return>")
  (evil-without-repeat-prefix-arg (loop repeat count do (evil-insert-newline-above))))

(define-key evil-normal-state-map "gI" (lambda () (interactive)
                                         (save-excursion
                                           (indent-region (point-min) (point-max) nil))))
(define-key evil-normal-state-map "gr" 'revert-buffer)
(define-key evil-normal-state-map "gt" 'toggle-truncate-lines)

(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(define-key evil-normal-state-map "ga" 'align-regexp)

(defun multi-occur-in-all-buffers (regexp)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(define-key evil-normal-state-map (kbd "g/") 'multi-occur-in-all-buffers)


(defun in-org-or-orgtbl-mode ()
  (or (and (boundp 'orgtbl-mode) (symbol-value 'orgtbl-mode))
      (eq major-mode 'org-mode)))

(evil-define-command evil-tab ()
  "Calls the most appropriate tab function for current mode and cursor position"
  :repeat nil
  (interactive)
  (cond
   ((and (in-org-or-orgtbl-mode) (org-at-table-p 'any))
    (org-cycle))
   ((memq major-mode '(calc-mode shell-mode eshell-mode magit-mode magit-status-mode))
    (call-interactively (local-key-binding "\t")))
   ((and (memq major-mode '(org-mode)) (not (eq evil-state 'insert)))
    (call-interactively (local-key-binding "\t")))
   ;; Make '<s' tab-complete to a SRC block
   ((org-try-structure-completion))
   (t
    (let ((bol-to-point (buffer-substring-no-properties (line-beginning-position) (point))))
      (if (string-match "^[ \t]*$" bol-to-point)
          (indent-for-tab-command)
        (call-interactively 'dabbrev-expand))))))

(define-key evil-insert-state-map (kbd "<tab>") 'evil-tab)
(define-key evil-normal-state-map (kbd "<tab>") 'evil-tab)
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map (kbd "<tab>") 'evil-tab)))

(evil-define-motion gordon-evil-ret-insert-state (count)
  "Calls most appropriate <enter> function for current mode and cursor position"
  :type line
  (cond ((and (in-org-or-orgtbl-mode) (org-at-table-p 'any))
         (org-return))
        ((evil-in-comment-p)
         (funcall comment-line-break-function))
        (t
         (evil-ret-and-indent count))))

(define-key evil-insert-state-map (kbd "RET") 'gordon-evil-ret-insert-state)

(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)

(evil-ex-define-cmd "W[rite]" 'evil-write)   ; add CAPITAL versions of commonly used ex commands
(evil-ex-define-cmd "S[ubstitute]" 'evil-ex-substitute)

; C-b bound to move-beginning-of-line by default
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)


(evil-select-search-module 'evil-search-module 'evil-search)

;; Ensure that ESC will exit just about anything. Otherwise you need to press it three times.
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


(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(define-and-bind-text-object "$" "\\$" "\\$")
(define-and-bind-text-object "|" "|" "|")
(define-and-bind-text-object "=" "\\(^\\|=\\) *" " *\\(=\\|$\\)")



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
  "Use calc package to evaluate a formula in an evil region."
  (when (eq evil-visual-selection 'line)
    (evil-visual-expand-region t))    ;move point and mark to ends of selected region, excluding newline

  (let ((original-beg (region-beginning)))
    (goto-char (region-end))   ; insert $ after end of region
    (if (eolp)
        (end-of-line)
      (forward-char))
    (insert "$")

    (goto-char original-beg)   ; ...and before beginning of region
    (insert "$"))

  (calc-embedded nil)    ; perform calculation on $ delimited region
  (calc-embedded nil)    ; quit calc-embedded mode
  (delete-char -1)       ; delete starting $
  (evil-find-char 1 ?$)
  (delete-char 1)        ; delete ending $
  (setq buffer-undo-list (remove* nil buffer-undo-list :count 2))) ; remove excess undo markers we created

(define-key evil-visual-state-map "m" 'gordon-evil-calc-evaluate)
