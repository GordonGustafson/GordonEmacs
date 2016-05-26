;; Gordon's customizations for various Emacs modes
(require 'evil)
(require 'cl)

;; LATEX CUSTOMIZATIONS

(setq-default TeX-PDF-mode t)  ;use .pdf for previews instead of .dvi

(setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
(setq TeX-view-program-selection '((output-pdf "Evince")))

(setq preview-image-type 'pnm) ;solves error: preview-image-type setting 'png unsupported by this Emacs
(setq preview-gs-options '("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")) ;switch to NOSAFER is need to make preview work????

(setq TeX-parse-self t) ; parse file on load in order to apply style hooks
(setq TeX-auto-save t)  ; save that parsed data in 'auto' folder when saving

;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("latexmk" "latexmk -pvc -pdf -f %s" TeX-run-TeX nil t
               :help "Run Latexmk on file")
             TeX-command-list)))

(setq TeX-command-default "latexmk")

(defface font-latex-verbatim-face
  (let ((font (if (and (assq :inherit custom-face-attributes))
                  '(:family "consolas"))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown" ,@font))
      (((class color) (background dark))
       (:foreground "burlywood" ,@font))
      (t (,@font))))
  "Face used to highlight TeX verbatim environments."
  :group 'font-latex-highlighting-faces)


(eval-after-load 'latex
  '(progn
     ;; tell Auctex not to mess with proof tree indentation
     (add-to-list 'LaTeX-indent-environment-list '("prooftree" current-indentation))

     (define-key LaTeX-mode-map (kbd "C-c e") 'LaTeX-environment)))

;; use LaTeX-environment to implement an text object for the current environment
(evil-define-text-object outer-latex-environment (count &optional beg end type)
  (save-excursion
    ;; LaTeX-mark-environment won't select the environment foo when point is
    ;; on the '\' in '\begin{foo}', so make sure we move past the '\begin{'
    (when (string-match (thing-at-point 'line) "\\\\begin{")
      (next-line))
    (LaTeX-mark-environment count)
    `(,(region-beginning) ,(region-end))))

(define-key evil-outer-text-objects-map "\\" 'outer-latex-environment)

(defmacro save-excursion-by-preceding-text (&rest body)
  "Like save-excursion, but restores point based on line number and the text preceding it, ignoring any whitespace changes.
Necessary because save-excursion doesn't work when text is replaced by shell-command-on-region."
  (interactive "")
  `(let* ((original-line (line-number-at-pos))
          (point-to-bol-string (buffer-substring-no-properties (line-beginning-position) (point)))
          (point-to-bol-regex (replace-regexp-in-string " *" " *" point-to-bol-string)))
     ,@body
     (goto-line original-line)
     (re-search-forward point-to-bol-regex)))

(defmacro command-on-all-delimited-regions (move-to-start-form move-to-end-form command)
  "Runs command on every region delimited by the results of move-to-start-form and move-to-end-form.

To determine bounds of a region, move to (point-min), invoke move-to-start-form, then invoke move-to-end-form.
Inside command, start and end will be bound to the results of those forms.
Terminate when move-to-start-form returns nil."
  (interactive "")
  `(save-excursion
     (while (progn (goto-char (point-min)) ,move-to-start-form)
       (let ((start (point-marker)))
         ,move-to-end-form
         (let ((end (point-marker)))
           ,command)))))

(defun format-latex-align ()
  "Adds basic latex math markup to all regions between @@@ and @@"
  (interactive "")
  (command-on-all-delimited-regions
   (prog1
       (re-search-forward "^@@@" (point-max) t)
     (goto-char (match-beginning 0)))
   (progn
     (next-line)           ;skip over @@@ since it contains @@
     (when (re-search-forward "^@@[^@]?" (point-max) t)
       (goto-char (match-end 0))))
   (progn
     (shell-command-on-region start end "bash ~/.emacs.d/format_latex_math.sh" nil t))))

(defun orgtbl-export-table-to-matrix (start end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (previous-line)
    ;; Emacs calc won't accept a matrix with \\ after the last row.
    ;; To accomodate this, put a marker after the second-to-last row of of the
    ;; matrix so we can perform a replace-regexp that excludes the last row.
    (let ((point-above-end (point-marker)))
      (goto-char start)
      (insert "\\begin{bmatrix}\n")
      (previous-line)
      (delete-indentation) ; merge current line with previous line
      (goto-char end)
      (insert "\n\\end{bmatrix}")
      (replace-regexp "^[ \t]*| " "" nil start end)
      (replace-regexp "|[^|]*$" "\\\\\\\\" nil start point-above-end)
      (replace-regexp "|[^|]*$" "" nil point-above-end end) ; remove final |
      (replace-regexp "|" "&" nil start end))))

(defun orgtbl-export-all-tables-to-matrices ()
  (interactive "")
  (command-on-all-delimited-regions
   (prog1
       (re-search-forward "^[ \t]*|" (point-max) t)   ;go to the next line of a table in buffer
     (goto-char (match-beginning 0)))
   (progn
     (while (string-match "^[ \t]*|" (thing-at-point 'line)) ;while we are on a table line
       (next-line))                                          ;move forward one line
     (search-backward "|")
     (forward-char))                                         ;then move after the last | in the table
   (progn
     (orgtbl-export-table-to-matrix start end))))

;; Taken 'verbatim' from http://tex.stackexchange.com/questions/186605/
(defun orgtbl-to-latex-verbatim (table params)
  "Convert the Orgtbl mode TABLE to LaTeX."
  (let* ((alignment (mapconcat (lambda (x) (if x "r" "l"))
                               org-table-last-alignment ""))
         (params2
          (list
           :tstart (concat "\\begin{tabular}{" alignment "}")
           :tend "\\end{tabular}"
           :lstart "" :lend " \\\\" :sep " & "
           :efmt "%s\\,(%s)" :hline "\\hline")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))



;; ORG-MODE CUSTOMIZATIONS

(setq default-major-mode 'org-mode)
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode)) ;open txt files in org-mode

(global-set-key (kbd "C-|") 'org-table-create-or-convert-from-region)

;; Gold provides a nice contrast with everything but level-1 headings, which are
;; already hard to confuse with level-3 headings.
(set-face-attribute 'org-level-3 nil ':foreground "gold")

(setq org-support-shift-select t)
(setq org-startup-folded 'showall) ;show everything on startup
(setq org-startup-truncated nil)   ;don't wrap lines
(setq org-log-done nil)            ; don't insert a timestamp when a task is marked as finished
;; allow "-*- mode: org; org-log-done: t -*-" in a file to set org-log-done to t:
(setq safe-local-variable-values (cons '(org-log-done . t) safe-local-variable-values))

(setq org-M-RET-may-split-line nil)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook (lambda ()
                           (org-indent-mode t)
                           ;; org-mode has a very unpredictable definition of
                           ;; paragraph-start, so just use a form feed or line
                           ;; filled with whitespace (the global default)
                           (setq paragraph-start "\f\\|[ 	]*$")
                           (setq paragraph-separate "[ 	\f]*$")))

(setq org-directory "~/org") ;setting not used often by org

(setq org-agenda-file-regexp ".*\\.org")    ;include all org files in listed directories
(setq org-agenda-files (list "~/org"
                             "~/Dropbox/orgmode"))

(setq org-mobile-checksum-binary (or (executable-find "shasum")
                                     (executable-find "sha1sum")
                                     (executable-find "md5sum")
                                     (executable-find "md5")))

(setq org-mobile-inbox-for-pull "~/org/from-mobile.org")
(setq org-mobile-directory "~/Dropbox/orgmode")



;; ORG LATEX CUSTOMIZATIONS

(require 'ox-latex) ; required so we can set org-latex-default-packages-alist

(define-key org-mode-map (kbd "C-c e") 'LaTeX-environment)

;; remove this font package because it provides a definition for iint that conflicts with amsmath (causes error)
(setq org-latex-default-packages-alist (delete '("" "wasysym" t) org-latex-default-packages-alist))

(setq org-export-async-debug t)



;; DESKTOP CUSTOMIZATIONS

(require 'desktop)

(setq desktop-save nil)
(setq desktop-restore-eager 6) ; restore 6 buffers now and the rest when idle
(desktop-save-mode 0)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)



;; OPENWITH CUSTOMIZATIONS

(when (require 'openwith nil 'noerror)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp '("mpg" "mpeg" "mp3" "flac" "m4a" "mp4" "avi" "wmv"
                                                 "wav" "mov" "flv" "ogm" "ogg" "mkv")) "vlc" '(file))
         (list (openwith-make-extension-regexp '("png" "gif" "jpeg" "jpg"))  "iceweasel" '(file))
         (list (openwith-make-extension-regexp '("pdf")) "evince" '(file))
         (list (openwith-make-extension-regexp '("doc" "docx" "rtf" "odt")) "libreoffice" '(file))))
  (openwith-mode t))

(setq large-file-warning-threshold 250000000) ; confirm if larger than 250 Mb



;; DIRED MODE CUSTOMIZATIONS

(require 'dired-x)
(setq-default dired-omit-mode t)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))   ; hide dot-files

(put 'dired-find-alternate-file 'disabled nil)

;; a non-recursive copy is pretty much making a new directory,
;; and I don't use hardlinks often enough to need non-recursive deletes
(setq dired-recursive-copies 'always) ; "always" means no asking
(setq dired-recursive-deletes 'always)

(setq dired-listing-switches "-alhv")    ; h = human-readable file sizes, v = sort numerically

;; evil-integration contains a similar version of this.
;; if you have any trouble rebinding things, see if that's what's causing it.
(eval-after-load 'dired
  '(progn
     ;; Ensure we don't override certain bindings from evil.
     ;; Unbinding them here is easier than rebinding to their old values.
     (define-key dired-mode-map "g" nil)
     (define-key dired-mode-map "G" nil)
     (define-key dired-mode-map "n" nil)
     (define-key dired-mode-map "N" nil)
     (define-key dired-mode-map "?" nil)
     (define-key dired-mode-map "." nil)

     ;; dired-do-delete makes it easy to delete the wrong thing, especially if you
     ;; mark something and later try to delete the file under the cursor with D).
     ;; Just use d x instead
     (define-key dired-mode-map "D" nil)

     ;; r renames to current directory (defined later), R renames to other dired window
     (define-key dired-mode-map "R" (lambda (_)
                                      (interactive "p") ; commands needs to be interactive to be bound to a key
                                      (let ((dired-dwim-target t))
                                        (call-interactively 'dired-do-rename))))

     ;; c copies to current directory (defined later), C copies to other dired window
     (define-key dired-mode-map "C" (lambda (_)
                                      (interactive "p") ; commands needs to be interactive to be bound to a key
                                      (let ((dired-dwim-target t))
                                        (call-interactively 'dired-do-copy))))

     (evil-make-overriding-map dired-mode-map 'normal t)
     (evil-add-hjkl-bindings dired-mode-map 'normal
       "H" (lambda () (interactive) (evil-window-top 2)  (evil-end-of-line))
       "M" (lambda () (interactive) (evil-window-middle) (evil-end-of-line))
       "L" (lambda () (interactive) (evil-window-bottom) (evil-previous-line) (evil-end-of-line))
       "K" 'gordon-other-window
       (kbd "<return>") 'dired-find-file
       ;; evil-integration overrides "r", so we can't define this in dired-mode-map:
       "r" 'dired-do-rename
       "c" 'dired-do-copy)))



;; TERM MODE CUSTOMIZATIONS

(eval-after-load 'term
  '(progn
     (define-key term-raw-map (kbd "M-x") 'nil) ; M-x should go to Emacs

     ;; map C-<backspace> to send M-<backspace> so bash deletes the previous word
     (define-key term-raw-map (kbd "C-<backspace>") 'term-send-raw-meta)))


(delete 'term-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'term-mode)



;; SHELL MODE CUSTOMIZATIONS

(require 'shell)

(defun open-shell-buffer-by-number (shell-buffer-number)
  "Switch to or create a shell in the buffer named *shell*<shell-buffer-number>"
  (interactive "P")
  (if (null shell-buffer-number)
      (setq shell-buffer-number 1))
  (let* ((shell-buffer-name (format "*shell*<%d>" shell-buffer-number)))
    (shell shell-buffer-name)))

(define-key      gordon-global-mode-map (kbd "C-z") 'open-shell-buffer-by-number)
(define-key              shell-mode-map (kbd "C-z") 'open-shell-buffer-by-number)
(evil-define-key 'normal shell-mode-map (kbd "C-z") 'open-shell-buffer-by-number)
(evil-set-toggle-key "<f6>") ; unbinds C-z in evil


(defun scroll-to-location-if-not-visible (buffer location)
  (let ((window-displaying-buf (get-buffer-window buffer (selected-frame))))
    ;; When `buffer` is visible but `location` in `buffer` is not
    (when (and window-displaying-buf
               (not (pos-visible-in-window-p location window-displaying-buf)))
      (set-window-start win location))))

(defun rerun-last-command-in-shell-by-number (shell-buffer-number)
  (interactive "P")
  (if (null shell-buffer-number)
      (setq shell-buffer-number 1))
  (let ((shell-buffer-name (format "*shell*<%d>" shell-buffer-number)))
    (set-buffer shell-buffer-name)
    (scroll-to-location-if-not-visible shell-buffer-name (point-max))
    ;; The rest of this functions simulates user actions that would re-execute
    ;; the last command. Hopefully there's a better way, but I'll admit I didn't
    ;; look for one. This relies on the `comint-previous-input` putting the
    ;; cursor at the end of the input. This is a reasonable assumption, but it's
    ;; gross to rely on where the point is.
    (goto-char (point-max))  ; `end-of-buffer` docs say to use this instead
    (comint-previous-input 1)
    (comint-send-input)))

(define-key evil-normal-state-map "S" 'rerun-last-command-in-shell-by-number)


;; use the same binding as bash. C-? is bound to redo if you need it.
(define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward-regexp)
(define-key shell-mode-map (kbd "M-r") nil)

(define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
(define-key shell-mode-map (kbd "C-n") 'comint-next-input)
(define-key shell-mode-map (kbd "M-p") nil)
(define-key shell-mode-map (kbd "M-n") nil)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq dirtrack-list '("^\\(?:(.*?)\\)\\(.*\\)\\$" 1)) ; assume PS1=\w$
(add-hook 'shell-mode-hook (lambda () (dirtrack-mode 1)))

(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input 'this) ; move to prompt when typing
(setq comint-input-ignoredups t)              ; no duplicates in command history

(evil-define-key 'normal shell-mode-map (kbd "<return>") 'comint-send-input)
(evil-define-key 'normal shell-mode-map (kbd "C-d")
  (lookup-key evil-motion-state-map (kbd "C-d"))) ; fix shell's stupid binding

;; execute my .bashrc in *shell*
(setq explicit-bash-args (append '("--login" "--init-file" "~/.bashrc") explicit-bash-args))



;; ESHELL CUSTOMIZATIONS

(setq eshell-scroll-to-bottom-on-input 'this) ; move to prompt when typing

(eshell-command "alias sudo '*sudo $*'")      ; eshell's sudo often doesn't work

(add-hook 'eshell-mode-hook
          (lambda ()
            (evil-define-key 'normal eshell-mode-map (kbd "<return>") 'eshell-send-input)))



;; MAGIT CUSTOMIZATIONS

(setq magit-last-seen-setup-instructions "1.4.0")
(require 'magit)

(let ((evil-magit-mode-maps '(magit-mode-map magit-cherry-mode-map
                              magit-diff-mode-map magit-log-mode-map
                              magit-log-select-mode-map magit-process-mode-map
                              magit-reflog-mode-map magit-refs-mode-map
                              magit-revision-mode-map magit-stash-mode-map
                              magit-stashes-mode-map magit-status-mode-map)))
  (loop for mode-map-symbol  in evil-magit-mode-maps do
        (let ((mode-map (symbol-value mode-map-symbol)))
          (evil-define-key 'normal mode-map
            (kbd "j")   (lookup-key evil-motion-state-map "j")
            (kbd "k")   (lookup-key evil-motion-state-map "k")
            (kbd "H")   (lookup-key evil-motion-state-map "H")
            (kbd "M")   (lookup-key evil-motion-state-map "M")
            (kbd "L")   (lookup-key evil-motion-state-map "L")
            (kbd "g")   (lookup-key evil-motion-state-map "g")
            (kbd "G")   (lookup-key evil-motion-state-map "G")
            (kbd "V")   (lookup-key evil-motion-state-map "V")
            (kbd "gr")  'magit-refresh
            (kbd "/")   (lookup-key evil-motion-state-map "/")
            (kbd "?")   (lookup-key evil-motion-state-map "?")
            (kbd "n")   (lookup-key evil-motion-state-map "n")
            (kbd "N")   (lookup-key evil-motion-state-map "N")
            (kbd ".")   (lookup-key evil-normal-state-map ".")
            (kbd "i")   'magit-mark-item)
          (evil-make-overriding-map mode-map 'normal t))))

(evil-define-key 'normal magit-status-mode-map (kbd "C-k") 'magit-delete-thing)
(evil-define-key 'normal magit-status-mode-map (kbd "J") (lookup-key magit-status-mode-map "j"))


(let ((evil-magit-modes '(magit-cherry-mode magit-diff-mode
                          magit-log-mode magit-log-select-mode
                          magit-process-mode magit-reflog-mode
                          magit-refs-mode magit-revision-mode
                          magit-stash-mode magit-stashes-mode
                          magit-status-mode)))
  (setq evil-emacs-state-modes (remove-if (lambda (mode)
                                            (memq mode evil-magit-modes))
                                          evil-emacs-state-modes)))

(define-key magit-mode-map (kbd "1") nil)
(define-key magit-mode-map (kbd "2") nil)
(define-key magit-mode-map (kbd "3") nil)
(define-key magit-mode-map (kbd "4") nil)

(define-key magit-mode-map (kbd "C-1") 'magit-section-show-level-1)
(define-key magit-mode-map (kbd "C-2") 'magit-section-show-level-2)
(define-key magit-mode-map (kbd "C-3") 'magit-section-show-level-3)
(define-key magit-mode-map (kbd "C-4") 'magit-section-show-level-4)

(global-set-key (kbd "C-x g") 'magit-status)

;; remove unnecessary bindings from git-rebase-mode:
(setq git-rebase-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map text-mode-map)
        (define-key map (kbd "C-c C-c") 'server-edit)
        (define-key map (kbd "C-c C-k") 'git-rebase-abort)
        map))

(evil-define-key 'normal git-rebase-mode-map (kbd "<return>") 'git-rebase-show-commit)

(add-hook 'git-rebase-mode-hook (lambda () (read-only-mode -1)))



;; DSVN CUSTOMIZATIONS

(autoload 'svn-status "dsvn" "Run `svn status'." t)

(eval-after-load 'dsvn
  '(progn
     (let ((evil-dsvn-mode-maps '(svn-status-mode-map svn-log-mode-map)))
       (loop for mode-map-symbol in evil-dsvn-mode-maps do
             (let ((mode-map (symbol-value mode-map-symbol)))
               (evil-make-overriding-map mode-map 'normal t)
               (evil-define-key 'normal  mode-map
                 (kbd "j") (lookup-key evil-motion-state-map "j")
                 (kbd "k") (lookup-key evil-motion-state-map "k")
                 (kbd "d") 'svn-diff-file))))

     (evil-define-key 'normal svn-log-mode-map (kbd "<return>") 'svn-log-show-diff)
     (evil-define-key 'normal svn-status-mode-map (kbd "<return>") 'svn-find-file)))

(global-set-key (kbd "C-x s") 'svn-status) ; override save-some-buffers


;; DIFF-MODE CUSTOMIZATIONS

(evil-define-key 'normal diff-mode-map
  (kbd "q") 'quit-window
  (kbd "<return>") 'diff-goto-source)



;; AUTO-COMPLETE CUSTOMIZATIONS

;; setup completion sources when entering appropriate major modes:
(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(global-auto-complete-mode)

;; (setq ac-use-fuzzy nil)
(setq ac-auto-start 5)
(setq ac-delay .6)
(setq ac-auto-show-menu 1.3)



;; SMEX CUSTOMIZATIONS

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)



;; IDO CUSTOMIZATIONS

(require 'ido)
(ido-mode t)



;; FIND-FILE-IN-PROJECT CUSTOMIZATIONS

(require 'find-file-in-project)

(defun gordon-find-file (use-find-file-in-project)
  (interactive "P")
  (if use-find-file-in-project
      (find-file-in-project)
    (ido-find-file)))

(global-set-key (kbd "C-x f")   'gordon-find-file)
(global-set-key (kbd "C-x C-f") 'gordon-find-file)

(setq ffip-limit 8192) ; This is why you buy RAM! (default is 512)

;; They forgot some. If things get too slow you may want to trim this variable.
;; Use add-to-list so these won't be added twice if they are added upstream.
(add-to-list 'ffip-patterns "*.tex")
(add-to-list 'ffip-patterns "*.css")
(add-to-list 'ffip-patterns "*.cs")
(add-to-list 'ffip-patterns "*.java")

(add-to-list 'ffip-prune-patterns "bin") ; don't find files in any bin directory

(setq ffip-project-file '(".git" "GTAGS")) ; keep a GTAGS file in project root


;; OCCUR CUSTOMIZATIONS

(setq evil-emacs-state-modes (remove 'occur-mode evil-emacs-state-modes))

(evil-define-key 'normal occur-mode-map
  (kbd "<return>") 'occur-mode-goto-occurrence
  (kbd "C-o")      'occur-mode-display-occurrence
  (kbd "C-x C-q")  'occur-edit-mode)



;; GGTAGS (GNU GLOBAL) CUSTOMIZATIONS

(require 'ggtags)
(add-hook 'prog-mode-hook (lambda () (ggtags-mode 1)))

;; Finds the definition if point is on a reference and vice versa.
;; HOWEVER, gnu global doesn't know about references when using the
;; ctags backend, so finding all refences only works in C, C++, and Java.
(define-key evil-motion-state-map "\C-]" 'ggtags-find-tag-dwim)
(define-key evil-insert-state-map "\C-]" 'ggtags-find-tag-dwim)

(defun ggtags-find-tag-dwim-other-window ()
  "Like `ggtags-find-tag-dwim', but put buffer in another window.
Only intended for interactive use."
  (interactive)
  (let (value)
    (switch-to-buffer-other-window
     (save-window-excursion
       (setq value (call-interactively 'ggtags-find-tag-dwim))
       (unless (or (bufferp value) (bufferp (car-safe value)))
         (setq value (current-buffer)))
       (current-buffer)))
    value))

(define-key evil-window-map (kbd "C-]") 'ggtags-find-tag-dwim-other-window)



;; COMPILATION-MODE CUSTOMIZATIONS

(define-key compilation-mode-map "g" nil)

;; gnu-global-mode derives from compilation-mode
(evil-define-key 'normal compilation-mode-map
  "gr" 'recompile
  "h" (lookup-key evil-motion-state-map "h")
  "?" (lookup-key evil-motion-state-map "?")
  "0" 'evil-digit-argument-or-evil-beginning-of-line
  (kbd "<return>") 'compile-goto-error
  (kbd "<tab>")    'compilation-next-error)




;; RAINBOW-DELIMITERS CUSTOMIZATIONS

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)



;; DTRT MODE CUSTOMIZATIONS

(require 'dtrt-indent)
(dtrt-indent-mode t)



;; WHITESPACE MODE CUSTOMIZATIONS

(require 'whitespace)

(setq whitespace-style '(face trailing indentation empty
                              space-before-tab space-after-tab))
(add-hook 'prog-mode-hook (lambda ()
                            (dtrt-indent-try-set-offset)
                            (whitespace-mode t))) ; reactivate whitespace-mode to pick up any change in settings



;; GENERAL PROGRAMMING CUSTOMIZATIONS

(setq-default c-basic-offset 4)
(setq-default tab-width 4)

;; consider whitespace or * to be valid line prefixes when filling
;; paragraphs. * can be a line prefix in javadoc comments. This variable
;; only applies when the paragraph being filled starts as one line.
(setq adaptive-fill-first-line-regexp "\\`[ \\t*]*\\'")



;; ESS CUSTOMIZATIONS

(eval-after-load 'ess-mode
  '(ess-toggle-underscore nil))



;; OCTAVE CUSTOMIZATIONS

; .m files are opened in objc-mode by default
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))



;; C MODE CUSTOMIZATIONS

;; Make M-; produce C99 comments
(add-hook 'c-mode-hook (lambda () (setq comment-start "//" comment-end "")))



;; C-SHARP (C#) MODE CUSTOMIZATIONS

(setq auto-mode-alist (cons '( "\\.cs\\'" . csharp-mode) auto-mode-alist) )



;; VERILOG MODE CUSTOMIZATIONS

(setq verilog-indent-level 3)
(setq verilog-auto-newline nil)
(setq verilog-tab-always-indent nil)
(setq verilog-linter "verilator --lint-only ")

(add-hook 'verilog-mode-hook
          (lambda ()
            (define-key verilog-mode-map (kbd "C-c C-c") 'verilog-auto-save-compile)))



;; GRAPHVIZ DOT MODE CUSTOMIZATIONS

(setq graphviz-dot-auto-indent-on-newline nil)
(setq graphviz-dot-auto-indent-on-semi nil)



;; ARTIST MODE CUSTOMIZATIONS

(defadvice artist-mode (after deactive-evil-for-artist-mode activate)
  (if artist-mode
      (turn-off-evil-mode)
    (turn-on-evil-mode)))



;; CALC MODE CUSTOMIZATIONS

(require 'calc)
(require 'calc-ext)

(setq calc-display-trail nil) ;don't display trail buffer by default

;;remap common operators to dvorak
(define-key calc-mode-map "}" 'calc-plus)
(define-key calc-mode-map "'" 'calc-minus)
(define-key calc-mode-map "[" 'calc-divide)
(define-key calc-mode-map "q" 'calc-algebraic-entry)

;; need to remap K to switch to other buffers because I ensure
;; that evil switches to insert state after entering a calc buffer (see evil_customizations)
(define-key calc-mode-map "K" 'other-window)

;; use the latex calc language when the buffer is in org-mode (calc embedded)
(add-to-list 'calc-language-alist '(org-mode . latex))

(add-to-list 'evil-insert-state-modes 'calc-mode)



;; W3M CUSTOMIZATIONS

(setq evil-emacs-state-modes (remove 'w3m-mode evil-emacs-state-modes))

(setq browse-url-browser-function 'w3m-browse-url)
(setq w3m-use-cookies t)
(setq w3m-home-page "http://www.stackoverflow.com/questions")
(setq w3m-search-default-engine "g")
;; don't use the word at point as the default search text
;; (you have to delete it if you want to search something else)
(setq w3m-search-word-at-point nil)
(add-hook 'w3m-mode-hook 'w3m-lnum-mode)

;; w3m-lnum's default hint color isn't great with dark-blue color theme
(face-spec-set 'w3m-lnum '((t (:foreground "#FF9999"))) )

(defun w3m-new-tab ()
  (interactive)
  (w3m-copy-buffer nil nil nil t))

(defun w3m-browse-url-new-tab (url &optional new-session)
  (interactive)
  (w3m-new-tab)
  (w3m-browse-url url))

(defun w3m-follow-hint-new-tab ()
  (interactive)
  ;; prefix argument opens a new session
  ;; how are sessions different from tabs????
  ;; should w3m-new-tab use sessions somehow????
  (w3m-lnum-follow 4))

(defun w3m-search-new-tab (search-engine query)
  (interactive (w3m-search-read-variables))
  (w3m-new-tab)
  (w3m-search-do-search 'w3m-goto-url search-engine query))

(eval-after-load "w3m-search"
  '(progn
     ;; C-u S g RET <search term> RET
     (add-to-list 'w3m-search-engine-alist '("g" "http://www.google.com/search?q=%s" utf-8))
     (add-to-list 'w3m-search-engine-alist '("q" "http://stackoverflow.com/search?q=%s" utf-8))
     (add-to-list 'w3m-search-engine-alist '("w" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8))))

(eval-after-load 'w3m-lnum
  '(progn
     (defvar w3m-mode-map)
     (evil-make-overriding-map w3m-lnum-mode-map 'normal t)
     (evil-make-overriding-map w3m-mode-map 'normal t)
     (evil-define-key 'normal w3m-mode-map
       "o" 'w3m-search
       "t" 'w3m-search-new-tab
       ;;  "f" 'w3m-view-this-url  w3m-lnum handles this
       "F" 'w3m-follow-hint-new-tab
       "b" 'evil-backward-word-begin
       (kbd "<backspace>") 'w3m-view-previous-page
       "gg" 'evil-goto-first-line
       "G" 'evil-goto-line
       "d" 'w3m-delete-buffer
       "h" 'w3m-previous-buffer
       "j" 'w3m-scroll-up
       "k" 'w3m-scroll-down
       "l" 'w3m-next-buffer
       "H" 'w3m-view-previous-page
       "K" 'other-window
       "L" 'w3m-view-next-page
       (kbd "C-e") 'w3m-scroll-up
       (kbd "C-y") 'w3m-scroll-down)))



;; MAIL CUSTOMIZATIONS

(global-set-key (kbd "C-x i") (lambda ()
                                (interactive)
                                (gnus)
                                (gnus-group-read-group 100 t "INBOX")
                                (evil-normal-state)
                                (gnus-summary-insert-new-articles)))

(defadvice gnus (after switch-to-normal-state activate)
  (evil-normal-state))

(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnimap-authinfo-file "~/.authinfo")))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "gordon3.14@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq send-mail-function 'message-send-mail-function)

(setq user-mail-address "gordon3.14@gmail.com")
(setq user-full-name "Gordon Gustafson")
(setq gnus-use-full-window nil)

;; always fetch all the messages in a newsgroup.
;; The prompt for how many to fetch can take longer to answer than the actual fetching would
(setq gnus-large-newsgroup nil)

;; always read the backup file of group interactions if it exists (it's there in case Emacs crashes before the updates are applied)
;; Disabled so I have one less prompty to answer
(setq gnus-always-read-dribble-file t)

(eval-after-load 'gnus-msg
  '(progn
     ;; use the standard gnus-summary-mode bindings as a base
     (evil-make-overriding-map gnus-summary-mode-map 'normal t)
     (evil-add-hjkl-bindings gnus-summary-mode-map  'normal
       ;; "h"                        ;overrides gnus-summary-select-article-buffer
       ;; "j"                        ;overrides gnus-summary-goto-article
       ;; "k"                        ;overrides gnus-summary-kill-same-subject-and-select
       ;; "l"                        ;overrides gnus-summary-goto-last-article
       "K" 'other-window             ;overrides gnus-summary-mime-map
       "H" 'evil-window-top          ;overrides gnus-summary-help-map
       "M" 'evil-window-middle       ;overrides gnus-summary-mark-map
       "L" 'evil-window-bottom)))    ;overrides gnus-summary-lower-score
