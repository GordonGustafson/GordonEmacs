; Gordon's customizations for various Emacs modes
(require 'evil)
(require 'cl)

; LATEX CUSTOMIZATIONS

(setq-default TeX-PDF-mode t)  ;use .pdf for previews instead of .dvi

(setq preview-image-type 'pnm) ;solves error: preview-image-type setting 'png unsupported by this Emacs
(setq preview-gs-options '("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")) ;switch to NOSAFER is need to make preview work????

(setq TeX-parse-self t) ; parse file on load in order to apply style hooks
(setq TeX-auto-save t)  ; save that parsed data in 'auto' folder when saving
(setq-default TeX-master nil) ; ask for master file if needed

;(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook (lambda ()
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
  '(define-key LaTeX-mode-map (kbd "C-c e") 'LaTeX-environment))

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
Inside command, start and end will be bound to the results of those forms."
  (interactive "")
  `(save-excursion
    (while t          ;until one of the forms tries a failed search
    (goto-char (point-min))
    ,move-to-start-form
      (let ((start (point-marker)))
        ,move-to-end-form
        (let ((end (point-marker)))
          ,command)))))

(defun format-latex-align ()
  "Adds basic latex math markup to all regions between @@@ and @@"
  (interactive "")
  (command-on-all-delimited-regions
    (progn
      (re-search-forward "^@@@")
      (goto-char (match-beginning 0)))
    (progn
      (next-line)           ;skip over @@@ since it contains @@
      (re-search-forward "^@@[^@]?")
      (goto-char (match-end 0)))
    (progn
      (shell-command-on-region start end "bash %HOME%/.emacs.d/format_latex_math.sh" nil t))))

(defun orgtbl-export-table-to-matrix (start end)
  (interactive "r")
  (goto-char start)
  (insert "\\begin{bmatrix}\n")
  (previous-line)
  (delete-indentation) ;merge this one with the previous line
  (goto-char end)
  (insert "\n\\end{bmatrix}")
  (replace-regexp "^[ \t]*| " "" nil start end)
  (replace-regexp "|[^|]*$" "\\\\\\\\" nil start end)
  (replace-regexp "|" "&" nil start end))

(defun orgtbl-export-all-tables-to-matrices ()
  (interactive "")
  (command-on-all-delimited-regions
    (progn
      (re-search-forward "^[ \t]*|")   ;go to the next line of a table in buffer
      (goto-char (match-beginning 0)))
    (progn
      (while (string-match "^[ \t]*|" (thing-at-point 'line)) ;while we are on a table line
        (next-line))                                          ;move forward one line
      (search-backward "|")
      (forward-char))                                         ;then move after the last | in the table
    (progn
      (orgtbl-export-table-to-matrix start end))))



; ORG-MODE CUSTOMIZATIONS

(setq default-major-mode 'org-mode)
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode)) ;open txt files in org-mode

(global-set-key (kbd "C-|") 'org-table-create-or-convert-from-region)

(set-face-attribute 'org-level-3 nil ':foreground "PaleGreen")

(setq org-support-shift-select t)
(setq org-startup-folded 'showall) ;show everything on startup
(setq org-startup-truncated nil)   ;don't wrap lines
(setq org-log-done t)              ;insert a timestamp when a task is marked as finished
(setq org-M-RET-may-split-line nil)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook (lambda ()
             (org-indent-mode t)) t)

(setq org-directory "~\\org")       ;not used often by org

(setq org-agenda-file-regexp ".*\\.org")    ;include all org files in listed directories
(setq org-agenda-files (list "~\\org"
                             "~\\Dropbox\\orgmode"))

(setq org-mobile-checksum-binary (or (executable-find "shasum")
                                     (executable-find "sha1sum")
                                     (executable-find "md5sum")
                                     (executable-find "md5")))

(setq org-mobile-inbox-for-pull "~\\org\\from-mobile.org")
(setq org-mobile-directory "~\\Dropbox\\orgmode")



; ORG LATEX CUSTOMIZATIONS

(require 'ox-latex) ; required so we can set org-latex-default-packages-alist

(define-key org-mode-map (kbd "C-c e") 'LaTeX-environment)

;remove this font package because it provides a definition for iint that conflicts with amsmath (causes error)
(setq org-latex-default-packages-alist (delete '("" "wasysym" t) org-latex-default-packages-alist))

;assumes batch file sumatra is somewhere on $PATH
(setq TeX-view-program-list '(("sumatra" "sumatra -zoom 100% %o")))

(setq TeX-view-program-selection '(((output-dvi style-pstricks)
                                        "dvips and start")
                                       (output-dvi "Yap")
                                       (output-pdf "sumatra")
                                       (output-html "start")))

(setq org-export-async-debug t)



;; WHITESPACE MODE CUSTOMIZATIONS

(require 'whitespace)

(setq whitespace-style '(face trailing lines-tail indentation
                           empty tabs space-before-tab space-after-tab))
(setq whitespace-line-column 80)
(add-hook 'prog-mode-hook (lambda () (whitespace-mode t)))



; W3M CUSTOMIZATIONS

(setq browse-url-browser-function 'w3m-browse-url)
(setq w3m-use-cookies t)
(setq w3m-home-page "http://www.stackoverflow.com/questions")
(setq w3m-search-default-engine "g")
;don't use the word at point as the default search text
;(you have to delete it if you want to search something else)
(setq w3m-search-word-at-point nil)
(add-hook 'w3m-mode-hook 'w3m-lnum-mode)

;w3m-lnum's default hint color isn't great with dark-blue color theme
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
  ;prefix argument opens a new session
  ;how are sessions different from tabs????
  ;should w3m-new-tab use sessions somehow????
  (w3m-lnum-follow 4))

(defun w3m-search-new-tab (search-engine query)
  (interactive (w3m-search-read-variables))
  (w3m-new-tab)
  (w3m-search-do-search 'w3m-goto-url search-engine query))

(eval-after-load "w3m-search" '(progn
  ; C-u S g RET <search term> RET
  (add-to-list 'w3m-search-engine-alist '("g" "http://www.google.com/search?q=%s" utf-8))
  (add-to-list 'w3m-search-engine-alist '("q" "http://stackoverflow.com/search?q=%s" utf-8))
  (add-to-list 'w3m-search-engine-alist '("w" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8))))

(eval-after-load 'w3m-lnum '(progn
  (defvar w3m-mode-map)
  (evil-make-overriding-map w3m-lnum-mode-map 'normal t)
  (evil-make-overriding-map w3m-mode-map 'normal t)
  (evil-define-key 'normal w3m-mode-map
    "o" 'w3m-search
    "t" 'w3m-search-new-tab
;   "f" 'w3m-view-this-url  w3m-lnum handles this
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



; TRAMP CUSTOMIZATIONS

(eval-after-load "tramp"
  '(progn
     (add-to-list 'tramp-methods
                  (mapcar
                   (lambda (x)
                     (cond
                      ((equal x "sshx") "cygssh")
                      ((eq (car x) 'tramp-login-program) (list 'tramp-login-program "fakecygpty ssh"))
                      (t x)))
                   (assoc "sshx" tramp-methods)))
     (setq tramp-default-method "cygssh")))

(add-to-list 'load-path (substitute-in-file-name "$ELISP_ROOT/tramp-2.2.7/lisp"))



; OPENWITH CUSTOMIZATIONS

(when (require 'openwith nil 'noerror)
  (setq openwith-associations (list
         (list (openwith-make-extension-regexp '("mpg" "mpeg" "mp3" "flac" "m4a"
            "mp4" "avi" "wmv" "wav" "mov" "flv" "ogm" "ogg" "mkv")) "vlc" '(file))
         (list (openwith-make-extension-regexp '("png" "gif" "jpeg" "jpg"))  "firefox" '(file))
         (list (openwith-make-extension-regexp '("pdf")) "sumatra" '(file))))
  (openwith-mode t))

(setq large-file-warning-threshold 250000000) ; confirm if larger than 250 Mb

; IDO CUSTOMIZATIONS

(require 'ido)
(ido-mode t)



; DIRED MODE CUSTOMIZATIONS

; evil-integration contains a similar version of this.
; if you have any trouble rebinding things, see if that's what's causing it.
(eval-after-load 'dired
  '(progn
     ;; use the standard Dired bindings as a base
     (evil-make-overriding-map dired-mode-map 'normal t)
     (evil-add-hjkl-bindings dired-mode-map 'normal
       "J" 'dired-goto-file
       "K" 'other-window
       "r" 'dired-do-redisplay
       ; move to the first real thing in the folder:
       "H" (lambda () (interactive) (evil-window-top) (evil-next-line 4) (evil-end-of-line))
       "M" (lambda () (interactive) (evil-window-middle) (evil-end-of-line))
       "L" (lambda () (interactive) (evil-window-bottom) (evil-previous-line) (evil-end-of-line))
       "n" 'evil-search-next
       "N" 'evil-search-previous
       "?" 'evil-search-backward
       (kbd "<return>") 'dired-find-file
       ";" (lookup-key dired-mode-map ":"))))



; SHELL MODE CUSTOMIZATIONS

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input 'this) ; move to prompt when typing
(setq comint-input-ignoredups t)              ; no duplicates in command history



; ESHELL CUSTOMIZATIONS

(setq eshell-scroll-to-bottom-on-input 'this) ; move to prompt when typing



; EZ-SHELL CUSTOMIZATIONS

(defun ez-shell ()
  (interactive)
  (with-current-buffer (get-buffer-create "*ez-shell*")
    (let ((shell-commands-to-run (buffer-substring-no-properties (point-min) (point-max))))
      (if (string= shell-commands-to-run "")
        (pop-to-buffer "*ez-shell*")
        (let* ((commands (split-string shell-commands-to-run "\n" t))
               (results (loop for command in commands collect (eshell-command-result command)))
               (report (mapconcat 'identity results "\n")))
          (with-current-buffer (get-buffer-create "*report*")
            (erase-buffer)
            (insert report)
            (when (not (get-buffer-window (current-buffer)))
              (message report))))))))

(define-key evil-normal-state-map "S" 'ez-shell)



; MAGIT CUSTOMIZATIONS

(require 'magit)

(let ((evil-magit-mode-maps '(magit-mode-map magit-commit-mode-map
                              magit-status-mode-map magit-log-mode-map
                              magit-cherry-mode-map magit-reflog-mode-map
                              magit-diff-mode-map magit-wazzup-mode-map
                              magit-branch-manager-mode-map)))
  (loop for mode-map in evil-magit-mode-maps do
        (progn
          (evil-define-key 'normal  (symbol-value mode-map)
            (kbd "C-w") evil-window-map
            (kbd "j")   (lookup-key evil-motion-state-map "j")
            (kbd "k")   (lookup-key evil-motion-state-map "k"))
          (evil-make-overriding-map (symbol-value mode-map) 'normal t))))

(let ((evil-magit-modes '(magit-mode magit-commit-mode
                          magit-status-mode magit-log-mode
                          magit-cherry-mode magit-reflog-mode
                          magit-diff-mode magit-wazzup-mode
                          magit-branch-manager-mode)))
  (setq evil-emacs-state-modes (remove-if (lambda (mode)
                                            (memq mode evil-magit-modes))
                                          evil-emacs-state-modes)))

(evil-define-key 'normal magit-mode-map
  (kbd "C-k") 'magit-discard-item)

(evil-define-key 'normal magit-commit-mode-map
  (kbd "n") 'evil-search-next)

(evil-define-key 'normal magit-log-mode-map
  (kbd "<return>") 'magit-visit-item)

(global-set-key (kbd "C-x g") 'magit-status)

; remove unnecessary bindings from git-rebase-mode:
(setq git-rebase-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c") 'server-edit)
        (define-key map (kbd "C-c C-k") 'rebase-mode-abort)
        map))

(add-hook 'git-rebase-mode-hook (lambda () (read-only-mode -1)))



; AUTO-COMPLETE CUSTOMIZATIONS

; setup completion sources when entering appropriate major modes:
(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(global-auto-complete-mode)



; SMEX CUSTOMIZATIONS

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)



; GENERAL PROGRAMMING CUSTOMIZATIONS

(setq-default c-basic-offset 4)
(setq-default tab-width 4)

; consider whitespace or * to be valid line prefixes when filling
; paragraphs. * can be a line prefix in javadoc comments. This variable
; only applies when the paragraph being filled starts as one line.
(setq adaptive-fill-first-line-regexp "\\`[ \\t*]*\\'")



; C-SHARP (C#) MODE CUSTOMIZATIONS

(setq auto-mode-alist (cons '( "\\.cs\\'" . csharp-mode) auto-mode-alist) )



; ARTIST MODE CUSTOMIZATIONS

(defadvice artist-mode (after deactive-evil-for-artist-mode activate)
  (if artist-mode
    (turn-off-evil-mode)
    (turn-on-evil-mode)))



; CALC MODE CUSTOMIZATIONS

(require 'calc)
(require 'calc-ext)

(setq calc-display-trail nil) ;don't display trail buffer by default

;remap common operators to dvorak
(define-key calc-mode-map "}" 'calc-plus)
(define-key calc-mode-map "'" 'calc-minus)
(define-key calc-mode-map "[" 'calc-divide)
(define-key calc-mode-map "q" 'calc-algebraic-entry)

;need to remap K to switch to other buffers because I ensure
;that evil switches to insert state after entering a calc buffer (see evil_customizations)
(define-key calc-mode-map "K" 'other-window)

;use the latex calc language when the buffer is in org-mode (calc embedded)
(add-to-list 'calc-language-alist '(org-mode . latex))

(add-to-list 'evil-insert-state-modes 'calc-mode)


; MAIL CUSTOMIZATIONS

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

;CC myself on all sent emails because windows doesn't have the starttls.exe necessary
;for GNUS to be able to retrieve sent emails from gmail.
(setq gnus-posting-styles
  '((".*"
    ("CC" "gordon3.14@gmail.com")
    (address "gordon3.14@gmail.com"))))

(setq user-mail-address "gordon3.14@gmail.com")
(setq user-full-name "Gordon Gustafson")
(setq gnus-use-full-window nil)

;always fetch all the messages in a newsgroup.
;The prompt for how many to fetch can take longer to answer than the actual fetching would
(setq gnus-large-newsgroup nil)

;always read the backup file of group interactions if it exists (it's there in case Emacs crashes before the updates are applied)
;Disabled so I have one less prompty to answer
(setq gnus-always-read-dribble-file t)

(eval-after-load 'gnus-msg
  '(progn
     ;; use the standard gnus-summary-mode bindings as a base
     (evil-make-overriding-map gnus-summary-mode-map 'normal t)
     (evil-add-hjkl-bindings gnus-summary-mode-map  'normal
      ;"h"                           ;overrides gnus-summary-select-article-buffer
      ;"j"                           ;overrides gnus-summary-goto-article
      ;"k"                           ;overrides gnus-summary-kill-same-subject-and-select
      ;"l"                           ;overrides gnus-summary-goto-last-article
       "K" 'other-window             ;overrides gnus-summary-mime-map
       "H" 'evil-window-top          ;overrides gnus-summary-help-map
       "M" 'evil-window-middle       ;overrides gnus-summary-mark-map
       "L" 'evil-window-bottom)))    ;overrides gnus-summary-lower-score
