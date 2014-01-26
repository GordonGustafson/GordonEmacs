; LATEX CUSTOMIZATIONS

(load (substitute-in-file-name "$ELISP_ROOT/auctex/site-lisp/site-start.el")) 
(setq TeX-PDF-mode t)  ;use .pdf for previews instead of .dvi

(setq preview-image-type 'pnm) ;solves error: preview-image-type setting 'png unsupported by this Emacs
(setq preview-gs-options '("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")) ;switch to NOSAFER is need to make preview work????
; (setq preview-gs-command "C:\\\"Program Files\"\\gs\\gs8.71\\bin\\gswin32c.exe")
; "C:\\Program Files\\gs\\gs9.10\\bin\\gswin64c.exe"

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push 
    '("latexmk" "latexmk -pvc -pdf -f %s" TeX-run-TeX nil t
      :help "Run Latexmk on file")
    TeX-command-list)))

(setq TeX-command-default "latexmk")

(define-key evil-normal-state-map "S" 'prepare-for-export-to-latex)

(define-key LaTeX-mode-map (kbd "C-c e") 'LaTeX-environment)

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

(defun prepare-for-export-to-latex ()
  "Adds basic latex math markup to all regions between @@@ and @@"
  (interactive "")
  (message (buffer-name))
  ;(evil-with-single-undo
    (save-excursion
      (goto-char (point-min))
      (let ((start-delimiter "\\([^@]?\\)@@@\\([^@]?\\)") 
            (end-delimiter   "\\([^@]?\\)@@\\([^@]?\\)"))
        (while (re-search-forward start-delimiter) ;while start-delimiter exists in buffer
          (goto-char (point-min))
          (re-search-forward start-delimiter)
          (replace-match "\\1\\\\begin{align*}\\2") ;leave point at end of replacement text
          (forward-line 1)
          (let ((start-of-equations (point-marker)))
            (re-search-forward end-delimiter)
            (replace-match "\\1\\\\end{align*}\\2")
            (previous-line)     ;replacing the match moves us to the end of the NEXT line
            (beginning-of-line)
            (let ((end-of-equations (point-marker))
                    ; gawk "! /begin|end/ && /[^ \][^ \][ \t]*$/ {printf $0, \"  \\\\\"}" 
                   (command "gawk \"! /begin|end/ && /[^ \\][^ \\][ \t]*$/ {print $0, \\\"  \\\\\\\\\\\\\\\\\\\"}\""))
              (goto-char start-of-equations)
              (shell-command-on-region start-of-equations end-of-equations command nil t)
              ; reindent the file since shell-command-on-region inserts its output directly
              (org-indent-mode t)
              (while (< (point) end-of-equations) ;run this loop once per line
                (when (not (search-forward "&=" (line-end-position) t))
                  (progn
                    (beginning-of-line)
                    (when (re-search-forward "\\([^&=]\\)=\\|^=" (line-end-position) t)
                      (replace-match "\\1&="))
                      (forward-line 1))))))))) ;leaves point at beginning of next line 
  nil) ;return nil to tell write-contents-functions that we didn't save the file ourselves

;(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;(setq reftex-plug-into-AUCTeX t)







; ORG-MODE CUSTOMIZATIONS

(add-to-list 'load-path (substitute-in-file-name "$ELISP_ROOT\\orgmode\\lisp"))
(add-to-list 'load-path (substitute-in-file-name "$ELISP_ROOT\\orgmode\\contrib\\lisp"))

;; (require 'org-mobile)
(require 'evil)

(setq org-support-shift-select t)

(setq default-major-mode 'org-mode)
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode)) ;open txt files in org-mode instead of text-mode


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

; require latex so we can bind keys in org mode to latex commands
(require 'latex)

; required so we can set org-latex-default-packages-alist
(require 'ox-latex)

(define-key org-mode-map (kbd "C-c e") 'LaTeX-environment)

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))

;remove this font package because it provides a definition for iint that conflicts with amsmath (causes error)
(setq org-latex-default-packages-alist (delete '("" "wasysym" t) org-latex-default-packages-alist)) 

;(setcdr (assoc "\\.pdf\\'" org-file-apps) "C:\\\"Program Files (x86)\"\\SumatraPDF\\SumatraPDF.exe %s")
(setq org-latex-to-pdf-process (list "latexmk -f -pvc -pdf %f"))

;I put a batch file that calls SumatraPDF.exe in emacs/bin, which is on %PATH%
(setq TeX-view-program-list '(("sumatra" "sumatra -zoom 100% %o")))

(setq TeX-view-program-selection '(((output-dvi style-pstricks)
                                        "dvips and start")
                                       (output-dvi "Yap")
                                       (output-pdf "sumatra")
                                       (output-html "start")))

(add-hook 'org-mode-hook 
  (lambda () 
    (add-hook 'after-save-hook
      (lambda () 
        (if (file-exists-p (concat (file-name-sans-extension buffer-file-name) ".tex"))
          (org-latex-export-to-latex 1))
    ) nil t)))



(defvar *latexmk-process* nil)

(setq org-export-async-debug t)

(defun latexmk ()
  (interactive "")
  (org-latex-export-to-latex 1)
  (setq *latexmk-process* (start-process-shell-command "latexmk process" nil (concat "latexmk -f -pvc -pdf \"" (concat (file-name-sans-extension buffer-file-name) ".tex") "\""))))

(defun latexmk-cleanup ()
  (interactive "")
  (shell-command "latexmk cleanup process" nil "latexmk -c"))
  


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
    "K" 'other-window

    (kbd "C-e") 'w3m-scroll-up
    (kbd "C-y") 'w3m-scroll-down)))
 
(defun setup-my-w3m-keymap
  "Use my customized map."
  (interactive)
  (define-key w3m-mode-map "h" 'w3m-history)
  (define-key w3m-mode-map "t" 'w3m-scroll-down-or-previous-url)
  (define-key w3m-mode-map "n" 'w3m-scroll-up-or-next-url)
  ;; I don't often w3m to edit pages, so I'm borrowing o and e (right
  ;; below , / . for tab navigation) for page navigation instead.
  (define-key w3m-mode-map "o" 'w3m-view-previous-page)
  (define-key w3m-mode-map "e" 'w3m-view-next-page)
  ;; Browse in new sessions by default
  (define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url-new-session)
  (define-key w3m-mode-map [(shift return)] 'w3m-view-this-url)
  (define-key w3m-mode-map "g" 'w3m-goto-url)
  (define-key w3m-mode-map "G" 'w3m-goto-url-new-session)
  )

;(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;(global-set-key "\C-xm" 'browse-url-at-point)



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

(add-to-list 'load-path "$ELISP_ROOT/tramp-2.2.7/lisp")

; HELP MODE CUSTOMIZATIONS

;(eval-after-load "help"
; '(progn
;    (define-key help-mode-map "K" 'other-window)))

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
       "H" 'evil-window-top                   
       "M" 'evil-window-middle                   
       "L" 'evil-window-bottom                
       (kbd "<return>") 'dired-find-file
       ";" (lookup-key dired-mode-map ":")))) ; ":d", ":v", ":s", ":e"

; SHELL MODE CUSTOMIZATIONS

; shells has its own autocomplete, so use it by letting the tab key go through: 
;; (add-hook 'shell-mode-hook 
;;   (lambda 
;;     (define-key evil-insert-state-map (kbd "<tab>") 'self-insert-command)))

;; (setq explicit-shell-file-name "fakecygpty")
;; (setq explicit-fakecgypty-args "cmd")

;; (autoload 'bash-completion-dynamic-complete 
;;   "bash-completion"
;;   "BASH completion hook")
;; (add-hook 'shell-dynamic-complete-functions
;;   'bash-completion-dynamic-complete)
;; (add-hook 'shell-command-complete-functions
;;   'bash-completion-dynamic-complete)

; ARTIST MODE CUSTOMIZATIONS

(defadvice artist-mode (after deactive-evil-for-artist-mode activate)
  (if artist-mode
    (turn-off-evil-mode)
    (turn-on-evil-mode)))

; PYTHON MODE CUSTOMIZATIONS

(add-hook 'python-mode-hook
  (lambda ()
    (defun run-current-file-as-python ()
      (interactive)
      (save-buffer)
      (shell-command (concat "python \"" buffer-file-name "\"")))
    (define-key python-mode-map "\C-r" 'run-current-file-as-python))
)

;(setq python-python-command "C:/Python31/python3.1.exe")
;(setq python-shell-interpreter "C:/Python31/python3.1.exe")

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

;; (defadvice calc-dispatch (after calc-go-into-insert-state activate)
;;   (evil-insert-state))

;use the latex calc language when the buffer is in org-mode (calc embedded)
(add-to-list 'calc-language-alist '(org-mode . latex))

(define-key evil-normal-state-map "gh" (kbd "M-0 C-x * e C-x * x"))
  ;; (lambda ()
  ;;   (interactive)
  ;;   (calc-dispatch 0)))



;; (eval-after-load 'calc
;;   '(progn
;;     ;; (let ((overriding-calc-map (make-sparse-keymap)))
;;     ;;   (define-key overriding-calc-map (kbd "<tab>") 'calc-roll-up)
;;     ;;   (define-key overriding-calc-map (kbd "S-<tab>") 'calc-roll-down)
;;       (evil-make-overriding-map calc-mode-map 'insert t)))
      ;; (define-key calc-mode-map "j" 'next-line)
      ;; (define-key calc-mode-map "k" 'previous-line)
      ;; (define-key calc-mode-map "K" 'other-window)))

  ;; (define-key calc-mode-map "_" 'calc-auto-algebraic-entry)
  ;; (define-key calc-mode-map "d" 'calcDigit-start)
  ;; (define-key calc-mode-map "g" 'calc-info)
  ;; (define-key calc-mode-map "l" 'calc-change-sign)
  ;; (define-key calc-mode-map "x" 'calc-quit)
  ;; (define-key calc-mode-map "T" 'nil)
  ;; (define-key calc-mode-map "T?" 'calc-shift-Y-prefix-help)
  ;; (define-key calc-mode-map "{" 'calc-help)




; PACKAGE CUSTOMIZATIONS

(require 'package)

;; Be conscious of which archive the packages come from!
;; Some have higher quality packages than others...

(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


; SCALA CUSTOMIZATIONS

;(add-to-list 'load-path "~/.emacs.d/scala/scala-mode2")
;(require 'scala-mode2)
;(add-to-list 'load-path "~/.emacs.d/scala/ensime/elisp")
;(require 'ensime)
;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


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
     ;; use the standard gnus-group-mode bindings as a base
     (evil-make-overriding-map gnus-group-mode-map 'normal t)
     (evil-add-hjkl-bindings gnus-group-mode-map  'normal
      ;"h" overrides gnus-summary-select-article-buffer
      ;"j" overrides gnus-group-jump-to-group
      ;"k" overrides nothing
      ;"l" overrides gnus-group-jump-to-group
       "K" 'other-window                 ;overrides nothing
       "H" 'evil-window-top              ;overrides gnus-group-help-map  
       "M" 'evil-window-middle           ;overrides gnus-group-mark-map  
       "L" 'evil-window-bottom)

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
