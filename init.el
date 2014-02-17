
(setq auto-mode-alist (cons '( "\\.cs\\'" . csharp-mode ) auto-mode-alist ))
(setq c-basic-offset 4)

;(setq tramp-default-method "ssh")
;
;(nconc (cadr (assq 'tramp-login-args (assoc "ssh" tramp-methods)))
;       '(("bash" "-i")))
;(setcdr (assq 'tramp-remote-shell (assoc "ssh" tramp-methods))
;	'("bash -i"))

(make-frame-visible)

(setq frame-title-format "%b - emacs") 

; mapping C-<backspace> to M-<delete> in Autohotkey sends C-M-<delete> when capslock is used instead of control:
;(global-set-key (kbd "C-<backspace>") 'backward-kill-word)  
(global-set-key (kbd "C-M-<delete>") 'backward-kill-word)  

(global-set-key (kbd "C-<delete>") 'kill-word)
(global-set-key (kbd "C-x p") 'fill-paragraph)
(global-set-key (kbd "C-h a") 'apropos)

(defun window-top-or-bottom ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((this-window-y-min (nth 1 (window-edges)))
	     (this-window-y-max (nth 3 (window-edges))))
    (cond
     ((eq 0 this-window-y-min) 'top)
     ((eq (- (frame-height) 1) this-window-y-max) 'bottom)
     (t 'mid)))) 

(defun window-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (interactive)
  (let* ((this-window-x-min (nth 0 (window-edges)))
	     (this-window-x-max (nth 2 (window-edges))))
    (cond
     ((eq 0 this-window-x-min) 'left)
     ((eq (+ (frame-width) 3) this-window-x-max) 'right)
     (t 'mid))))

(defun move-horizontal-inner-edge-vertically (delta)
  (case (window-top-or-bottom) 
    ('top    (window-resize (get-buffer-window) delta nil))
    ('bottom (window-resize (get-buffer-window) (- delta) nil))))

(defun move-vertical-inner-edge-horizontally (delta)
  (case (window-left-or-right)
    ('left  (window-resize (get-buffer-window) delta t))
    ('right (window-resize (get-buffer-window) (- delta) t))))

(global-set-key (kbd "M-J") (lambda (prefix-arg) (interactive "p") (move-horizontal-inner-edge-vertically    prefix-arg)))
(global-set-key (kbd "M-K") (lambda (prefix-arg) (interactive "p") (move-horizontal-inner-edge-vertically (- prefix-arg))))
(global-set-key (kbd "M-L") (lambda (prefix-arg) (interactive "p") (move-vertical-inner-edge-horizontally    prefix-arg)))
(global-set-key (kbd "M-H") (lambda (prefix-arg) (interactive "p") (move-vertical-inner-edge-horizontally (- prefix-arg))))

   
(setq ctags-extensions "*.h *.c*")                                ;variable only used in this file
(setq ctags-path "C:\\Users\\GGustafson\\Desktop\\ctags58\\ctags.exe") ;only used in this file
(setq tags-file-path "")                                          ;only used in this file

(setq tags-revert-without-query t)                                ;revisit TAGS file if it changes on disk

(defun create-tags (raw-directory-name)
  "create a tags file in the chosen directory for all files in the directory."
  (interactive "DProject Root Directory: ")
  ;(setq tags-file-path (concat directory-name "TAGS"))
  (let ((directory-name (replace-regexp-in-string "/" "\\\\" (file-name-as-directory raw-directory-name))))
  (message (format "\"%s\" -f \"%sTAGS\" -eR \"%s\" %s" ctags-path directory-name directory-name ctags-extensions))))
  
;(defadvice find-tag (before update-tags-before-search activate)
;  "Automatically create tags file."
;  (unless (file-exists-p tags-file-path)
;    (shell-command "etags *.[ch] -o TAGS 2>/dev/null"))
;  (visit-tags-table tag-file)))

(set-face-attribute 'default nil
                    :family "Consolas" :height 100)

(defun toggle-full-screen () (interactive) (shell-command "emacs_fullscreen.exe"))
(global-set-key [f11] 'toggle-full-screen)

(defadvice switch-to-buffer (before existing-buffer-only activate)
  "When interactive, switch to existing buffers only."
  (interactive "b"))  ;only allow buffer names as input



(defun replace-garbage-chars ()
"Replace goofy MS and other garbage characters with latin1 equivalents."
(interactive)
(save-excursion				;save the current point
  (replace-string "΄" "\"" nil (point-min) (point-max))
  (replace-string "“" "\"" nil (point-min) (point-max))
  (replace-string "’" "'" nil (point-min) (point-max))
  (replace-string "“" "\"" nil (point-min) (point-max))
  (replace-string "—" "--" nil (point-min) (point-max)) ; multi-byte
  (replace-string "" "'" nil (point-min) (point-max))
  (replace-string "" "'" nil (point-min) (point-max))
  (replace-string "" "\"" nil (point-min) (point-max))
  (replace-string "" "\"" nil (point-min) (point-max))
  (replace-string "" "\"" nil (point-min) (point-max))
  (replace-string "" "\"" nil (point-min) (point-max))
  (replace-string "‘" "\"" nil (point-min) (point-max))
  (replace-string "’" "'" nil (point-min) (point-max))
  (replace-string "¡\"" "\"" nil (point-min) (point-max))
  (replace-string "¡­" "..." nil (point-min) (point-max))
  (replace-string "" "..." nil (point-min) (point-max))
  (replace-string "" " " nil (point-min) (point-max)) ; M-SPC
  (replace-string "" "`" nil (point-min) (point-max))  ; \221
  (replace-string "" "'" nil (point-min) (point-max))  ; \222
  (replace-string "" "``" nil (point-min) (point-max))
  (replace-string "" "''" nil (point-min) (point-max))
  (replace-string "" "*" nil (point-min) (point-max))
  (replace-string "" "--" nil (point-min) (point-max))
  (replace-string "" "--" nil (point-min) (point-max))
  (replace-string " " " " nil (point-min) (point-max)) ; M-SPC
  (replace-string "¡" "\"" nil (point-min) (point-max))
  (replace-string "´" "\"" nil (point-min) (point-max))
  (replace-string "»" "<<" nil (point-min) (point-max))
  (replace-string "Ç" "'" nil (point-min) (point-max))
  (replace-string "È" "\"" nil (point-min) (point-max))
  (replace-string "é" "e" nil (point-min) (point-max)) ;; &eacute;
  (replace-string "ó" "-" nil (point-min) (point-max))
))



;(setq initial-buffer-choice t) ;buffer to view when not opening a certain file
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

; add all installed packages to load path. Installing the packgage with ELPA does this itself somehow,
; but packages installed in NTEmacs don't seem to be put on the load path for Cygwin Emacs.
; Should probably find a better solution than just throwing the whole package directory in the load path
; (let ((default-directory "~/.emacs.d/elpa/"))
;   (normal-top-level-add-subdirs-to-load-path))


(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(height . 57))
(add-to-list 'default-frame-alist '(width . 107))

(setq read-buffer-completion-ignore-case t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)  
(setq blink-matching-delay .500)

(setq-default tab-width 4)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist `(("." . "~/backups")))
(setq backup-by-copying t)

; consider whitespace or * to be valid line prefixes when filling
; paragraphs. * can be a line prefix in javadoc comments. This variable
; only applies when the paragraph being filled starts as one line.
(setq adaptive-fill-first-line-regexp "\\`[ \\t*]*\\'")


;; Make sure that the bash executable can be found
;; (setq explicit-shell-file-name "C:/cygwin64/bin/bash.exe")
;; (setq shell-file-name explicit-shell-file-name)
;; (add-to-list 'exec-path "C:/cygwin64/bin")

(defun term-exec-1 (name buffer command switches)
  ;; We need to do an extra (fork-less) exec to run stty.
  ;; (This would not be needed if we had suitable Emacs primitives.)
  ;; The 'if ...; then shift; fi' hack is because Bourne shell
  ;; loses one arg when called with -c, and newer shells (bash,  ksh) don't.
  ;; Thus we add an extra dummy argument "..", and then remove it.
  (let ((process-environment
	 (nconc
	  (list
	   (format "TERM=%s" term-term-name)
	   (format "TERMINFO=%s" data-directory)
	   (format term-termcap-format "TERMCAP="
		   term-term-name term-height term-width)
	   ;; We are going to get rid of the binding for EMACS,
	   ;; probably in Emacs 23, because it breaks
	   ;; `./configure' of some packages that expect it to
	   ;; say where to find EMACS.
	   (format "EMACS=%s (term:%s)" emacs-version term-protocol-version)
	   (format "INSIDE_EMACS=%s,term:%s" emacs-version term-protocol-version)
	   (format "LINES=%d" term-height)
	   (format "COLUMNS=%d" term-width))
	  process-environment))
	(process-connection-type t)
	;; We should suppress conversion of end-of-line format.
	(inhibit-eol-conversion t)
	;; The process's output contains not just chars but also binary
	;; escape codes, so we need to see the raw output.  We will have to
	;; do the decoding by hand on the parts that are made of chars.
	(coding-system-for-read 'binary))
    (apply 'start-process name buffer
	   "C:/cygwin64/bin/bash.exe -c"
	   (format "stty -nl echo rows %d columns %d sane 2>/dev/null;\
if [ $1 = .. ]; then shift; fi; exec \"$@\""
		   term-height term-width)
	   ".."
	   command switches)))



; settings loaded last because they could cause problems
(require 'cl)
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defun package-installed-and-up-to-date-p (package)
  ;assumes that package-refresh-contents has already been called
  ;also assume all built-in packages are out of date, which is usually true.
  (if (not (package-installed-p package)) 
    nil
    (let* ((newest-desc  (cdr (assq package package-archive-contents)))
           (installed-desc (cdr (or (assq package package-alist) (assq package package--builtins))))
           (newest-version  (package-desc-vers newest-desc))
           (installed-version (package-desc-vers installed-desc)))
             (or (equal installed-version newest-version) (version-list-< newest-version installed-version)))))

(message "%s" "Emacs Prelude is now refreshing its package database...")
(package-refresh-contents)
(message "%s" " done.")
(dolist (package '(color-theme org auctex frame-cmds frame-fns bash-completion csharp-mode))
  (when (not (package-installed-and-up-to-date-p package))
    (package-install package)))

(dolist (upgradeable-packgage (package-menu--find-upgrades))
  (package-install upgradeable-packgage))

(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-blue) ;find a way to get brighter highlighting of mismatched delimiters in dark-blue like gnome2 has!
;(color-theme-gnome2)  

(require 'ispell)
(setq ispell-program-name "aspell")

(require 'frame-cmds)
(global-set-key (kbd "C-S-J") 'enlarge-frame)
(global-set-key (kbd "C-S-K") 'shrink-frame)
(global-set-key (kbd "C-S-L") 'enlarge-frame-horizontally)
(global-set-key (kbd "C-S-H") 'shrink-frame-horizontally)

(require 'server)

;called with command line argumnts to emacs
(defun start-a-server ()
  (interactive)
  (server-edit)
  (make-frame-invisible nil t))

(load "~/.emacs.d/evil_customizations.el")
(load "~/.emacs.d/mode_customizations.el")
(load "~/.emacs.d/dvorak_customizations.el")
(load "~/.emacs.d/bugfixes.el")
