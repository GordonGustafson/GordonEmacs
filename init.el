; The init file that loads the other .el files in this directory
; It should have path $HOME/.emacs.d/init.el for Emacs to find it automatically

(make-frame-visible)
(setq frame-title-format "%b - emacs")

(setq user-emacs-directory "~/.emacs.d/user-emacs-directory/")

(global-set-key (kbd "C-<delete>") 'kill-word)
(global-set-key (kbd "C-x p") 'fill-paragraph)
(global-set-key (kbd "C-h a") 'apropos)

(defun window-top-or-bottom ()
  "Figure out if the current window is on top, bottom or in the middle"
  (let* ((this-window-y-min (nth 1 (window-edges)))
         (this-window-y-max (nth 3 (window-edges))))
    (cond
     ((eq 0 this-window-y-min) 'top)
     ((eq (- (frame-height) 1) this-window-y-max) 'bottom)
     (t 'mid))))

(defun window-left-or-right ()
  "Figure out if the current window is to the left, right or in the middle"
  (interactive)
  (let* ((this-window-x-min (nth 0 (window-edges)))
         (this-window-x-max (nth 2 (window-edges))))
    (cond
     ((eq 0 this-window-x-min) 'left)
     ((eq (+ (frame-width) 3) this-window-x-max) 'right)
     (t 'mid))))

(defun move-horizontal-edge-vertically (delta)
  (case (window-top-or-bottom)
    ('top    (window-resize (get-buffer-window) (- delta) nil))
    ('bottom (window-resize (get-buffer-window)    delta  nil))
    ('mid
      (if (> delta 0)
        (window-resize (window-in-direction 'above) (- delta) nil)
        (window-resize (window-in-direction 'below)    delta  nil)))))

(defun move-vertical-edge-horizontally (delta)
  (case (window-left-or-right)
    ('right (window-resize (get-buffer-window) (- delta) t))
    ('left  (window-resize (get-buffer-window)    delta  t))
    ('mid
      (if (> delta 0)
        (window-resize (window-in-direction 'right) (- delta) t)
        (window-resize (window-in-direction 'left )    delta  t)))))

(global-set-key (kbd "M-K") (lambda (prefix-arg) (interactive "p") (move-horizontal-edge-vertically    prefix-arg)))
(global-set-key (kbd "M-J") (lambda (prefix-arg) (interactive "p") (move-horizontal-edge-vertically (- prefix-arg))))
(global-set-key (kbd "M-L") (lambda (prefix-arg) (interactive "p") (move-vertical-edge-horizontally    prefix-arg)))
(global-set-key (kbd "M-H") (lambda (prefix-arg) (interactive "p") (move-vertical-edge-horizontally (- prefix-arg))))


(set-face-attribute 'default nil
                    :family "Consolas" :height 100)

(defun toggle-full-screen () (interactive) (shell-command "emacs_fullscreen.exe"))
(global-set-key [f11] 'toggle-full-screen)

(defadvice switch-to-buffer (before existing-buffer-only activate)
  "When interactive, switch to existing buffers only."
  (interactive "b"))  ;only allow buffer names as input

(setq read-buffer-completion-ignore-case t)



(defun replace-garbage-chars ()
"Replace goofy MS and other garbage characters with latin1 equivalents."
(interactive)
(save-excursion
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


(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(height . 57))
(add-to-list 'default-frame-alist '(width . 107))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq blink-matching-delay .500)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist `(("." . "~/backups")))
(setq backup-by-copying t)

(put 'narrow-to-region 'disabled nil)



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

(defvar gordon-packages '(color-theme org auctex frame-cmds frame-fns
                          bash-completion csharp-mode openwith git-commit-mode
                          magit)
  "Packages that will be installed/updated to the latest version on startup")

(dolist (package gordon-packages)
  (when (not (package-installed-and-up-to-date-p package))
    (package-install package)))

(dolist (upgradeable-packgage (package-menu--find-upgrades))
  (package-install upgradeable-packgage))

(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-blue)

(require 'ispell)
(setq ispell-program-name "aspell")

(require 'frame-cmds)
(global-set-key (kbd "C-S-J") 'enlarge-frame)
(global-set-key (kbd "C-S-K") 'shrink-frame)
(global-set-key (kbd "C-S-L") 'enlarge-frame-horizontally)
(global-set-key (kbd "C-S-H") 'shrink-frame-horizontally)

(require 'server)

;; called with -funcall argument to emacs
(defun start-a-server ()
  (interactive)
  (server-edit)
  (make-frame-invisible nil t)
  ;; if we leave this frame on the *scratch* buffer, things like ido-mode
  ;; will consider it visible and may try to switch to this invisible frame
  ;; when asked to view *scratch*. Since not all platforms support --daemon,
  ;; just visit to a buffer we probably won't try to switch to:
  (switch-to-buffer "*Quail Completions*"))


(load "~/.emacs.d/evil_customizations.el")
(load "~/.emacs.d/mode_customizations.el")
(load "~/.emacs.d/dvorak_customizations.el")
(load "~/.emacs.d/bugfixes.el")
