;; The init file that loads the other .el files in this directory
;; It should have path $HOME/.emacs.d/init.el for Emacs to find it automatically

;; Add my personal bin directory to Emacs' equivalent of $PATH
(add-to-list 'exec-path "~/bin")

(make-frame-visible)
(setq frame-title-format "%b - emacs")

(setq user-emacs-directory "~/.emacs.d/user-emacs-directory/")

(global-set-key (kbd "C-<delete>") 'kill-word)
(global-set-key (kbd "C-x p") 'fill-paragraph)
(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-x C-k") 'kill-buffer)       ; I really meant "C-x k"
(global-set-key (kbd "C-h C-f") 'describe-function) ; I really meant "C-h f"

(defvar gordon-global-mode-map (make-keymap)
  "As long as this is first in minor-mode-map-alist, these bindings will
override all others.")


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

(define-key gordon-global-mode-map (kbd "M-K") (lambda (prefix-arg) (interactive "p") (move-horizontal-edge-vertically    prefix-arg)))
(define-key gordon-global-mode-map (kbd "M-J") (lambda (prefix-arg) (interactive "p") (move-horizontal-edge-vertically (- prefix-arg))))
(define-key gordon-global-mode-map (kbd "M-L") (lambda (prefix-arg) (interactive "p") (move-vertical-edge-horizontally    prefix-arg)))
(define-key gordon-global-mode-map (kbd "M-H") (lambda (prefix-arg) (interactive "p") (move-vertical-edge-horizontally (- prefix-arg))))

;; scale up font a little. Note: emacs appears to take whatever was
;; set with `xrandr --dpi xxx` into account (it influences emacs' scaling)
(set-face-attribute 'default nil :family "Consolas" :height 130)
;; Remove fringes on windows to avoid wasting space
(set-fringe-mode 0)

;; Seting these to newlines makes one extra character fit on each line.
;; I don't know why this works; it may break in the future.
(let ((newline-glyph (make-glyph-code ?\n)))
  (set-display-table-slot standard-display-table 'truncation newline-glyph)
  (set-display-table-slot standard-display-table 'wrap newline-glyph))

;; let me wrap lines in narrow windows if I want to
(setq truncate-partial-width-windows 10)

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



;;(setq initial-buffer-choice t) ;buffer to view when not opening a certain file
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
(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
(setq sentence-end-double-space nil)
(setq blink-matching-delay .500)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist `(("." . "~/backups")))
(setq backup-by-copying t)

(put 'narrow-to-region 'disabled nil)
(setq-default fill-column 80)



;; settings loaded last because they could cause problems
(require 'package)

;(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(defvar gordon-packages '(color-theme-modern org ;frame-cmds frame-fns
                          openwith magit auto-complete smex
                          ggtags goto-chg undo-tree
                          find-file-in-project evil-anzu dtrt-indent)
  "Packages that will be installed/updated to the latest version on startup")

(setq package-check-signature nil)

(defun install-gordon-packages ()
  (interactive)
  (mapc 'package-install gordon-packages)
  ;; If we don't remove the evil package we could end up using it instead
  ;; of the git version. There's no way to remove it programmatically.
  ;; If we don't remove it from other packages' dependencies we'll get an
  ;; error on startup when the evil package isn't installed.
  (message "Don't forget to remove the evil package and remove it from \
other packages' dependencies!"))

(load-theme 'dark-blue t)

;; From https://emacs.stackexchange.com/questions/17866
;; Not sure if this is needed?
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(require 'ispell)
(setq ispell-program-name "aspell")

;; (require 'frame-cmds)
;; (define-key gordon-global-mode-map (kbd "C-S-J") 'enlarge-frame)
;; (define-key gordon-global-mode-map (kbd "C-S-K") 'shrink-frame)
;; (define-key gordon-global-mode-map (kbd "C-S-L") 'enlarge-frame-horizontally)
;; (define-key gordon-global-mode-map (kbd "C-S-H") 'shrink-frame-horizontally)

;; (require 'server)

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
;; this is a no-op if we're not using a mac window system
(load "~/.emacs.d/mac_customizations.el")


;; do this after everything else so this appears first in minor-mode-map-alist
(define-minor-mode gordon-global-mode
  "A minor mode so that my key settings override annoying major modes."
  t "" 'gordon-global-mode-map)

(evil-make-overriding-map gordon-global-mode-map 'normal t)
(gordon-global-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Documents/notes.org" "~/Dropbox/org")))
 '(package-selected-packages
   (quote
    (go-mode yaml-mode ein jupyter groovy-mode jenkins json-mode exec-path-from-shell markdown-mode smex openwith magit ggtags find-file-in-project evil-anzu dtrt-indent color-theme-modern auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
