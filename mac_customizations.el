;; Gordon's Mac OSX-specific customizations for Emacs

(when (memq window-system '(mac ns))
  ;; the screen is shorter on a Macbook
  (add-to-list 'default-frame-alist '(height . 52))

  ;; Synchronize Emacs $PATH with that seen in a terminal window
  ;; since Mac doesn't do this for windowed Emacs
  (when (not (package-installed-p 'exec-path-from-shell))
    (package-install 'exec-path-from-shell))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH")

  ;; fonts appear smaller on Mac, so make the default bigger
  (set-face-attribute 'default nil
                      :family "Consolas" :height 135)

  ;; Assumes you've installed GNU coreutils with brew
  ;; (When you get into advanced usage you start finding incompatibilities,
  ;; so I use GNU coreutils everywhere as they're more standard AND portable)
  (let ((path-to-gnu-bash "/usr/local/bin/bash"))
    (when (file-exists-p path-to-gnu-bash)
      (setq explicit-shell-file-name path-to-gnu-bash)))

  (when (require 'openwith nil 'noerror)
    (setq openwith-associations
          (list
           (list (openwith-make-extension-regexp '("mpg" "mpeg" "mp3" "flac" "m4a" "mp4" "avi" "wmv"
                                                   "wav" "mov" "flv" "ogm" "ogg" "mkv")) "open -a /Applications/iTunes.app" '(file))
           ;; This opens images in a new firefox window. I couldn't
           ;; find a way to make them open in a new tab instead.
           (list (openwith-make-extension-regexp '("png" "gif" "jpeg" "jpg"))  "open -a /Applications/Firefox.app -new-tab" '(file))
           (list (openwith-make-extension-regexp '("pdf")) "open -a /Applications/Preview.app" '(file))
           (list (openwith-make-extension-regexp '("doc" "docx" "rtf" "odt")) "open -a /Applications/Pages.app" '(file))))))
