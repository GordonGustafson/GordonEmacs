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
                      :family "Consolas" :height 125)

  ;; Assumes you've installed GNU coreutils with brew
  ;; (When you get into advanced usage you start finding incompatibilities,
  ;; so I use GNU coreutils everywhere as they're more standard AND portable)
  (let ((path-to-gnu-bash "/usr/local/bin/bash"))
    (when (file-exists-p path-to-gnu-bash)
      (setq explicit-shell-file-name path-to-gnu-bash))))
