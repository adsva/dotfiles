(server-start)

; packages!
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'"))
)

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  ; colors!
  (load-theme 'tangotango t)
)

; Gofmt on save
(add-hook 'before-save-hook 'gofmt-before-save)

;(toggle-text-mode-auto-fill)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 2)
(show-paren-mode 1)
(column-number-mode 1)
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration 1)
(setq scroll-step 1)
;(setq auto-save-default nil)

;; IF colors are causing trouble with screen, try ading this to
;; screenrc and setting TERM to xterm-26color
;; # terminfo and termcap for nice 256 color terminal
;; # allow bold colors - necessary for some reason
;; attrcolor b ".I"
;; # tell screen how to set colors. AB = background, AF=foreground
;; termcapinfo xterm 'Co#256:AB=\E[48;5;%dm:AF=\E[38;5;%dm'
;; # erase background with current bg color
;; defbce "on"

;; Better buffer handling
(require 'ido)
(ido-mode)

(require 'uniquify)
(setq-default uniquify-buffer-name-style 'post-forward)

(global-set-key "\M-r" 'rgrep)
(global-set-key "\M-n" 'next-error)
(global-set-key "\M-p" 'previous-error)

;; More ergonomical than arrow keys
(global-set-key "\M-i" 'previous-line)
(global-set-key "\M-j" 'backward-char)
(global-set-key "\M-k" 'next-line)
(global-set-key "\M-l" 'forward-char)

;;;---------------------------------------------------------------------
;;; display-buffer

;; The default behaviour of `display-buffer' is to always create a new
;; window. As I normally use a large display sporting a number of
;; side-by-side windows, this is a bit obnoxious.
;;
;; The code below will make Emacs reuse existing windows, with the
;; exception that if have a single window open in a large display, it
;; will be split horisontally.

(setq pop-up-windows nil)

(defun my-display-buffer-function (buf not-this-window)
  (if (and (not pop-up-frames)
           (one-window-p)
           (or not-this-window
               (not (eq (window-buffer (selected-window)) buf)))
           (> (frame-width) 162))
      (split-window-horizontally))
  ;; Note: Some modules sets `pop-up-windows' to t before calling
  ;; `display-buffer' -- Why, oh, why!
  (let ((display-buffer-function nil)
        (pop-up-windows nil))
    (display-buffer buf not-this-window)))

(setq display-buffer-function 'my-display-buffer-function)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ========== Place Backup Files in Specific Directory ==========

;; Enable backup files.
(setq make-backup-files t)

;; Save all backup files in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


; Startup splash
(setq inhibit-startup-message t)

; Tilde issues
(defun insert-tilde ()
  "mg: inserts ~ at cursor position"
  (interactive)
  (insert "~")
  )
(global-set-key [dead-tilde] 'insert-tilde)


; Damn delete!
(global-set-key "\C-d" 'delete-backward-char)

; space indentation in c-modes
(setq c-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))
(setq objc-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))
(setq c++-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))

(setq c-default-style "k&r"
      c-basic-offset 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("b1e54397de2c207e550dc3a090844c4b52d1a2c4a48a17163cce577b09c28236" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
