(server-start)

; packages!
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (setq web-mode-engines-alist '(("django"    . "\\.html\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  :ensure t)
(use-package go-mode
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  :ensure t)
(use-package projectile
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'ido)
  (setq projectile-use-git-grep t)
  (global-set-key "\C-f" 'projectile-find-file)
  (global-set-key "\C-p" 'projectile-grep)
  :ensure t)
(use-package flycheck
  :init
  (global-flycheck-mode)
  (setq flycheck-flake8rc "~/.flake8rc")
  :ensure t)
(use-package flx-ido
  :init
  (flx-ido-mode 1)
  (setq ido-use-faces nil)
  :ensure t)
(use-package ws-butler
  :init
  (ws-butler-global-mode)
  :ensure t)
(use-package sublime-themes
  :ensure t)
(use-package ido-vertical-mode
  :init
  (ido-vertical-mode)
  :ensure t)

; web-mode


(add-to-list 'load-path "~/.emacs.d/lisp/")

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  (load-theme 'dorsey t)
)


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 2)
(show-paren-mode 1)
(column-number-mode 1)
(setq scroll-step 1)
(scroll-bar-mode 0)

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
(ido-mode 1)
(ido-everywhere 1)

(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

(require 'uniquify)
(setq-default uniquify-buffer-name-style 'post-forward)

(setq ack-and-a-half-executable "/usr/bin/ack")


(global-set-key "\M-r" 'rgrep)
(global-set-key "\M-n" 'next-error)
(global-set-key "\M-p" 'previous-error)

;; More ergonomical than arrow keys
(global-set-key "\M-i" 'previous-line)
(global-set-key "\M-j" 'backward-char)
(global-set-key "\M-k" 'next-line)
(global-set-key "\M-l" 'forward-char)


(global-set-key [S-dead-grave] "`")

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

;(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
 '(column-number-mode t)
 '(custom-safe-themes (quote ("e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "8ecf7ee27ae787aa8fa733f816288671b608762b15f9fc8d31bb4b472630fe31" "b1e54397de2c207e550dc3a090844c4b52d1a2c4a48a17163cce577b09c28236" default)))
 '(global-font-lock-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))
