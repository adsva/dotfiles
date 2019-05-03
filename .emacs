; packages!
(add-to-list 'load-path "~/.emacs.d/elisp/")

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (defvar ac-source-css-property-names
  '((candidates . (loop for property in ac-css-property-alist
			collect (car property)))))

  :config
  (setq web-mode-engines-alist '(("django"    . "\\.html\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property-names ac-source-css-property))
	  ("javascript" . (ac-source-words-in-buffer))
	  ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-current-column-highlight t)
  (add-hook 'web-mode-hook (lambda () (tern-mode t)))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'web-mode-hook #'(lambda ()
			       (enable-minor-mode
				'("\\.\\(js\\|vue\\)\\'" . prettier-js-mode))))
  :ensure t)
(use-package auto-complete
  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-modes 'web-mode)
  (add-to-list 'ac-modes 'python-mode)
  (global-auto-complete-mode t)
  (setq ac-ignore-case nil)
  (eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
  :ensure t)
(use-package go-eldoc
  :ensure t)
(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  :ensure t)
(use-package go-guru
  :ensure t)
(use-package go-autocomplete
  :ensure t)
(use-package virtualenvwrapper
  :config
  :ensure t)
(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ido)
  (setq projectile-use-git-grep t)
  (setq projectile-switch-project-action 'venv-projectile-auto-workon)
  (global-set-key "\C-f" 'projectile-find-file)
  (global-set-key "\C-p" 'projectile-grep)
  :ensure t)
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq flycheck-flake8rc "~/.flake8rc")
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
		  (or (buffer-file-name) default-directory)
		  "node_modules"))
	   (eslint (and root
			(expand-file-name "node_modules/eslint/bin/eslint.js"
					  root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  :ensure t)
(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq ido-use-faces nil)
  :ensure t)
(use-package ws-butler
  :config
  (ws-butler-global-mode)
  :ensure t)
(use-package sublime-themes
  :ensure t)
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode)
  :ensure t)
(use-package jedi
  :config
  (setq python-environment-virtualenv
	(append python-environment-virtualenv
		'("--python" "/usr/bin/python3")))
  (defun add-py-debug ()
    (interactive)
    (move-beginning-of-line 1)
    (insert "import pdb; pdb.set_trace();\n"))
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'venv-projectile-auto-workon)
  (add-hook 'python-mode-hook (lambda () (local-set-key (kbd "C-b") 'add-py-debug)))
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  :ensure t)
(use-package cc-mode
  :ensure t)
(use-package sass-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
  :ensure t)
(use-package rust-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package tern
  :config
  (setq tern-command (append tern-command '("--no-port-file")))
  :ensure t)
(use-package tern-auto-complete
  :ensure t)
(use-package py-isort
  :config
  (add-hook 'before-save-hook 'py-isort-before-save)
  :ensure t)
(use-package blacken
  :config
  (defun conditional-blacken-mode ()
    (cond ((locate-dominating-file default-directory ".blacken")
	   (blacken-mode))))
  (add-hook 'python-mode-hook 'conditional-blacken-mode)
  :ensure t)
(use-package prettier-js
  :ensure t)

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  (load-theme 'dorsey t)
  (set-face-background 'default "#1d1d1d")
  (set-face-foreground 'default "#f8f8f8")
)


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 2)
(setq js-indent-level 2)

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
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
  (defadvice ido-find-file (after find-file-sudo activate)
    "Find file as root if necessary."
    (unless (and buffer-file-name
		 (file-writable-p buffer-file-name))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))

(add-hook 'ido-setup-hook 'ido-define-keys)

(require 'uniquify)
(setq-default uniquify-buffer-name-style 'post-forward)

(setq ack-and-a-half-executable "/usr/bin/ack")

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
	(when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
	  (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(global-set-key "\M-r" 'rgrep)
(global-set-key "\M-n" 'next-error)
(global-set-key "\M-p" 'previous-error)

;; More ergonomical than arrow keys
(global-set-key "\M-i" 'previous-line)
(global-set-key "\M-j" 'backward-char)
(global-set-key "\M-k" 'next-line)
(global-set-key "\M-l" 'forward-char)


(global-set-key [S-dead-grave] "`")
(global-set-key (kbd "C-<backspace>") 'c-hungry-delete-backwards)

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
 '(custom-safe-themes
   (quote
    ("e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "486759384769d44b22bb46072726c2cfb3ccc3d49342e5af1854784d505ffc01" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "8ecf7ee27ae787aa8fa733f816288671b608762b15f9fc8d31bb4b472630fe31" "b1e54397de2c207e550dc3a090844c4b52d1a2c4a48a17163cce577b09c28236" default)))
 '(font-use-system-font t)
 '(global-font-lock-mode t)
 '(package-selected-packages
   (quote
    (toml-mode blacken scss-mode add-node-modules-path prettier-js nginx-mode swift-mode vue-mode virtualenvwrapper yaml-mode elpy ws-butler web-mode use-package sublime-themes projectile pkgbuild-mode jedi ido-vertical-mode go-guru go-eldoc go-autocomplete flycheck flx-ido)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
