(require 'package)

(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))


;; (use-package lsp-mode
;;   :custom
;;   (lsp-enable-snippet nil)
;;   (lsp-pylsp-plugins-flake8-ignore ["W291" "E203" "E501" "W503" "E701" "E704" "D102"])
;;   (lsp-pylsp-plugins-pydocstyle-enabled nil)
;;   (lsp-pylsp-plugins-isort-enabled nil)
;;   :hook
;;    ((python-mode . lsp))
;;    ((web-mode . lsp))
;;   :ensure t)

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (global-set-key "\M-r" 'lsp-find-references)
 )
  

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode)
  :ensure t)

(use-package ws-butler
  :config
  (ws-butler-global-mode)
  :ensure t)

(use-package blacken
  :config
  (add-hook 'python-mode-hook 'blacken-mode)
  :ensure t)

(use-package fantom-theme
  :ensure t)
(use-package sublime-themes
  :ensure t)
(use-package company
  :ensure t)
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :hook
  (web-mode . prettier-js-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-style-padding 0)
  (web-mode-script-padding 0)
  (web-mode-enable-current-element-highlight t)
  :ensure t)
(use-package prettier-js
  :ensure t)

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ido)
  (setq projectile-use-git-grep t)
  ;(setq projectile-switch-project-action 'venv-projectile-auto-workon)
  (global-set-key "\C-f" 'projectile-find-file)
  (global-set-key "\C-p" 'projectile-grep)
  (global-set-key "\C-o" 'projectile-switch-to-buffer)
  (global-set-key "\C-l" 'projectile-switch-project)
  :ensure t)
(use-package f :ensure t)
(use-package pyvenv
  :config
  (defun pyvenv-autoload ()
          (interactive)
          "auto activate venv directory if exists"
          (f-traverse-upwards (lambda (path)
              (let ((venv-path (f-expand "venv" path)))
              (when (f-exists? venv-path)
              (pyvenv-activate venv-path))))))

  (add-hook 'python-mode-hook 'pyvenv-autoload)
 :ensure t)


(use-package flycheck
  :config
  ;(global-flycheck-mode)
  ;(setq flycheck-flake8rc ".flake8")
  :ensure t)


(use-package persp-projectile
  :config
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode)
  :ensure t)

(use-package py-isort
  :config
  (setq py-isort-options '("--multi-line=3" "--trailing-comma"))
  (add-hook 'before-save-hook 'py-isort-before-save)
  :ensure t)

;(use-package ivy
;  :ensure t)

; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(defun ido-define-keys ()
 (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
 (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
 (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
 (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)
(ido-mode 1)



; uniquify
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'post-forward)

; keys
;(global-set-key "\M-r" 'rgrep)
(global-set-key "\M-n" 'next-match)
(global-set-key "\M-p" 'previous-error)
(global-set-key "\M-e" 'flycheck-next-error)


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 2)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(setq-default scroll-error-top-bottom t)
(setq make-backup-files nil)
(setq column-number-mode t)

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  ;(load-theme 'base16-ashes t)
  (load-theme 'base16-everforest-dark-hard t)
  ;(load-theme 'base16- t)
  ;(load-theme 'base16-ashes t)
  (set-face-background 'default "#171e23")
  (set-face-foreground 'default "#f3e6ca")
)


;; Optional: ensure flycheck cycles, both when going backward and forward.
;; Tries to handle arguments correctly.
;; Since flycheck-previous-error is written in terms of flycheck-next-error,
;; advising the latter is enough.
(defun flycheck-next-error-loop-advice (orig-fun &optional n reset)
  ; (message "flycheck-next-error called with args %S %S" n reset)
  (condition-case err
      (apply orig-fun (list n reset))
    ((user-error)
     (let ((error-count (length flycheck-current-errors)))
       (if (and
            (> error-count 0)                   ; There are errors so we can cycle.
            (equal (error-message-string err) "No more Flycheck errors"))
           ;; We need to cycle.
           (let* ((req-n (if (numberp n) n 1)) ; Requested displacement.
                  ; An universal argument is taken as reset, so shouldn't fail.
                  (curr-pos (if (> req-n 0) (- error-count 1) 0)) ; 0-indexed.
                  (next-pos (mod (+ curr-pos req-n) error-count))) ; next-pos must be 1-indexed
             ; (message "error-count %S; req-n %S; curr-pos %S; next-pos %S" error-count req-n curr-pos next-pos)
             ; orig-fun is flycheck-next-error (but without advise)
             ; Argument to flycheck-next-error must be 1-based.
             (apply orig-fun (list (+ 1 next-pos) 'reset)))
         (signal (car err) (cdr err)))))))

(advice-add 'flycheck-next-error :around #'flycheck-next-error-loop-advice)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("94256a2952c7a6a77b4f290e6a16d1b2ded393591e7e7fc03edf8cfd34a9b55e"
     "f8a16292b06df595902fe9ff742af8b160d33106db91f69d4efba46e2f151d48"
     "847025d7513316abf78a6a26eb785d7979d0fa891cee2dc76d519220c30d043d"
     "d8b8c09a745470f6c088dce5df19ade98894f4ced69ce32d53aded94d512826d"
     "ac068453acfa7c3cedd4ad1db7a49230657e5e9c8e2fe4ec287cffc2b8310384"
     "434c19477cfebbf4582c5aa8d9eb95b1b01207ccb8bd8b562d02ca77e7da31d5"
     "664111db1521fe3351061dc87aea95fa98b3f244f4b830fbc048d39c3a8bc125"
     "94ac10e5261b9a32c9f1a7ef88f3fb89bfcbad843436aaaedc97c7975d8e6ab2"
     "27dac7a05a4dabd15ee4fec7c881b172cb8464a11afcf3de6ffad3c61f20247a"
     "74797b7cbcc4356101a62037316149faa4935522775adaaa68972f7488361c0d"
     "33c4f1e69f2f266a0b8e006858039298e6ff00868048cbfb9e20d7e0e4d410c3"
     "7bf34d114ec815e05a1ecb7f1acfd61ef453bfd27d12cc4c2babfa08ca1314da"
     "cbc8efdcd8e957c9ede1b4a82fed7fa1f3114ff6e7498c16f0cccb9509c1c17c"
     "5d205766ba4c831730cf88aaba6fa76cd77af631f8572cd85b661766f25fd206"
     "de385583975ed8e83b71b212b3094ee74785834718e2413bc3acff36224fba8d"
     "1cfbec19edafb831c7729be2f6454ec019c21b9a54b39b3bb5ec276a6b21d484"
     "51fa6edfd6c8a4defc2681e4c438caf24908854c12ea12a1fbfd4d055a9647a3"
     "c53db9aec64c633360ecb6c1200fee65b55c528ba5dc3853c9c357024b5296c4"
     "daa27dcbe26a280a9425ee90dc7458d85bd540482b93e9fa94d4f43327128077"
     "c20728f5c0cb50972b50c929b004a7496d3f2e2ded387bf870f89da25793bb44"
     "d2ab3d4f005a9ad4fb789a8f65606c72f30ce9d281a9e42da55f7f4b9ef5bfc6"
     "8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378"
     "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02"
     "09b833239444ac3230f591e35e3c28a4d78f1556b107bafe0eb32b5977204d93"
     "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6"
     "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1"
     "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
