;;; Code:
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

(package-initialize)
(require 'package)
(require 'cl-lib)

;; Disable GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1) ;; отключаем графическое меню
(tool-bar-mode     -1) ;; отключаем tool-bar
(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
(setq use-dialog-box     nil) ;; никаких графических диалогов и окон - все через минибуфер
(setq redisplay-dont-pause t)  ;; лучшая отрисовка буфера
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал

;; Linum plugin
(require 'linum) ;; вызвать Linum
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format  "%d ") ;; задаем формат нумерации строк

;; Fringe settings
(fringe-mode '(8 . 0)) ;; органичиталь текста только слева
(setq-default indicate-empty-lines t) ;; отсутствие строки выделить глифами рядом с полосой с номером строки
(setq-default indicate-buffer-boundaries 'left) ;; индикация только слева

;; Line wrapping
(setq word-wrap          t) ;; переносить по словам
(global-visual-line-mode t)

;; Scrolling settings
(setq scroll-step               1) ;; вверх-вниз по 1 строке
(setq scroll-margin            10) ;; сдвигать буфер верх/вниз когда курсор в 10 шагах от верхней/нижней границы
(setq scroll-conservatively 10000)

(setq x-select-enable-clipboard t)

(setq search-highlight        t)
(setq query-replace-highlight t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(setq-default frame-title-format
 '(:eval (concat (when (file-remote-p default-directory)
                   (let ((user (file-remote-p default-directory 'user))
                         (host (file-remote-p default-directory 'host)))
                     (format "%s@%s:" user host)))
                 (or buffer-file-truename dired-directory (buffer-name)))))


(let ((set-transparent-fringe
       (lambda (&rest _)
        (set-face-background 'fringe (face-attribute 'default :background)))))
  (advice-add #'load- :after set-transparent-fringe)
  (advice-add #'disable- :after set-transparent-fringe))


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)))

(use-package ivy
  :ensure t
  :defer
  :init (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
	 ("<f6>" . ivy-resume)))

(use-package counsel
  :ensure t
  :defer
  :init
  :bind (("M-x" . counsel-M-x)
	 ("C-c C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhytmbox)
         ("C-r" . counsel-expression-history)))

(use-package expand-region
  :ensure t
  :defer
  :init
  :bind (("M-s" . er/expand-region)))

(use-package magit
  :ensure t
  :defer
  :init)

(use-package elpy
  :ensure t
  :defer
  :init (elpy-enable)
  (setq elpy-rpc-backend "jedi"))

(electric-pair-mode)

(use-package ergoemacs-mode
  :ensure t
  :defer
  :init (setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
  (setq ergoemacs-keyboard-layout "us")
  (ergoemacs-mode 1))

(use-package xah-fly-keys
  :ensure t
  :defer
  :init)

(require 'xah-fly-keys)

(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(use-package flycheck
  :ensure t
  :defer
  :hook (prog-mode . flycheck-mode))

(use-package company
  :ensure t
  :defer
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
          (setq company-minimum-prefix-length 1))

(use-package lsp-mode
  :ensure t
  :defer
  :commands lsp
  :config (require 'lsp-clients))

(use-package lsp-ui
  :enusre t
  :defer)

(use-package toml-mode
  :ensure t
  :defer)

(use-package rust-mode
  :ensure t
  :defer
  :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :defer
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :defer
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(load-theme 'dracula t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dracula-theme flycheck rust-mode xah-fly-keys use-package rainbow-mode powerline magit imenu-anywhere expand-region ergoemacs-mode elpy diminish counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
