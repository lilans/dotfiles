;disable backup
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

(use-package idle)
(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)))

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode))

(use-package powerline
  :ensure t
  :defer t
  :init)

(use-package rainbow-mode
  :ensure t
  :defer t
  :init (rainbow-mode))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(if (display-graphic-p) 
    (load-theme 'dracula t)
  (load-theme 'zenburn t))
 
(use-package swiper
  :ensure t
  :defer
  :init
  :bind (("\C-s" . swiper)))

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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit avy doom-themes xah-fly-keys racer flycheck-rust cargo rust-mode flymake-google-cpplint flymake-cppcheck google-c-style flycheck-clangcheck zenburn-theme zenburn use-package rainbow-mode rainbow-delimiters powerline imenu-anywhere idle-require idle-highlight-mode idle-highlight firebelly-theme dracula-theme counsel company cl-lib-highlight cherry-blossom-theme boon))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'flycheck-clangcheck)

(defun my-select-clangcheck-for-checker ()
  "Select clang-check for flycheck's checker."
  (flycheck-set-checker-executable 'c/c++-clangcheck
                                   "/usr/bin/clang-check")
  (flycheck-select-checker 'c/c++-clangcheck))

(add-hook 'c-mode-hook #'my-select-clangcheck-for-checker)
(add-hook 'c++-mode-hook #'my-select-clangcheck-for-checker)

;; enable static analysis
(setq flycheck-clangcheck-analyze t)

(use-package clang-format
  :ensure t
  :defer t
  :init 
  :bind (("C-c r" . clang-format-region)
	 ("C-c u" . clang-format-buffer)))
  
(setq clang-format-style-option "google")


;; Rust
(use-package rust-mode
  :ensure t
  :defer t
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

  
(use-package cargo
  :ensure t
  :defer t
  :init
  :bind (("C-c C-b" . cargo-process-build)
	 ("C-c C-r" . cargo-process-run)
         ("C-c C-t" . cargo-process-test)))

(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c f") #'rust-format-buffer)))

(setq rustfmt-bin "~/.cargo/bin/rustfmt")

(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))


(setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "~/git/rust/src") ;; Rust source code PATH

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(use-package flycheck-rust
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'xah-fly-keys)

(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(add-to-list 'load-path "~/git/emacs-doom-themes")

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(require 'doom-themes)

(load-theme 'doom-one t)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-org-config)

(add-to-list 'default-frame-alist '(font . "SauceCodePro Nerd Font Mono Medium 9"))

(add-to-list 'load-path "~/git/avy")

(require 'avy)

(global-set-key (kbd "C-:") 'avy-goto-char)
