;; Turn off visual helpers I don't use:

(when (featurep 'tool-bar)
  (tool-bar-mode -1))

(when (featurep 'menu-bar)
  (menu-bar-mode -1))

(when (featurep 'scroll-bar)
  (scroll-bar-mode -1))

(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

;; Turn off automatic backups:

(setq auto-save-default nil)
(setq make-backup-files nil)

;; Turn off making lock files:

(setq create-lockfiles nil)

;; Make sure Unicode is set up by default everywhere:

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Skip boring dialogs:

(require 'dired)

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

(add-hook 'comint-exec-hook ; Skip confirmation when terminating active shell.
          (lambda ()
            (let ((process (get-buffer-process (current-buffer))))
              (set-process-query-on-exit-flag process nil))))

;; Use my preferred font:
;;
;; See https://monolisa.dev for examples.
(let ((preferred-font (font-spec :family "MonoLisa Custom" :size 16)))
  (when (find-font preferred-font)
    (set-frame-font preferred-font t t t)))

;; Use font ligatures:
;;
;; Taken from custom patches, see https://bitbucket.org/mituharu/emacs-mac/src/master/ for details.
(when (functionp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode t))

;; Maximize current window:

(toggle-frame-maximized)

;; Make cursor less jumpy:

(setq scroll-margin 3)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; Simple mode to repeat commands:

(repeat-mode t)

;; Add basic comforts for text:

(add-hook 'text-mode-hook
	  (lambda ()
	    (visual-line-mode t)))

;; Add basic comforts for programming:

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default require-final-newline t)

(add-hook 'sgml-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 1)
            (sgml-guess-indent)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-toggle-auto-state t)
            (c-toggle-hungry-state t)))

(add-hook 'prog-mode-hook
          (lambda ()
            (hl-line-mode t)
            (show-paren-local-mode t)
            (display-line-numbers-mode t)))

;; Use better package manager:
;;
;; Requires default package manager disabled in early-init.el file,
;; see https://github.com/radian-software/straight.el#getting-started for explanation.

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) ; Integrate with `use-package' macro for easy configuration options.

;; Add support for popular config formats:

(use-package toml-mode
  :straight t
  :ensure t)

(use-package yaml-mode
  :straight t
  :ensure t)

;; Use Vim emulation for better editing experience:

(use-package evil
  :straight t
  :ensure t
  :config (evil-mode t))

(use-package evil-nerd-commenter
  :straight t
  :ensure t)

;; Use Magit for superior Git experience:

(use-package magit
  :straight t
  :ensure t
  :bind ("C-c C-s" . magit-status))

;; Add vendor binaries to the `exec-path' list:
(let ((custom-bin (expand-file-name "bin" (file-name-directory load-file-name))))
  (push custom-bin exec-path))

;; Delta is a popular tool that improves git diffs:
;;
;; XXX Make sure delta is in `exec-path'.
;;
;; Manual configuration required,
;; see https://github.com/dandavison/delta
;; see https://scripter.co/using-git-delta-with-magit/
(use-package magit-delta
  :straight t
  :ensure t
  :hook (magit-mode . magit-delta-mode))

;; Use web-mode to edit templates:

(use-package web-mode
  :straight t
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 1)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

;; Add basic support for Haskell:

(use-package haskell-mode
  :straight t
  :ensure t)

;; Add basic support for Elixir:

(use-package elixir-mode
  :straight t
  :ensure t)

;; Add basic support for PHP:

(use-package php-mode
  :straight t
  :ensure t)

;; Add basic support for JavaScript:

(use-package js2-mode
  :straight t
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package js2-refactor
  :straight t
  :ensure t
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (js2-refactor-mode))))

;; Make TODOs visible:

;; TODO Use overlay mode.

;; (use-package hl-todo
;;   :straight t
;;   :ensure t
;;   :hook (prog-mode . hl-todo-mode)
;;   :config
;;   (setq hl-todo-highlight-punctuation ":"
;;         hl-todo-keyword-faces
;;         `(("FIXME"    error bold)
;;           ("HACK"     error bold)
;;           ("XXX"      error bold)
;;           ("TODO"     error bold)
;;           ("TECHDEBT" error bold)
;;           ("NOTE"     error bold))))

;; Use tree-sitter to highlight programming languages:

(defconst supports/dynamic-module 
  (and (functionp 'module-load) (not (null module-file-suffix))))

(use-package tree-sitter
  :if supports/dynamic-module
  :straight t
  :ensure t
  :init
  (require 'tree-sitter))

(use-package tree-sitter-langs
  :if supports/dynamic-module
  :straight t
  :ensure t
  :init
  (require 'tree-sitter-langs))

;; XXX Tree-sitter has a limited set of supported languages, excluding:
;;  - Haskell
;;  - ?
(when supports/dynamic-module
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook
            (lambda ()
              (tree-sitter-hl-mode))))

;; Use company for basic completion:

(use-package company
  :straight t
  :ensure t
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (company-mode)))
  :custom
  (company-idle-delay 0)
  (company-show-numbers t))

;; Use Tabnine for more meaningful completion:
;;
;; NOTE Call `company-tabnine-install-binary' to install Tabnine executable.
(use-package company-tabnine
  :straight t
  :ensure t
  :init (add-to-list 'company-backends #'company-tabnine))

;; Use custom theme:

(defcustom custom/dark-theme 'tango-dark
  "Theme to use for dark mode.")

(defcustom custom/light-theme 'tango
  "Theme to use for light mode.")

(defun custom/get-application-mode ()
  (let ((command "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\""))
    (if (string-equal (shell-command-to-string command) "true")
        'dark
      'light))) ; TODO Handle Linux and Windows.

(defun custom/update-theme ()
  "Use dark or light theme based on application mode."
  (interactive)
  (when (and custom/light-theme custom/dark-theme)
    (let ((mode (custom/get-application-mode)))
      (if (eq mode 'dark)
          (load-theme custom/dark-theme t)
        (load-theme custom/light-theme t)))))

(use-package modus-themes
  :straight t
  :ensure t
  :init
  (setq custom/dark-theme 'modus-vivendi)
  (setq custom/light-theme 'modus-operandi)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-fringes 'subtle)
  (custom/update-theme))

;; Change themes automagically:

(let ((interval-seconds 5)
      (repeat t))
  (run-with-idle-timer interval-seconds repeat 'custom/update-theme))

;; Ido replacement with better interface:

(use-package vertico
  :straight t
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t)
  (setq vertico-resize t)
  (setq vertico-count 15)
  (setq vertico-scroll-margin 2)
  (setq enable-recursive-minibuffers t))

;; Save history for future completions:

(use-package savehist
  :straight t
  :ensure t
  :init
  (savehist-mode))

;; Custom completion style using space-separated components:

(use-package orderless
  :straight t
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
