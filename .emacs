;; Turn off visual helpers I don't use:

(when (featurep 'tool-bar)
  (tool-bar-mode -1))

(when (featurep 'menu-bar)
  (menu-bar-mode -1))

(when (featurep 'scroll-bar)
  (scroll-bar-mode -1))

(when (featurep 'fringe)
  (fringe-mode 0))

(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

;; Turn off automatic backups:

(setq auto-save-default nil)
(setq make-backup-files nil)

;; Turn off making lock files:

(setq create-lockfiles nil)

;; Use graphical window dividers:

(when (display-graphic-p)
  (window-divider-mode t))

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

;; Change windows by S + <arrow> keys:

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Use my preferred font:
;;
;; See https://monolisa.dev for examples.

(defcustom custom/font-name "MonoLisa Custom"
  "Preferred font name.")

(defcustom custom/font-size 16
  "Preferred font size.")

(let ((preferred-font (font-spec :family custom/font-name :size custom/font-size)))
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
            (show-paren-local-mode t)))

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
  :ensure t
  :init
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines))

;; Use Magit for superior Git experience:

(use-package magit
  :straight t
  :ensure t
  :bind
  ("C-c C-s" . magit-status)
  :init
  (let ((path (expand-file-name "src" (file-name-directory load-file-name)))
        (depth 2))
    (setq magit-repository-directories (list (cons path depth)))))

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

;; Show completions in child frame:

(use-package company-box
  :straight t
  :ensure t
  :after (company)
  :hook
  (company-mode . company-box-mode))

;; Use Tabnine for more meaningful completion:
;;
;; NOTE M-x `company-tabnine-install-binary' to install Tabnine daemon executable.
(use-package company-tabnine
  :straight t
  :ensure t
  :after (company company-box)
  :init
  (add-to-list 'company-backends #'company-tabnine))

;; Show code smells marks:
;;
;; BUG THIS PACKAGE FAILS TO KEEP SETTINGS WHEN THE THEME IS RELOADED
;; BUG EXAMPLE, WHEN SWITCHING BETWEEN DAY/NIGHT THEMES

(defun custom/--update-todo-keywords-faces ()
  "Set custom faces and reload hl-todo package."
  (setq hl-todo-keyword-faces
	    '(("TODO"     . "#FF0000")
          ("NOTE"     . "#FF0000")
          ("HACK"     . "#FF0000")
          ("BUG"      . "#FF0000")
          ("XXX"      . "#FF0000")
          ("FIXME"    . "#FF0000")
          ("TECHDEBT" . "#FF0000")))
  (global-hl-todo-mode))

(use-package hl-todo
  :straight t
  :ensure t
  :init
  (custom/--update-todo-keywords-faces))

;; Small mode-line enhancements:

(use-package moody
  :straight t
  :ensure t
  :init
  (setq x-underline-at-descent-line t))

;; Use custom theme:

(defcustom custom/light-theme 'tango
  "Theme to use in light mode.")

(defcustom custom/dark-theme 'tango-dark
  "Theme to use in dark mode.")

(defun custom/--get-system-theme ()
  "Determines if underlying system uses light or dark appearance."
  (let ((command "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\""))
    (if (string-equal (shell-command-to-string command) "true")
        'dark
      'light))) ; TODO Change to work on Linux and Windows.

(defvar custom/--system-theme nil)

(defun custom/update-theme ()
  "Use dark or light theme in sync with the system."
  (interactive)
  (when (and custom/light-theme custom/dark-theme)
    (let ((system-theme (custom/--get-system-theme)))
      (when (not (eq custom/--system-theme system-theme))
        (if (eq system-theme 'dark)
            (load-theme custom/dark-theme t)
          (load-theme custom/light-theme t))
        (custom/--update-todo-keywords-faces) ; HACK Apply above package again to make it re-read settings.
        (setq custom/--system-theme system-theme)))))

(use-package modus-themes
  :straight t
  :ensure t
  :after (hl-todo)
  :init
  (setq custom/light-theme 'modus-operandi)
  (setq custom/dark-theme 'modus-vivendi)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-fringes 'subtle)
  (setq modus-themes-subtle-line-numbers t)
  (setq modus-themes-syntax '(alt-syntax))
  (setq modus-themes-mode-line '(moody))
  (setq modus-themes-links '(neutral-underline background))
  (setq modus-themes-promts '(background bold))
  (setq modus-themes-markup '(bold italic background intense))
  (setq modus-themes-paren-match '(bold intense))
  (setq modus-themes-region '(no-extend bg-only))
  (custom/update-theme)
  (let ((comment "#004c2e")
        (divider "#bcbcbc")
        (fadeout "#efefef"))
    (set-face-attribute 'font-lock-comment-face nil :foreground comment)
    (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground comment)
    (set-face-attribute 'mode-line nil :overline divider)
    (set-face-attribute 'mode-line nil :underline (list :color divider))
    (set-face-attribute 'mode-line-inactive nil :background fadeout)
    (set-face-attribute 'window-divider nil :foreground fadeout)
    (set-face-attribute 'window-divider-first-pixel nil :foreground divider)
    (set-face-attribute 'window-divider-last-pixel nil :foreground divider)
    ))

;; Change themes automagically:

(defcustom custom/system-theme-change-detect-interval 3
  "Seconds between theme update attempts.")

(let ((interval custom/system-theme-change-detect-interval)
      (repeat t))
  (run-with-idle-timer interval repeat 'custom/update-theme))

;; Insert pairing brackets:

(use-package smartparens
  :straight t
  :ensure t
  :init
  (require 'smartparens-config)
  :hook
  (prog-mode . smartparens-mode))

;; Select portions of text with + and - keys:

(use-package expand-region
  :straight t
  :ensure t
  :init
  (define-key evil-normal-state-map (kbd "+") 'er/expand-region)
  (define-key evil-normal-state-map (kbd "-") 'er/contract-region))

;; Cleanup whitespace:

(use-package whitespace-cleanup-mode
  :straight t
  :ensure t
  :hook
  (prog-mode . whitespace-cleanup-mode))

;; Automatic indentation of code:

(use-package aggressive-indent
  :straight t
  :ensure t
  :hook
  (prog-mode . aggressive-indent-mode))

;; Apply different color to parens based on the level of nesting:

(use-package rainbow-delimiters
  :straight t
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Manage projects from Emacs:
;;
;; NOTE Requires Magit to know where to find repositories.

(defun update-projects () ; No prefix for simpler interactive calls.
  "Add repositories known to Magit as projects."
  (interactive)
  (unless (featurep 'magit)
    (require 'magit))
  (mapcar 'projectile-add-known-project (magit-list-repos)))

(use-package projectile
  :straight t
  :ensure t
  :init
  (projectile-mode t)
  (update-projects)
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (setq projectile-require-project-root t)
  (setq projectile-switch-project-action #'projectile-find-file)
  (setq projectile-enable-caching t)
  (setq projectile-file-exists-remote-cache-expire nil))

;; Simple yet better-than-default in-buffer search:

(use-package ctrlf
  :straight t
  :ensure t
  :init
  (ctrlf-mode t))

;; Try Embark + Marginalia for context-based actions text and files:

(use-package marginalia
  :straight t
  :ensure t
  :config
  (marginalia-mode))



;; Cool interface for context switching:
;;
;; NOTE This configuration is a rough copy from package documentation.
;; NOTE I keep it here while learning.
;; TODO Narrow required functions and delete the rest.
(use-package consult
  :straight t
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5)
  (setq register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
   (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

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
