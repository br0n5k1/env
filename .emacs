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

;; Use Ido-mode to navigate between files:

(require 'ido)

(setq ido-enable-flex-matching t)

(ido-mode t)
(ido-everywhere t)

;; Make cursor less jumpy:

(setq scroll-margin 3)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

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
	    (show-paren-local-mode t)
            (display-line-numbers-mode t)))
