;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings slime slime-repl undo-tree clojure-mode clojure-test-mode color-theme-solarized haskell-mode))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; load-path extensions
(add-to-list 'load-path "~/.emacs.d/evil/lib")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/extras")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "/opt/lono-dev-env/emacs")

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                       (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; lonopair
(require 'lonopair)
(require 'lono-slime-extensions)

;; evil mode
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "M-]") 'find-tag)
;(define-key evil-normal-state-map (kbd "M-,") 'nrepl-jump-back)
;(define-key evil-normal-state-map (kbd "M-.") 'nrepl-jump)
(define-key evil-normal-state-map (kbd "M-,") 'slime-pop-find-definition-stack)
(define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition)
(define-key evil-normal-state-map (kbd "C-x M-x") 'slime-send-dwim)

;; nrepl
;(setq nrepl-popup-stacktraces nil)

;; visual
(require 'color-theme)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-solarized-light)
     ))

(set-default-font "Ubuntu Mono-11")
(blink-cursor-mode)

(if window-system
    (progn  ; Non-terminal adjustments
      )

  (progn    ; Terminal adjustments
    ;; The light colored hl-line is fine for GUI emacs but makes
    ;; it impossible to read things in a white-on-black terminal
    (defadvice hl-line-mode (after
                             abrooks-advise-hl-line-mode
                             activate compile)
      (set-face-background hl-line-face "white"))))

;; prevent extraneous tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; setup dired+
(require 'dired+)

;; setup revive
(require 'revive)

;; clojure related
(require 'slamhound)
(setq auto-mode-alist (cons '("\\.txn$" . clojure-mode) auto-mode-alist))

;; elm related
(setq auto-mode-alist (cons '("\\.elm$" . haskell-mode) auto-mode-alist))
