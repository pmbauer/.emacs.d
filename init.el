;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings slime slime-repl undo-tree clojure-mode midje-mode color-theme-solarized))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; load-path extensions
(add-to-list 'load-path "~/.emacs.d/evil/lib")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/extras")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "/opt/lono-dev-env/emacs")

;; lonopair
(require 'lonopair)

;; evil mode
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "M-]") 'find-tag)
(define-key evil-normal-state-map (kbd "M-,") 'slime-pop-find-definition-stack)
(define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition)
(define-key evil-normal-state-map (kbd "C-x M-x") 'slime-send-dwim)

;; visual
(require 'color-theme)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-solarized-light)
     ))

(set-default-font "Monospace-10")
(blink-cursor-mode)

;; prevent extraneous tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; setup dired+
(require 'dired+)

;; clojure related
(require 'slamhound)
