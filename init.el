;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p '(starter-kit
             starter-kit-lisp
             starter-kit-bindings
             undo-tree
             clojure-mode
             clojure-test-mode
             color-theme-solarized
             haskell-mode
             markdown-mode
             auto-complete
             ac-nrepl))
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

(defun lono-nrepl-hook-function ()
  (local-set-key (kbd "C-x M-x") 'nrepl-send-dwim))

(add-hook 'nrepl-interaction-mode-hook 'lono-nrepl-hook-function)

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(eval-after-load "paredit"
  '(progn
     ;;(define-key paredit-mode-map (kbd "C-c 0") 'paredit-forward-slurp-sexp)
     ;;(define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-barf-sexp)
     ;;(define-key paredit-mode-map (kbd "C-c 9") 'paredit-backward-slurp-sexp)
     ;;(define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-barf-sexp)
     ;;(define-key paredit-mode-map (kbd "M-R") 'paredit-raise-sexp)
     ;;(define-key paredit-mode-map (kbd "C-k") 'paredit-eager-kill-line)
     ;;(define-key paredit-mode-map (kbd "M-)") 'paredit-wrap-round-from-behind)
     ))

;; evil mode
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "M-]") 'find-tag)
(define-key evil-normal-state-map (kbd "M-,") 'nrepl-jump-back)
(define-key evil-normal-state-map (kbd "M-.") 'nrepl-jump)
;(define-key evil-normal-state-map (kbd "C-x M-x") 'nrepl-send-dwim)
;(define-key evil-normal-state-map (kbd "M-,") 'slime-pop-find-definition-stack)
;(define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition)
;(define-key evil-normal-state-map (kbd "C-x M-x") 'slime-send-dwim)

;; nrepl
(defun nrepl-refresh ()
  (interactive)
  (evil-write-all nil)
  (nrepl-switch-to-repl-buffer nil)
  (goto-char (point-max))
  (insert "(clojure.tools.namespace.repl/refresh)")
  (nrepl-return))

(defun nrepl-reset ()
  (interactive)
  (evil-write-all nil)
  (nrepl-switch-to-repl-buffer nil)
  (goto-char (point-max))
  (insert "(user/reset)")
  (nrepl-return))

(setq nrepl-popup-stacktraces nil)

(add-hook 'nrepl-mode-hook 'paredit-mode)

; autocomplete
(require 'ac-nrepl)
 (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
 (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
 (eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'nrepl-mode))
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

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

  (progn
    ;; The light colored hl-line is fine for GUI emacs but makes
    ;; it impossible to read things in a white-on-black terminal
    (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)))


;; prevent extraneous tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; setup dired+
(require 'dired+)

;; setup revive
(require 'revive)

;; clojure related
(setq auto-mode-alist (cons '("\\.txn$" . clojure-mode) auto-mode-alist))

;; elm related
(setq auto-mode-alist (cons '("\\.elm$" . haskell-mode) auto-mode-alist))
