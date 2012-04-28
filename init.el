;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings undo-tree clojure-mode midje-mode color-theme-solarized))
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

;; visual
(require 'color-theme)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-solarized-light)
     ))

(set-default-font "Monospace-10")
(blink-cursor-mode)

(if window-system
    (progn ; Non-terminal adjustments
        )

    (progn ; Terminal adjustments
        ;; The light colored hl-line is fine for GUI emacs but makes
        ;; it impossible to read things in a white-on-black terminal
        (defadvice hl-line-mode (after
                                 abrooks-advise-hl-line-mode
                                 activate compile)
          (set-face-background hl-line-face "white"))

        ;; This is the coloring of the selection region
        (face-spec-set 'region
                       '((((class color) (background light))
                          (;:foreground "blue"
                           :background "color-17"
                           :strike-through nil
                           :underline nil))
                         (t (:foreground "purple"
                             :background "unspecified"
                             :strike-through nil
                             :underline t))))
        ;; The idle-highlight is used by starter-kit... I think.
        (face-spec-set 'idle-highlight
                       '((((class color) (background light))
                          (;:foreground "black"
                           :background "gray25"
                           :strike-through nil
                           :underline nil))
                         (t (:foreground "purple"
                             :background "unspecified"
                             :strike-through nil
                             :underline t))))
        ;; The idle-highlight is used by starter-kit... I think.
        (face-spec-set 'show-paren-match
                       '((((class color) (background light))
                          (:foreground "yellow"
                           :background nil
                           :bold t
                           :strike-through nil
                           :underline nil))
                         (t (:foreground "purple"
                             :background "unspecified"
                             :strike-through nil
                             :underline t))))
        ;; This makes our trailing whitespace actually visible
        (face-spec-set 'trailing-whitespace
                       '((((class color) (background light))
                          (;:foreground "red"
                           :background "red"
                           :strike-through nil
                           :underline nil))
                         (t (;:foreground "red"
                             :background "red"
                             :strike-through nil
                             :underline nil))))))

;; prevent extraneous tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; setup dired+
(require 'dired+)

;; clojure related
(require 'slamhound)

(setq path-to-ctags "/usr/bin/ctags")
(defun create-clj-tags (dir-name)
  "Create tags file."
  (interactive "Directory: ")
  (shell-command
   (format "%s --langmap=Lisp:+.clj --regex-Lisp='/[ \t\(]*def[a-z]* \([a-z!-]+\)/\1/'  --regex-Lisp='/[ \t\(]*ns \([a-z.]+\)/\1/' -f %s/TAGS -e -R %s" path-to-ctags dir-name (directory-file-name dir-name))))

