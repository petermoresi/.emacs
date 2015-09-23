;;; init.el --- Emacs Config for evil-mode

(when (>= emacs-major-version 24)
  ;; ELPA Setup
  (require 'package)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize)

  (mapc
   (lambda (package)
     (or (package-installed-p package)
         (package-install package)))
   ;; List of packages to install
   '(apache-mode
     chef-mode
     cider
     clojure-mode
     coffee-mode
     org
     web-mode
     markdown-mode
     flycheck
     company
     magit
     web-mode
     htmlize
     evil
     evil-leader
     evil-org
     undo-tree
     yasnippet
     helm
     helm-company
     helm-git
     helm-gitlab
     helm-google
     helm-make
     helm-orgcard
     helm-proc
     leuven-theme)))

(menu-bar-mode -1)
(load-theme 'tango-dark t) ;; for Emacs 24+

;; enable windmove - http://www.emacswiki.org/emacs/WindMove
;; use shift and arrow keys to switch between buffers.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Global auto revert
(global-auto-revert-mode)

;; Turn on autocomplete everywhere
(global-company-mode)

;; Turn on the line number mode globally
(global-linum-mode)

;; Ensure that lines are not truncated
(setq-default truncate-lines t)

;; Global Keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-xg" 'magit-status)

;; https://github.com/magit/magit/issues/1968
;; Change magit setting for vim mindset
(setq-default evil-overriding-maps
              '(
                (magit-mode-map . nil)
                (magit-status-mode-map . nil)
                (magit-svn-status-mode-map . nil)
                (magit-svn-mode-map . nil)
                (magit-reflog-mode-map . nil)
                (magit-log-select-mode-map . nil)
                ; some other magit mode
                ))
(setq-default evil-normal-state-modes
              '(
                magit-mode
                magit-diff-mode
                magit-status-mode
                magit-log-mode
                magit-reflog-mode
                magit-process-mode))
(setq-default evil-insert-state-modes
              '(
                magit-log-select-mode
                diff-mode
                git-rebase-mode
                magit-popup-mode
                magit-popup-sequence-mode))
;;; then load evil

(with-eval-after-load 'magit
  (define-key magit-mode-map "\s" nil) ;space I use space as my evil-leader key
  (define-key magit-diff-mode-map "\s" nil) ;space
  (define-key magit-diff-mode-map "j" nil)

  (define-key magit-status-mode-map "j" 'next-line) ;may be should evil-next-line
  (define-key magit-mode-map "j" 'next-line)
  (define-key magit-mode-map "k" 'previous-line)
  (define-key magit-file-section-map "K" 'magit-discard)
  (define-key magit-file-section-map "k" nil)
  (define-key magit-hunk-section-map "K" 'magit-discard)
  (define-key magit-hunk-section-map "k" nil)
  (define-key magit-unstaged-section-map "k" nil)
  (define-key magit-unstaged-section-map "K" 'magit-discard)
  (define-key magit-staged-section-map "K" 'magit-discard)
  (define-key magit-staged-section-map "k" nil)
  (define-key magit-stash-section-map "K" 'magit-stash-drop)
  (define-key magit-stash-section-map "k" nil)
  (define-key magit-stashes-section-map "K" 'magit-stash-clear)
  (define-key magit-stashes-section-map "k" nil)

  (define-key magit-untracked-section-map "K" 'magit-discard)
  (define-key magit-untracked-section-map "k" nil)

  (define-key magit-branch-section-map "K" 'magit-branch-delete)
  (define-key magit-branch-section-map "k" nil)

  (define-key magit-remote-section-map "K" 'magit-remote-remove)
  (define-key magit-remote-section-map "k" nil)

  (define-key magit-tag-section-map "k" nil)
  (define-key magit-tag-section-map "K" 'magit-tag-delete))

;; Web Mode file mapping
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . web-mode))

;; Web Mode setup
(setq web-mode-content-types-alist
      '(("jsx"  . "\\.js[x]?\\'")
        ("jsx"  . "\\.es6$")
        ))

(add-hook 'web-mode-hook
          (lambda ()
            (tern-mode t)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (web-mode-set-content-type "jsx")
            (message "now set to: %s" web-mode-content-type)))

(setq-default indent-tabs-mode nil)

;; Set backups in a different directory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Turn off noisy bell
 (setq visible-bell 1)

;; Company Mode - autocomplete goodness
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-<tab>") 'company-complete)

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))


;; AVOID WEIRD BEHAVIOR IN PATH ENVIRONMENT VARIABLE Copy PATH from ~/.bashrc
(let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

; helm config
(require 'helm-config)
(helm-mode 1)

;; helm company setup
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; bind helm-google to 'h' followed by '-' while holding Ctrl key.
(global-set-key (kbd "C-h C--") 'hhttp://www.emacswiki.org/emacs/WindMoveelm-google)

;; ORG Mode Configuration
(setq org-directory "~/org")

;; Make the source code pretty
(setq org-src-fontify-natively t)

;; Make my data secure org-crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key "A706CA78")

;; Auto-saving does not cooperate with org-crypt.el: so you need
;; to turn it off if you plan to use org-crypt.el quite often.
;; Otherwise, you'll get an (annoying) message each time you
;; start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-
(setq auto-save-default nil)

;; Setup developer workflow
(setq org-todo-keywords
      '((sequence "TODO(t)" "ACTIVE(a)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

;; ORG Capture
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("w" "Work Task" entry (file+headline "~/org/gtd.org" "Work")
	 "* TODO %?\n  %T\n  %i\n  %a")
	("p" "Personal Task" entry (file+headline "~/org/gtd.org" "Personal")
	 "* TODO %?\n  %T\n  %i\n  %a")
	("j" "Journal" entry (file+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
	("b" "Log Purchase" entry(file+datetree "~/org/budget.org")
	 "* %?\n    :PROPERTIES:\n    :AMOUNT: \n    :STORE: \n    :PURCHASE_BY: \n    :PAYMENT_METHOD: \n    :END:\n\n%U\n %i\n")))

;; YASnippet
(require 'yasnippet)

;; (setq yas-snippet-dirs
'("~/.emacs.d/plugins/yasnippet/snippets/"        ;; snippets packaged with YASnippets
  "~/.emacs.d/snippets")				;; personal snippets

;; Markdown Setup
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Enable Color in Shell
;; http://www.emacswiki.org/cgi-bin/wiki?AnsiColor
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'rsh-mode-hook 'ansi-color-for-comint-mode-on)

;; Map up/down keys to previous input in the shell
(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

;; Make emacs evil (vim-like)
;; Installed from git://gitorious.org/evil/evil.git
(require 'evil)

;; Installed from https://github.com/cofi/evil-leader
(require 'evil-leader)

;; Installed from git://github.com/edwtjo/evil-org-mode.git
(require 'evil-org)
(evil-mode 5)

;; ;; override default undo behavior
(require 'undo-tree)

;; Variable managed with emacs GUI

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "08851585c86abcf44bb1232bced2ae13bc9f6323aeda71adfa3791d6e7fea2b6" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" default))))
