;; ELPA Setup
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Load Ruby with RVM
;;(require 'rvm)
;;(rvm-use ruby-2.1.3");; use rvm's default ruby for the current Emacs session

;; AVOID WEIRD BEHAVIOR IN PATH ENVIRONMENT VARIABLE Copy PATH from ~/.bashrc
(let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))


;; Enable Color in Shell
;; http://www.emacswiki.org/cgi-bin/wiki?AnsiColor
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'rsh-mode-hook 'ansi-color-for-comint-mode-on)

;; ORG Mode Configuration
(setq org-directory "~/org")

;; Make the source code pretty
(setq org-src-fontify-natively t)

;; Make my data secure org-crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(setq org-crypt-key "A706CA78")
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.

(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need
;; to turn it off if you plan to use org-crypt.el quite often.
;; Otherwise, you'll get an (annoying) message each time you
;; start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-


;; ORG Capture
(define-key global-map "\C-cc" 'org-capture)


(setq org-capture-templates
      '(("w" "Work Task" entry (file+headline "~/org/gtd.org" "Work")
	 "* TODO %?\n  %T\n  %i\n  %a")
	("p" "Personal Task" entry (file+headline "~/org/gtd.org" "Personal")
	 "* TODO %?\n  %T\n  %i\n  %a")
	("j" "Journal" entry (file+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
	("b" "Budget Note" entry(file+datetree "~/org/budget.org")
	 "* %?\nEntered on %U\n %i\n %a")
	("h" "Helicopter Note", entry(file+datetree "~/org/heli.org")
	 "* %?\nEntered on %U\n %i\n %a")))

;; Leuven is beautiful paint for org-mode
(add-to-list 'custom-theme-load-path "/Users/jenmoresi/.emacs.d/elpa/leuven-theme-20140929.1435")
(load-theme 'leuven t)                  ;; for Emacs 24+


;; override default undo behavior
(require 'undo-tree)

;; $ cd ~/.emacs.d/plugins/
;; ~/.emacs.d/plugins$ git clone git://gitorious.org/evil/evil.git

;; Make emacs evil (vim-like)
(add-to-list 'load-path "~/.emacs.d/plugins/evil")
(require 'evil)
(evil-mode 1)

;; load the evil-leader
(add-to-list 'load-path "~/.emacs.d/plugins/evil-leader")
(require 'evil-leader')

;; mkdir -p ~/.emacs.d/plugins;
;; cd ~/.emacs.d/plugins;
;; git clone git://github.com/edwtjo/evil-org-mode.git

;; Make org-mode evil too
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(require 'evil-org)


(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)


(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(require 'evil-org)

;; Markdown Setup
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin")))
 '(org-babel-load-languages (quote ((R . t) (emacs-lisp . t) (js . t) (ruby . t) (python . t) (sqlite . t) (ledger . t) (C . t) (calc . t) (clojure . t) (org . t) (awk . t) (sql . t))))
 '(org-confirm-babel-evaluate nil))
(put 'upcase-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
