(in-package :stumpwm-init)

(defun local-bin (str)
  (concat "~/.local/bin/" str))

(defun exec-str (str)
  (concat "exec " str))

(defvar +app-launcher+
  (local-bin "emacs-application-launcher")
  "The path to the application launcher script.")

(defvar +primary-term+
  (local-bin "emacs-eshell")
  "The path to the terminal script.")

(defvar +secondary-term+
  (local-bin "emacs-vterm")
  "The path to the terminal script.")

(defvar +emacs-everywhere+
  (local-bin "emacs-everywhere")
  "The path to the emacs-everywhere script.")

(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-l") "move-focus right")

(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-L") "move-window right")

(define-key *top-map* (kbd "s-d") (exec-str +app-launcher+))
(define-key *top-map* (kbd "s-RET") (exec-str +primary-term+))
(define-key *top-map* (kbd "s-S-RET") (exec-str +secondary-term+))

(define-key *top-map* (kbd "s-E") (exec-str +emacs-everywhere+))
(define-key *top-map* (kbd "s-e") (exec-str "emacsclient -cn -a ''"))

(provide 'stumpwm-init-keybindings)
