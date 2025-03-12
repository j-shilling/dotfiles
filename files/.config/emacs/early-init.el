(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push (cons 'left-fringe 8) default-frame-alist)
(push (cons 'right-fringe 8) default-frame-alist)
(push '(no-special-glyphs) default-frame-alist)
(push '(undecorated) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(push '(internal-border-width . 8) default-frame-alist)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
