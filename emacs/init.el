(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'dashboard-refresh-buffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(setq graphic-only-plugins-setting nil)

;;字体

(setq default-frame-alist '((font . "KKong3-15")))
;;先安裝字體，這個是谷歌的，免費開源的最完美字體
(set-fontset-font t 'han "KKong3")
;;(set-fontset-font t 'han "Source Han Serif SC Regular")
;;其他漢字語系的字體
(set-fontset-font t 'kana "Noto Sans CJK JP Regular")
(set-fontset-font t 'hangul "Noto Sans CJK KR Regular")
(set-fontset-font t 'cjk-misc "Noto Sans CJK KR Regular")


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; 啓動自動存檔
;; https://github.com/manateelazycat/auto-save.git
(add-to-list 'load-path "~/git/auto-save/")
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)

;; 下面是設置插件下載地址和管理
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
	(package-refresh-contents))

(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; 安裝和使用皮膚
(use-package gruvbox-theme)
(load-theme 'gruvbox t)

;;中文rime輸入
(use-package posframe)
(use-package rime)
(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "KKong3-15"
            :internal-border-width 5))

(setq default-input-method "rime"
      rime-show-candidate 'posframe)

;; 安裝projectile
(use-package projectile)
(projectile-mode +1)
;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile--find-file)

;; 安裝writeroom
(use-package writeroom-mode)
(global-writeroom-mode)
(setq writeroom-mode 1)

;;安裝jk單行設置
(use-package evil-better-visual-line
  :ensure t
  :config
  (evil-better-visual-line-on))

;; 安裝awesome-tab
;; https://github.com/manateelazycat/awesome-tab.git
(use-package awesome-tab
	       :load-path "~/git/awesome-tab/"
	       :config
		   (awesome-tab-mode t))
;; 在dashboard去掉多餘的tab
(defun awesome-tab-hide-tab (x)
  (let ((name (format "%s" x)))
    (or
     (string-prefix-p "*scratch" name)
     (string-prefix-p "*Messages" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (and (string-prefix-p "magit" name)
               (not (file-name-extension name)))
     )))
;;安裝ivy
(use-package ivy
			 	 :diminish
			 	 :config
			 	(ivy-mode 1))
;;安裝doom-modeline
(use-package doom-modeline
			 	 :ensure t
			 	 :init (doom-modeline-mode 1)
			 	 :custom ((doom-modeline-height 15)))

;; orgmode 的一切設置

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.05)
                  (org-level-4 . 2.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "KKong3" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
		:hook (org-mode . efs/org-mode-setup)
		:config
		(setq org-ellipsis " ▾")
		(efs/org-font-setup))

(require 'org-tempo)
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))
;; org 裏面打開鏈接直接用回車
(setq org-return-follows-link t)
;; org 打開鏈接直接是本頁
(add-to-list 'org-link-frame-setup '(file . find-file))
;;(setq org-hide-emphasis-markers t)
;;(use-package org-appear)
;;(add-hook 'org-mode-hook 'org-appear-mode)
;;(setq org-appear-autolinks t)

;; org-roam设置
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory (file-truename "~/write/org-roam/"))
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))



;;evil mode
(use-package evil)
(evil-mode 1)

;;dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

; Set the title
;;(setq dashboard-init-info "This is an init message!")
(setq dashboard-banner-logo-title "None-Note")
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
;; Set the banner
;;(setq dashboard-startup-banner "/home/k/Pictures/logo1.png")
;; Content is not centered by default. To center, set
(setq dashboard-center-content t)
;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)))
(setq dashboard-set-init-info nil)

(setq dashboard-set-footer nil)
(setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'font-lock-keyword-face))


(add-hook 'dashboard-mode-hook '(lambda () (setq mode-line-format nil)))
(add-hook 'dashboard-mode-hook '(lambda () (setq mode-line-format nil)
                                           (doom-modeline-mode -1)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(writeroom-mode use-package projectile command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
