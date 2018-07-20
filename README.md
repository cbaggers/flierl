# flierl

```
(require 'flierl)
(flierl-setup)

(add-hook 'erlang-mode-hook
          (lambda ()
            (flycheck-mode)
            (define-key erlang-mode-map (kbd "M-p") 'flycheck-previous-error)
            (define-key erlang-mode-map (kbd "M-n") 'flycheck-next-error)))

(setq flycheck-check-syntax-automatically
      '(save idle-change new-line mode-enabled))
```
