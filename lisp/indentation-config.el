;;; indentation-config.el --- Per-project indentation overrides -*- lexical-binding: t; -*-
;;; This file is git-ignored so each clone can have its own values.

(setq-default indent-tabs-mode nil
              tab-width 2)

(setq js-indent-level 2
      js2-basic-offset 2
      ruby-indent-level 2
      sgml-basic-offset 2
      c-basic-offset 2
      css-indent-offset 2
      python-indent-offset 2
      web-mode-markup-indent-offset 4
      web-mode-code-indent-offset 4
      web-mode-css-indent-offset 4
      rust-indent-offset 2)

(provide 'indentation-config)
;;; indentation-config.el ends here
