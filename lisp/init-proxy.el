
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "127.0.0.1:1080")
        ("https" . "127.0.0.1:1080")))

(provide 'init-proxy)