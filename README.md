# auto-save-visited-local-mode

A buffer-local alternative to Emacs's built-in `auto-save-visited-mode`.

## Description

`auto-save-visited-local-mode` provides automatic saving of file-visiting buffers on a per-buffer basis, unlike the global `auto-save-visited-mode`. This allows you to enable auto-saving for specific buffers without affecting your entire Emacs session.

## Features

- **Buffer-local**: Only affects the current buffer, not all buffers
- **Feature-aligned**: Mirrors the behavior of built-in `auto-save-visited-mode`
- **Configurable interval**: Set custom idle time before auto-saving
- **Silent mode**: Optionally suppress save messages
- **Custom predicates**: Filter which buffers should be auto-saved
- **Smart defaults**: Skips remote files, read-only files, and indirect buffers

## Requirements

- Emacs 26.1 or later

## Installation

### From MELPA

```elisp
(package-install 'auto-save-visited-local-mode)
```

### Manual installation

Clone this repository and add to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/auto-save-visited-local-mode")
(require 'auto-save-visited-local-mode)
```

## Usage

### Basic usage

Enable the mode in the current buffer:

```elisp
M-x auto-save-visited-local-mode
```

Or programmatically:

```elisp
(auto-save-visited-local-mode)
```

### Configuration

#### Set custom interval

```elisp
;; Auto-save after 10 seconds of idle time (default is 5)
(setq-local auto-save-visited-local-interval 10)
```

#### Enable silent mode

```elisp
;; Suppress auto-save messages
(setq-local auto-save-visited-local-silent t)
```

#### Use custom predicate

```elisp
;; Only auto-save buffers in certain directories
(setq-local auto-save-visited-local-predicate
            (lambda ()
              (string-prefix-p "/home/user/projects/"
                               (or buffer-file-name ""))))
```

### Enable for Specific File Types

```elisp
;; Enable for all org-mode buffers
(add-hook 'org-mode-hook #'auto-save-visited-local-mode-turn-on)

;; Enable for all markdown files
(add-hook 'markdown-mode-hook #'auto-save-visited-local-mode-turn-on)
```

### Directory-local settings

Use `.dir-locals.el` to enable for all files in a directory:

```elisp
((nil . ((eval . (when (fboundp 'auto-save-visited-local-mode)
                   (auto-save-visited-local-mode-turn-on))))))
```

Or for specific modes with custom settings:

```elisp
((org-mode . ((eval . (when (fboundp 'auto-save-visited-local-mode)
                        (auto-save-visited-local-mode-turn-on)))
              (auto-save-visited-local-interval . 10)
              (auto-save-visited-local-silent . t))))
```

## Comparison with `auto-save-visited-mode`

| Feature       | `auto-save-visited-mode` | `auto-save-visited-local-mode`        |
|---------------|--------------------------|---------------------------------------|
| Scope         | Global (all buffers)     | Buffer-local                          |
| Configuration | Single global setting    | Per-buffer customization              |
| Use case      | Same behavior everywhere | Different settings per buffer/project |

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

GPL-3.0-or-later

## See Also

- `auto-save-visited-mode` - The built-in global equivalent
- `auto-save-mode` - Traditional Emacs auto-save to `#file#` backups
