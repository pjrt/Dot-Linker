## DOT Linker

Dot Linker is a tool for easily linking your dotfiles to where you want to in a
declarative way.

### Installing

Close the repo and call: `stack install`

hackage/stackage/binaries coming soon

### Usage

Make a mapping file (can be named anything):

```
vimrc: $HOME/.vimrc, $HOME/.config/nvim/init.vim
zshrc: $HOME/.zshrc
```

Assuming all your dotfiles are in one place (in a git repo somewhere?) simply
call:

```bash
dot-linker /path/to/dot-files /path/to/mapping-file
```

The tool will then link every file in your mappings file to all the locations
specified. If a link already exists, it will be skipped. If a dot file exists
in your dots directory but is not mentioned in the mappings file, it will also
be skipped.
