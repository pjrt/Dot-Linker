## DOT Linker

Dot Linker is a tool for easily linking your dotfiles to where you want to in a
declarative way.

### Installing

Close the repo and call: `stack install`

hackage/stackage/binaries coming soon

### Usage

Make a mapping file:

```
vimrc: /home/pjrt/.vimrc, /home/pjrt/.config/nvim/init.vim
zshrc: /home/pjrt/.zshrc
```

Assuming all your dotfiles are in one place (in a git repo somewhere?) simply
call:

```bash
dot-linker /path/to/dot-files /path/to/mapping-file
```
