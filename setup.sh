#!/bin/bash

dir=~/dotfiles
files="bash/bash_profile \
       bash/bashrc \
       bash/inputrc \
       git/gitignore \
       git/gitconfig \
       tmux/tmux.conf"

# do subdir-to-homedir files
for file in $files; do
    echo "Symlinking $file to home directory..."
    ln -s $dir/$file ~/.$(basename $file)
done

echo "Symlinking .vimrc to home directory..."
ln -s $dir/_vimrc ~/.vimrc

echo "Done!"
