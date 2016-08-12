#!/bin/bash

dir=~/dotfiles
files="bash/bash_profile bash/inputrc"

# do subdir-to-homedir files
for file in $files; do
    echo "Symlinking $file to home directory..."
    ln -s $dir/$file ~/.$(basename $file)
done

echo "Symlinking .vimrc to home directory..."
ln -s $dir/_vimrc ~/.vimrc

echo "Done!"
