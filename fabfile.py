import fabric
from fabric.api import cd, env, put, run, sudo, shell_env


def install_pyenv():
    sudo('apt-get install -yq git libbz2-dev libreadline-dev libssql-dev libsqlite-dev')
    run('git clone https://github.com/yyuu/pyenv.git ~/.pyenv')


def install_ubuntu():
    sudo('apt-get install -yq emacs gcc git lv mercurial texinfo tig tmux zsh')
