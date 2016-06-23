import fabric
from fabric.api import cd, env, put, run, sudo, shell_env


def install_pyenv():
    sudo('apt-get install -yq curl git libbz2-dev libreadline-dev libssl-dev libsqlite-dev')
    run('curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash')


def install_ubuntu():
    sudo('apt-get install -yq emacs gcc git lv mercurial texinfo tig tmux zip zsh')
