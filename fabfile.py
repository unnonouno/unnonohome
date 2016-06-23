import fabric
from fabric.api import cd, env, put, run, sudo, shell_env


def install_pyenv():
    sudo('apt-get install -yq curl git libbz2-dev libreadline-dev libssl-dev libsqlite-dev')
    run('curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash')


def install_ubuntu():
    sudo('apt-get install -yq emacs gcc git lv mercurial texinfo tig tmux zip zsh')


def install_dl():
    sudo('apt-get install -yq gfortran libatras-dev libjpeg-dev liblapack-dev libpng-dev libopenblas')


def install_dot():
    run('mkdir git')
    with cd('git'):
        run('git clone https://github.com/unnonouno/unnonohome.git')
    run('ln -s git/unnonohome/.zshrc .')
    run('ln -s git/unnonohome/.gitconfig .')
    run('ln -s git/unnonohome/.tmux.conf .')
    run('mkdir -p .emacs.d')
    run('ln -s ../git/unnonohome/init.el .emacs.d/.')


def install_cudnn():
    cudnn = 'cudnn-7.5-linux-x64-v5.0-ga'
    cudnn_ver = 'v5'
    run('curl -s -o {cudnn}.tgz http://developer.download.nvidia.com/compute/redist/cudnn/{cudnn_ver}/{cudnn}.tgz'.format(
        cudnn=cudnn,
        cudnn_ver=cudnn_ver,
    ))
    run('tar -xzf {cudnn}.tgz'.format(cudnn=cudnn))
    run('rm {cudnn}.tgz'.format(cudnn=cudnn))


def install_all():
    install_ubuntu()
    install_pyenv()
    install_dot()
