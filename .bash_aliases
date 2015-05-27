# set emacs as default editor
export EDITOR=emacs

# emacs aliases
alias emd='emacs --daemon'
alias emc='emacsclient -c'
alias emt='emacsclient -t'
alias eme='emacsclient -e'
alias emk="eme '(kill-emacs)'"
alias enw='emacs -nw'

# Network aliases
alias pgoog='ping google.com'
alias cyclewifi='networksetup -setairportpower en0 off; networksetup -setairportpower en0 on'
alias google-chrome='/usr/bin/open -a "/Applications/Google Chrome.app"'

alias be='bundle exec'

# Picnic servers
export PICNIC_AWS='ec2-54-201-23-8.us-west-2.compute.amazonaws.com'
export DICOM_AWS='ec2-54-149-56-255.us-west-2.compute.amazonaws.com'
alias picnic-aws='ssh -i ~/.ssh/picnic-aws.pem ubuntu@$PICNIC_AWS'
alias dicom-aws='ssh -i  ~/.ssh/picnic-aws.pem ubuntu@$DICOM_AWS'
alias picnic-sftp='sftp -i ~/.ssh/picnic-aws.pem ubuntu@$PICNIC_AWS'
alias dicom-sftp='sftp -i ~/.ssh/picnic-aws.pem ubuntu@$DICOM_AWS'
export PERSONAL_AWS='ec2-54-201-100-240.us-west-2.compute.amazonaws.com'
alias personal-aws='ssh -i ~/.ssh/aws-personal.pem ubuntu@$PERSONAL_AWS'

# git helpers
alias gh="git for-each-ref --sort=-committerdate --format='%(refname:short)' refs/heads/"
alias git=hub

# export Docker variables
export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=/Users/troyastorino/.boot2docker/certs/boot2docker-vm
export DOCKER_TLS_VERIFY=1
