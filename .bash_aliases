alias emd='emacs --daemon'
alias emc='emacsclient -c'
alias emt='emacsclient -t'
alias eme='emacsclient -e'
alias emk="eme '(kill-emacs)'"
alias enw='emacs -nw'
alias pgoog='ping google.com'
alias cyclewifi='networksetup -setairportpower en0 off; networksetup -setairportpower en0 on'
alias google-chrome='/usr/bin/open -a "/Applications/Google Chrome.app"'
alias be='bundle exec'
export PICNIC_AWS='ec2-54-201-246-226.us-west-2.compute.amazonaws.com'
alias picnic-aws='ssh -i ~/.ssh/picnic-aws.pem ubuntu@$PICNIC_AWS'
export PERSONAL_AWS='ec2-54-201-100-240.us-west-2.compute.amazonaws.com'
alias personal-aws='ssh -i ~/.ssh/aws-personal.pem ubuntu@$PERSONAL_AWS'
mkcd() {
    mkdir $1
    cd $1
}
