set -gx PATH /usr/local/bin $PATH /home/jephron/.local/bin /home/jephron/.local/node/bin /home/jephron/.miniconda3/bin 

set -gx PATH $PATH /home/jephron/.cargo/bin

#
# -- variables --
#
set fish_config_path $HOME/.config/fish/config.fish
set vim_config_path $HOME/.config/nvim/init.vim
set xmonad_config_path $HOME/.xmonad/xmonad.hs
set xmobar_config_path $HOME/.config/xmobar/xmobarrc

set -U WORKON_HOME $HOME/.miniconda3/envs

set -U EDITOR "nvim"

# convenience
alias l="ls -lh"

alias r="ranger"
alias vim="nvim"
alias v="nvim"

alias ca="conda activate"
alias todo="vim ~/todo.txt"

alias ww="cd ~/Devel/work"
alias rr="cd ~/Devel/rice/sources"
alias ff="cd ~/Devel/foo"
alias mm="cd ~/Media/Music"
alias hs="cd ~/Devel/foo/exercises/exercism/haskell"
alias wr="cd ~/Wrotes/writing/"
alias efs="cd ~/Devel/work/efs"
alias tasks="task"
alias ta="task add"
alias light="lightrs"


alias vimcurl=curlvim

function extract 
	switch (echo $argv)
	case  "*.xz"
		tar xJf $argv
	case  "*.tar.bz2 "  
		tar xvjf $argv    
	case  "*.tar.gz"    
		tar xvzf $argv    
	case  "*.bz2"       
		bunzip2 $argv     
	case  "*.rar"       
		unrar x $argv     
	case  "*.gz"        
		gunzip $argv      
	case  "*.tar"    
		tar xvf $argv     
	case  "*.tbz2"   
		tar xvjf $argv    
	case  "*.tgz"    
		tar xvzf $argv    
	case  "*.zip"    
		unzip $argv       
	case  "*.Z"      
		uncompress $argv  
	case  "*.7z"     
		7z x $argv        
	case  "*"        
		echo "Unable to extract '$argv'" 
	end
end

#
# -- plugins -- 
#

fundle plugin 'tuvistavie/fish-ssh-agent'

#
# -- aliases for opening config files -- 
# 

# config fish
alias cff="nvim $fish_config_path" 

# config vim
alias cfv="nvim $vim_config_path"

# config xmonad 
alias cfx="nvim $xmonad_config_path"

# config xmobar
alias cfm="nvim $xmobar_config_path"

function wifi_connect -d "connect to a wifi network" 
	nmcli dev wifi connect $argv
end

function wifi_disconnect -d "disconnect from a wifi network"
	nmcli con down 
end

#
# -- git --
#

# open current repo's github page
function gh -d "open the current repo's github page"
	set -l remote (git remote show origin)
	set -l url (echo $remote | cut -d '@' -f 2 | sed -e 's/:/\//' -e 's/\.git.*//')
	xdg-open https://$url
end
	

#
# -- misc -- 
#

# reload fish
alias reloaf="source $fish_config_path"

# edit a file with the current date as the name
alias vimdate='vim (date|sed "s/ /_/g")'

# emulate !! and !$ from bash  (https://superuser.com/a/944589/347488)
function bind_bang
    switch (commandline -t)[-1]
        case "!"
            commandline -t $history[1]; commandline -f repaint
        case "*"
            commandline -i !
    end
end

function bind_dollar
    switch (commandline -t)[-1]
        case "!"
            commandline -t ""
            commandline -f history-token-search-backward
        case "*"
            commandline -i '$'
    end
end

function fish_user_key_bindings
    bind ! bind_bang
    bind '$' bind_dollar
end

# OPAM configuration
# seems to break manpath
#. /home/jephron/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true


# Start X at login
if status is-login
    if test -z "$DISPLAY" -a $XDG_VTNR = 1
        exec startx -- -keeptty
    end
end
