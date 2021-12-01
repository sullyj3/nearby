function ncd --wraps='cd (nearby|head -n 100|fzf)' --description 'alias ncd=cd (nearby|head -n 100|fzf)'
  cd (nearby|head -n 10000|fzf) $argv; 
end
