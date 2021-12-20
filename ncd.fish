# todo: don't cd if nothing is selected

function ncd --wraps='cd (nearby|head -n 100|fzf)' --description 'alias ncd=cd (nearby|head -n 100|fzf)'
  if count $argv &> /dev/null;
    set TARGET (nearby|head -n 10000|fzf -q $argv);
  else;
    set TARGET (nearby|head -n 10000|fzf);
  end

  if count $TARGET &> /dev/null;
    cd $TARGET
  else;
    echo "No target selected!"
  end
end
