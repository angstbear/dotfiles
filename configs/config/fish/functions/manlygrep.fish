function manlygrep --description 'recursive grep that deals with files with spaces in them and also doesnt bother matching binary files'
    # remember to put single quotes around the file match expression to thwart the dreaded wildcard expansion demon
    find "$argv[1]" -name "$argv[2]" -printf "\"%p\"\\n" | xargs grep -I "$argv[3]"
end
