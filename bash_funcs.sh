
function path_remove() {
    dir_to_remove=$1
    PATH=:$PATH:
    while [[ $PATH = *":$dir_to_remove:"* ]]; do
        PATH=${PATH//":$dir_to_remove:"/:}
    done
    # Trim off ":" from the beginning and end.
    PATH=${PATH#:}
    PATH=${PATH%:}
}

function path_add() {
    path_to_add=$1
    PATH=$PATH:$path_to_add
    # Trim off ":" from the beginning and end.
    PATH=${PATH#:}
    PATH=${PATH%:}
}



#
# Vivado - assuming installed in /tools/
#
function remove_vivado() {
    path_to_remove=`which vivado | sed 's/\/vivado *$//'`
    path_remove $path_to_remove
    echo "Post-remove: $PATH"
}

function set_vivado() {
    echo "Current:     $PATH"
    ver=$1
    p=/tools/Xilinx_$ver/Vivado/$ver/bin
    if [[ -e $p ]];
    then
        remove_vivado
        path_add /tools/Xilinx_$ver/Vivado/$ver/bin
        echo "Final:       $PATH"
    else
        echo "Version $ver path doesn't exist: $p"
    fi

}

function get_vivado_versions() {
    foo=`ls -1 /tools | grep Xilinx_ | sort -r | sed 's/Xilinx_//'`
    echo $foo
}
