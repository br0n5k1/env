local THIS_DIRECTORY=$(dirname $0)

local BIN_DIRECTORY="$THIS_DIRECTORY/bin"

if [[ ! -d $BIN_DIRECTORY ]]; then
    mkdir $BIN_DIRECTORY
fi

local SRC_DIRECTORY="$THIS_DIRECTORY/src"

if [[ ! -d $SRC_DIRECTORY ]]; then
    mkdir $SRC_DIRECTORY
fi

export PATH="$BIN_DIRECTORY/zig-latest:$BIN_DIRECTORY:$PATH"
