arch=$(uname -m)
os=$(uname -s)

if [[ $arch == x86_64* && $os == Linux* ]]; then
    curl -LsSf https://get.nexte.st/latest/linux | tar zxf - -C ${CARGO_HOME:-~/.cargo}/bin
elif [[ $arch == arm64* && $os == Linux* ]] ; then
    curl -LsSf https://get.nexte.st/latest/linux-arm | tar zxf - -C ${CARGO_HOME:-~/.cargo}/bin
fi

if [[ $os == Darwin* ]]; then
    curl -LsSf https://get.nexte.st/latest/mac | tar zxf - -C ${CARGO_HOME:-~/.cargo}/bin
fi
