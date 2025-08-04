*Oberon-07 compiler for Linux x64*

# Build

Debian:
```bash
sudo apt update && sudo apt install -y git make
cd /opt && sudo git clone https://github.com/osipenco/c07 && cd c07
sudo make
export PATH="$PATH:/opt/c07/bin/"
```

Arch:
```bash
sudo pacman -Syu git make
cd /opt && sudo git clone https://github.com/osipenco/c07 && cd c07
sudo make
export PATH="$PATH:/opt/c07/bin/"
```

# Using

[README.rst](doc/README.rst)
