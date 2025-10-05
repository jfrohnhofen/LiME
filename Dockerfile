# run-arg: $([ -c /dev/dirty-jtag ] && echo --device $(readlink -f /dev/dirty-jtag))
# run-arg: $([ -c /dev/ttyACM0 ] && echo --device /dev/ttyACM0)
# run-arg: $([ -c /dev/ttyACM1 ] && echo --device /dev/ttyACM1)
# run-arg: --device=/dev/dri:/dev/dri
# run-arg: --env="DISPLAY"
# run-arg: --volume="/tmp/.X11-unix:/tmp/.X11-unix:rw"
# run-arg: --volume="/usr/local/diamond/3.13/cae_library/simulation/verilog/ecp5u/:/cae_library"

FROM ubuntu@sha256:58b87898e82351c6cf9cf5b9f3c20257bb9e2dcf33af051e12ce532d7f94e3fe AS build

ARG DEBIAN_FRONTEND=noninteractive

WORKDIR /tmp

RUN apt update \
 && apt install -y \
        autoconf \
        bison \
        build-essential \
        clang \
        cmake \
        default-jdk \
        flex \
        gawk \
        gcc-arm-none-eabi \
        git \
        graphviz \
        help2man \
        libboost-dev \
        libboost-filesystem-dev \
        libboost-iostreams-dev \
        libboost-program-options-dev \
        libboost-python-dev \
        libboost-system-dev \
        libboost-thread-dev \
        libeigen3-dev \
        libffi-dev \
        libftdi1-2 \
        libftdi1-dev \
        libnewlib-arm-none-eabi \
        libreadline-dev \
        libstdc++-arm-none-eabi-newlib \
        pkg-config \
        python3 \
        python3-dev \
        python3-pip \
        tcl-dev \
        unzip \
        wget \
        xdot \
        zlib1g-dev

RUN pip3 install orderedmultidict

# Build yosys
RUN git clone https://github.com/YosysHQ/yosys.git \
 && cd yosys \
 && git checkout yosys-0.44 \
 && git submodule update --init \
 && make -j$(nproc) \
 && make install

# Build Project Trellis
RUN git clone https://github.com/YosysHQ/prjtrellis \
 && cd prjtrellis/libtrellis \
 && git checkout 1.4 \
 && git submodule update --init --recursive \
 && cmake . \
 && make -j$(nproc) \
 && make install

 RUN apt install -y qtcreator qtbase5-dev qt5-qmake

# Build nextpnr
RUN git clone https://github.com/YosysHQ/nextpnr \
 && cd nextpnr \
 && git checkout nextpnr-0.7 \
 && git submodule update --init --recursive \
 && cmake -DARCH=ecp5 -DBUILD_GUI=ON \
 && make -j$(nproc) \
 && make install

# Download svlint 
RUN wget https://github.com/dalance/svlint/releases/download/v0.9.3/svlint-v0.9.3-x86_64-lnx.zip \
 && unzip svlint-v0.9.3-x86_64-lnx.zip

# Build verilator
RUN git clone https://github.com/verilator/verilator \
 && cd verilator \
 && git checkout v5.026 \
 && autoconf \
 && ./configure \
 && make -j$(nproc) \
 && make install

# Build openFPGALoader
RUN git clone https://github.com/trabucayre/openFPGALoader \
 && cd openFPGALoader \
 && git checkout v0.12.1 \
 && cmake . \
 && make -j$(nproc) \
 && make install

# Build dirtyJTAG firmware for RPi Pico
RUN git clone https://github.com/raspberrypi/pico-sdk.git \
 && cd pico-sdk \
 && git checkout 1.5.1 \
 && git submodule update --init lib/tinyusb
RUN git clone https://github.com/phdussud/pico-dirtyJtag.git \
 && cd pico-dirtyJtag \
 && git checkout c39d9b7 \
 && cmake -DPICO_SDK_PATH=/tmp/pico-sdk . \
 && make -j$(nproc)

FROM ubuntu@sha256:58b87898e82351c6cf9cf5b9f3c20257bb9e2dcf33af051e12ce532d7f94e3fe

ARG USER=user
ARG DEBIAN_FRONTEND=noninteractive

# Install packages
RUN apt update \
 && apt install -y \
        build-essential \ 
        curl \
        fd-find \
        file \
        git \
        gpg \
        gtkwave \
        libboost-dev \
        libboost-filesystem-dev \
        libboost-thread-dev \
        libboost-program-options-dev \
        libboost-iostreams-dev \
        libftdi1-2 \
        libftdi1-dev \
        libqt5quick5 \
        locales \
        make \
        minicom \
        nano \
        python3-dev \
        sudo \
        wget \
        yosys \
        zsh

# Set locale
RUN locale-gen en_US.UTF-8

# Create user and add to sudoers file
RUN groupadd $USER \
 && useradd -s $(which zsh) -g $USER -G plugdev,dialout -m $USER \
 && echo $USER ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USER \
 && chmod 0440 /etc/sudoers.d/$USER

# Setup shell
RUN su $USER sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" \
 && echo "export PROMPT='%{\$fg[magenta]%}%n%{\$reset_color%}@%{\$fg[blue]%}%m %{\$fg[yellow]%}%1~ %{\$reset_color%}%# '" >> "/home/$USER/.zshrc"

# Persist command history
RUN SNIPPET="export PROMPT_COMMAND='history -a' && export HISTFILE=/history/.zsh_history" \
 && echo "$SNIPPET" >> "/home/$USER/.zshrc" \
 && mkdir /history \
 && chown $USER /history

RUN apt update && apt install -y openjdk-21-jdk scala

# Copy over FPGA toolchain and dirtyJTAG image
COPY --from=build /usr/local/bin/* /usr/local/bin
COPY --from=build /usr/local/share/yosys /usr/local/share/yosys
COPY --from=build /usr/local/share/trellis /usr/local/share/trellis
COPY --from=build /usr/local/lib/trellis /usr/local/lib/trellis
COPY --from=build /tmp/bin/svlint /usr/local/bin
COPY --from=build /usr/local/share/verilator /usr/local/share/verilator
COPY --from=build /usr/local/share/openFPGALoader /usr/local/share/openFPGALoader
COPY --from=build /tmp/pico-dirtyJtag/dirtyJtag.uf2 /home/$USER

USER $USER
ENTRYPOINT [ "/usr/bin/zsh" ]
