FROM ubuntu:16.04

RUN apt-get -y update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
        build-essential m4 apt-utils \
        libffi-dev libssl-dev \
        libbz2-dev \
        libgmp3-dev \
        libev-dev \
        libsnappy-dev \
        libxen-dev \
        help2man \
        pkg-config \
        time \
        aspcud \
        wget \
        curl \
        darcs \
        git \
        unzip \
        automake \
        python-dev \
        python-pip \
        debhelper \
        psmisc \
        sudo \
        libtool \
        iptables \
        net-tools \
        ncurses-dev \
        tzdata

RUN useradd jenkins -u 1500 -g root --create-home
RUN echo "jenkins ALL=NOPASSWD: ALL" >/etc/sudoers.d/jenkins

RUN pip --disable-pip-version-check install fabric junit-xml nose simplejson python-etcd

# Install etcd:
ENV etcd_version=2.3.5
RUN wget -nv --show-progress --progress=dot:mega \
    https://github.com/coreos/etcd/releases/download/v${etcd_version}/etcd-v${etcd_version}-linux-amd64.tar.gz -O - \
    | tar zxf - \
    && cp etcd-v${etcd_version}-linux-amd64/etcd /usr/bin \
    && cp etcd-v${etcd_version}-linux-amd64/etcdctl /usr/bin \
    && rm -rf etcd-*

# Install redis:
ENV redis_version=3.0.7
RUN wget -nv --show-progress --progress=dot:mega \
    http://download.redis.io/releases/redis-${redis_version}.tar.gz -O - \
    | tar zxf - \
    && make -C redis-${redis_version} \
    && cp redis-${redis_version}/src/redis-server /usr/bin \
    && cp redis-${redis_version}/src/redis-cli /usr/bin \
    && rm -rf redis-${redis_version}

# most recent one (28/11/2017) is not compatible with our setup
RUN wget -nv --show-progress --progress=dot \
	https://raw.githubusercontent.com/ocaml/opam/afa822ec24c0f7afa278179cc59796a384129ce1/shell/opam_installer.sh

env ocaml_version=4.06.0+default-unsafe-string
RUN sh ./opam_installer.sh /usr/local/bin ${ocaml_version}

ADD opam.switch opam.switch
ADD opam-repository ovs-opam-repository
RUN chown jenkins opam.switch ovs-opam-repository

USER jenkins

RUN opam init --auto-setup --comp ${ocaml_version}
RUN opam repo add ovs ovs-opam-repository && \
    opam update -v && \
    opam switch import opam.switch -y

ENTRYPOINT ["/home/arakoon/docker/docker-entrypoint.sh"]
