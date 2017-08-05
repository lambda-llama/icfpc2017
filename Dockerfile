FROM debian:9

RUN apt-get update
RUN apt-get install -y curl libunwind8 gettext
RUN curl -sSL -o dotnet.tar.gz https://aka.ms/dotnet-sdk-2.0.0-preview2-linux-x64-bin
RUN mkdir -p /opt/dotnet && tar zxf dotnet.tar.gz -C /opt/dotnet
RUN ln -s /opt/dotnet/dotnet /usr/local/bin
# build nuget cache
RUN cd /tmp && \
    mkdir proj && \
    cd proj && \
    dotnet new console -lang f# && \
    dotnet restore && \
    cd .. && \
    rm -rf proj

ADD . /repo
WORKDIR /repo
