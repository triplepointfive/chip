FROM node:10

LABEL author=triplepointfive@gmail.com

RUN apt-get update && apt-get install -y curl apt-transport-https && \
  curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - && \
  echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
  apt-get update && apt-get install --no-install-recommends yarn

RUN userdel node
RUN useradd -m -s /bin/bash pureuser

WORKDIR /apps/pure
USER pureuser

COPY package.json .
COPY yarn.lock .

CMD yarn install
