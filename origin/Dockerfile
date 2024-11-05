FROM node:20.11.1-bookworm-slim

RUN apt-get update && \
    apt-get install -y \
    ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /usr/src/app

COPY package.json package-lock.json ./

COPY frontend/package.json frontend/package-lock.json ./frontend/

RUN npm ci && \
    cd frontend && \
    npm ci

COPY server.js ./

COPY frontend/ ./frontend/

RUN cd frontend && \
    npm run build && \
    mv dist ..

CMD ["node", "server.js"]
