FROM node:20.16.0

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

CMD ["npm", "start"]
