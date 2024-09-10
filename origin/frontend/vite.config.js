import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'
const path = require('path')

export default defineConfig({
    plugins: [elmPlugin()],
    build: {
        rollupOptions: {
            input: {
                index: path.resolve(__dirname, 'index.html'),
            }
        }
    },
    server: {
        port: 3001
    }
})
