module.exports = {
  content: [
    './index.{css,html,js}',
    './src/**/*.elm'
  ],
  plugins: [
    require('daisyui'),
  ],
  darkMode: 'class',
  theme: {
    extend: {
      screens: {
        '3xl': '1800px',
        'print': { 'raw': 'print' },
      },
    },
  },
  daisyui: {
    themes: ["light"],
  }
}
