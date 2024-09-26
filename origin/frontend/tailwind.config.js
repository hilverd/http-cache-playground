const defaultTheme = require('tailwindcss/defaultTheme');

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
      fontFamily: {
        sans: ['Inter var', ...defaultTheme.fontFamily.sans],
      },
    },
  },
  daisyui: {
    themes: ["light"],
  }
}
