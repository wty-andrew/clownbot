module.exports = {
  parser: 'babel-eslint',
  extends: [
    'eslint:recommended',
    'plugin:react/recommended',
    'plugin:react-hooks/recommended',
    'prettier',
  ],
  plugins: ['react', 'prettier'],
  settings: {
    react: {
      version: 'latest',
    },
  },
  rules: {
    'prettier/prettier': [
      'error',
      {
        singleQuote: true,
        trailingComma: 'es5',
        semi: false,
      },
    ],
    'no-console': 'warn',
  },
  env: {
    browser: true,
    node: true,
    es6: true,
    jest: true,
  },
}
