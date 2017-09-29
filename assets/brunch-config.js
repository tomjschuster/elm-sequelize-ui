exports.config = {

  files: {
    javascripts: {
      joinTo: 'js/app.js'
    },
    stylesheets: {
      joinTo: 'css/app.css'
    },
    templates: {
      joinTo: 'js/app.js'
    }
  },

  conventions: {
    assets: /^(static)/
  },

  paths: {
    watched: ['static', 'css', 'js', 'elm'],
    public: '../priv/static'
  },

  plugins: {
    babel: {
      ignore: [/vendor/, /elm.js$/]
    },

    elmBrunch: {
      mainModules: ['elm/Main.elm'],
      outputFile: 'elm.js',
      outputFolder: 'js',
      // executablePath: './node_modules/elm/binwrappers',
      makeParameters: ['--debug']
    }
  },

  modules: {
    autoRequire: {
      'js/app.js': ['js/app']
    }
  },

  npm: {
    enabled: true
  }
}
