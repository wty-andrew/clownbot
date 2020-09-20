const remarkMath = require('remark-math')
const rehypeKatex = require('rehype-katex')

const baseUrl = '/clownbot/'

module.exports = {
  title: 'Clownbot',
  url: 'https://wty-andrew.github.io',
  baseUrl,
  onBrokenLinks: 'throw',
  favicon: 'img/favicon.ico',
  organizationName: 'wty-andrew',
  projectName: 'clownbot',
  themeConfig: {
    navbar: {
      title: 'Clownbot',
      hideOnScroll: true,
      items: [
        {
          to: 'docs',
          activeBasePath: 'docs',
          label: 'Docs',
          position: 'left',
        },
        {
          to: 'blog',
          label: 'Blog',
          position: 'left',
        },
        {
          href: 'https://github.com/wty-andrew/clownbot',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [],
      copyright: `Copyright © ${new Date().getFullYear()} Andrew. Built with Docusaurus.`,
    },
    prism: {
      theme: require('prism-react-renderer/themes/oceanicNext'),
      additionalLanguages: ['cmake', 'lisp'],
    },
  },
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          remarkPlugins: [remarkMath],
          rehypePlugins: [rehypeKatex],
        },
        blog: {
          showReadingTime: true,
          remarkPlugins: [remarkMath],
          rehypePlugins: [rehypeKatex],
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      },
    ],
  ],
  stylesheets: [`${baseUrl}css/katex.min.css`],
}
