const assert = require('assert');
const { generateSitemap } = require('./generate-sitemap');

const xml = generateSitemap('https://example.com', ['/a', '/b']);
assert(xml.includes('<loc>https://example.com/a</loc>'));
assert(xml.includes('<loc>https://example.com/b</loc>'));
console.log('generate-sitemap tests passed');
