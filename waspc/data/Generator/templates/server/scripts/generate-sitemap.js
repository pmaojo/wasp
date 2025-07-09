const fs = require('fs/promises');
const path = require('path');

/**
 * Generate sitemap XML from provided baseUrl and route paths.
 * @param {string} baseUrl - Base URL of the web client.
 * @param {string[]} routes - Array of route paths (e.g. ["/", "/about"]).
 * @returns {string} XML sitemap contents.
 */
function generateSitemap(baseUrl, routes) {
  const urls = routes.map((r) => {
    const loc = baseUrl ? `${baseUrl.replace(/\/$/, '')}${r}` : r;
    return `  <url><loc>${loc}</loc></url>`;
  });
  return [
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    ...urls,
    '</urlset>',
    ''
  ].join('\n');
}

async function main() {
  const configPath = path.join(__dirname, '..', 'publicRoutes.json');
  try {
    await fs.access(configPath);
  } catch {
    console.log('No publicRoutes.json found, skipping sitemap generation.');
    return;
  }
  const { routes = [], baseUrl = process.env.WASP_WEB_CLIENT_URL || '' } = JSON.parse(
    await fs.readFile(configPath, 'utf8')
  );
  const xml = generateSitemap(baseUrl, routes);
  const outPath = path.join(__dirname, '..', '..', 'web-app', 'public', 'sitemap.xml');
  await fs.mkdir(path.dirname(outPath), { recursive: true });
  await fs.writeFile(outPath, xml);
  console.log(`Sitemap written to ${outPath}`);
}

if (require.main === module) {
  main().catch((e) => {
    console.error('Failed to generate sitemap:', e);
    process.exit(1);
  });
}

module.exports = { generateSitemap };
