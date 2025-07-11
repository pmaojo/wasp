---
title: SSR Support
---

Wasp apps render client-side by default. To enable server-side rendering during
code generation, use the `--ssr` flag with `wasp start` or `wasp build`.

```bash
wasp start --ssr
```

The generator creates additional entry points under `src/entry-client.tsx` and
`src/entry-server.tsx` which integrate with Vite's SSR mode. Deployment follows
the usual steps after running:

```bash
wasp build --ssr
```

Switch back to CSR by omitting the flag.
